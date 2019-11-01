package wolfendale.nunjucks

import cats._
import cats.data._
import cats.implicits._
import wolfendale.nunjucks.TemplateNode.Partial
import wolfendale.nunjucks.expression.runtime.Value
import wolfendale.nunjucks.expression.syntax.AST

// TODO: allow for custom Tags

sealed abstract class TemplateNode {

  def eval: State[Context, String]
}

object TemplateNode {

  private def enterScope: State[Context, Unit] =
    State.modify(_.frame.push)

  private def exitScope: State[Context, Unit] =
    State.modify(_.frame.pop)

  final case class Partial(contents: Seq[TemplateNode]) extends TemplateNode {

    def isEmpty: Boolean = contents.isEmpty

    override def eval: State[Context, String] =
      contents.foldLeft(State.pure[Context, String]("")) { (m, n) =>
        for {
          l <- m
          r <- n.eval
        } yield l + r
      }
  }

  final case class Literal(content: String) extends TemplateNode {

    override def eval: State[Context, String] = {
      State.pure(content)
    }
  }

  final case class Comment(content: String) extends TemplateNode {

    override def eval: State[Context, String] =
      State.empty
  }

  final case class Expression(expr: expression.syntax.AST) extends TemplateNode {

    override def eval: State[Context, String] =
      expr.eval.map(output)

    private def output(value: Value): String = value match {
      // Nunjucks runtime surpresses 'undefined' or 'null' values from being output:
      // https://github.com/mozilla/nunjucks/blob/1485a44297f1fef3dfd5db0d8e7e047ed1709822/nunjucks/src/runtime.js#L209-L217
      case Value.Undefined | Value.Null =>
        ""
      case Value.Arr(values) =>
        values.map(output).mkString(",")
      case result =>
        val output = result.toStr
        if (output.safe) output.value else output.escaped.value
    }
  }

  sealed abstract class Tag extends TemplateNode

  final case class If(contents: Seq[If.ConditionalContent], other: Option[Partial]) extends Tag {

    override def eval: State[Context, String] = {

      val all = other
        .map(contents :+ If.ConditionalContent(expression.syntax.AST.True, _))
        .getOrElse(contents)

      all.foldLeft(State.pure[Context, Option[String]](None)) { (m, n) =>
        m.flatMap {
          case None =>
            Monad[State[Context, *]].ifM(n.condition.eval.map(_.toBool))(ifTrue = n.content.eval.map(_.some),
                                                                         ifFalse = State.pure(None))
          case some => State.pure(some)
        }
      }
    }.map(_.getOrElse(""))
  }

  object If {

    final case class ConditionalContent(condition: expression.syntax.AST, content: Partial)
  }

  final case class Switch(exprToMatch: expression.syntax.AST.Expr,
                          matchConditions: Seq[Switch.ConditionalContent],
                          default: Option[Partial])
      extends Tag {

    require(default.isDefined || matchConditions.nonEmpty)

    override def eval: State[Context, String] = {

      val all: Seq[Switch.ConditionalContent] = default
        .map(matchConditions :+ Switch.ConditionalContent(Seq(exprToMatch), _))
        .getOrElse(matchConditions)

      val collapsedConditionals = all.foldRight(Seq.empty[Switch.ConditionalContent]) { (m, n) =>
        if (m.content.isEmpty && n.nonEmpty) {
          val last = n.head
          Switch.ConditionalContent(last.condition ++ m.condition, last.content) +: n.tail
        } else {
          m +: n
        }
      }

      collapsedConditionals.foldLeft(State.pure[Context, Option[String]](None)) { (m, n) =>
        m.flatMap {
          case None =>
            Monad[State[Context, *]].ifM(State.inspect(context =>
              n.condition.exists(_.eval.runA(context).value.equals(exprToMatch.eval.runA(context).value))))(
              ifTrue = n.content.eval.map(_.some),
              ifFalse = State.pure(None))
          case some => State.pure(some)
        }
      }
    }.map(_.getOrElse(""))
  }

  object Switch {
    final case class ConditionalContent(condition: Seq[expression.syntax.AST], content: Partial)
  }

  final case class For(identifiers: Seq[expression.syntax.AST.Identifier],
                       expr: expression.syntax.AST.Expr,
                       partial: Partial,
                       elseCase: Option[Partial])
      extends Tag {

    private lazy val runFor: State[Context, String] = {
      for {
        value <- expr.eval.map(_.toArr.values)
        result <- if (value.isEmpty) {
                   elseCase.fold(State.pure[Context, String](""))(_.eval)
                 } else {
                   State[Context, String] { context =>
                     value.zipWithIndex.foldLeft((context, "")) {
                       case ((c, m), (subValues, i)) =>
                         val loop = Value.Obj(
                           "index"     -> Value.Number(i + 1),
                           "index0"    -> Value.Number(i),
                           "revindex"  -> Value.Number(value.length - i),
                           "revindex0" -> Value.Number(value.length - i - 1),
                           "first"     -> Value.Bool(i == 0),
                           "last"      -> Value.Bool(i == value.length - 1),
                           "length"    -> Value.Number(value.length)
                         )

                         val parameters =
                           identifiers.map(_.value).zip(subValues.destructure)

                         val scope = c.frame.set(parameters :+ ("loop" -> loop), resolveUp = false)

                         partial.eval.run(scope).value.map(m + _)
                     }
                   }
                 }
      } yield result
    }

    override def eval: State[Context, String] =
      for {
        _      <- enterScope
        output <- runFor
        _      <- exitScope
      } yield output
  }

  final case class Set(names: NonEmptyList[expression.syntax.AST.Identifier], expr: expression.syntax.AST) extends Tag {

    override def eval: State[Context, String] =
      for {
        value <- expr.eval
        _ <- names.foldLeft(State.pure[Context, Unit](())) { (m, n) =>
              m.flatMap { _ =>
                State.modify { context =>
                  context.setFrameAndVariable(n.value, value, resolveUp = true)
                }
              }
            }
      } yield ""
  }

  final case class SetBlock(names: NonEmptyList[expression.syntax.AST.Identifier], partial: Partial) extends Tag {

    override def eval: State[Context, String] = partial.eval.flatMap { content =>
      names
        .foldLeft(State.pure[Context, Unit](())) { (m, n) =>
          m.flatMap(_ => State.modify(_.frame.set(n.value, Value.Str(content), resolveUp = true)))
        }
        .map(_ => "")
    }
  }

  final case class Verbatim(content: String) extends Tag {

    override def eval: State[Context, String] =
      State.pure(content)
  }

  final case class Macro(identifier: expression.syntax.AST.Identifier,
                         args: Map[expression.syntax.AST.Identifier, Option[expression.syntax.AST.Expr]],
                         content: Partial)
      extends Tag {

    override def eval: State[Context, String] =
      State
        .modify[Context] { definingContext =>
          lazy val body: Value.Function = Value.Function { parameters =>
            State[Context, Value] { callingContext =>
              val defaultArguments = args.flatMap {
                case (id, value) =>
                  value.map(id.value -> _.eval.runA(definingContext).value)
              }

              val byNameParameters = parameters.parameters.toList
                .mapFilter(param => param.name.map(_ -> param.value))
                .toMap

              val positionalParameters = args.keys
                .map(_.value)
                .filterNot(byNameParameters.isDefinedAt)
                .zip(parameters.parameters.toList.mapFilter(param =>
                  if (param.name.isEmpty) Some(param.value) else None))

              val caller = Map(
                "caller" -> callingContext.frame.get("caller")
              )

              val completeParameters =
                (defaultArguments ++ byNameParameters ++ positionalParameters ++ caller).toList

              val definingContextWithCallingVariables =
                if (definingContext.renderMode.get.withContext) {
                  definingContext.variables.set(callingContext.variables.getAll.toSeq)
                } else {
                  definingContext
                    .setFrameAndVariable(identifier.value, body, resolveUp = false)
                }

              val executingContext =
                definingContextWithCallingVariables.frame.set(completeParameters, resolveUp = false)

              val (executedContext, result) = content.eval.run(executingContext).value

              val addedVariables =
                executedContext.variables.getAll.toSet diff definingContextWithCallingVariables.variables.getAll.toSet

              val resultContext =
                if (definingContext.renderMode.get.withContext) {
                  callingContext.variables
                    .set(callingContext.variables.getAll.toSeq ++ addedVariables)
                } else {
                  callingContext
                }

              (resultContext, Value.Str(result, safe = true))
            }
          }

          if (definingContext.inBlock.get && definingContext.frame.get.pop.isRoot) {
            definingContext.variables.set(identifier.value, body)
          } else {
            definingContext.setFrameAndVariable(identifier.value, body, resolveUp = false)
          }
        }
        .map(_ => "")
  }

  final case class Call(parameters: Map[expression.syntax.AST.Identifier, Option[expression.syntax.AST.Expr]],
                        expr: expression.syntax.AST.Expr,
                        arguments: Seq[(Option[expression.syntax.AST.Identifier], expression.syntax.AST.Expr)],
                        partial: Partial)
      extends Tag {

    override def eval: State[Context, String] = State.inspect[Context, String] { context =>
      val body = Value.Function { callerParameters =>
        State[Context, Value] { callingContext =>
          val defaultArguments = parameters.flatMap {
            case (id, value) =>
              value.map(id.value -> _.eval.runA(context).value)
          }

          val byNameParameters = callerParameters.parameters.toList
            .mapFilter(param => param.name.map(_ -> param.value))
            .toMap

          val positionalParameters = parameters.keys
            .map(_.value)
            .filterNot(byNameParameters.isDefinedAt)
            .zip(callerParameters.parameters.toList.mapFilter(param =>
              if (param.name.isEmpty) Some(param.value) else None))

          val completeParameters =
            (defaultArguments ++ byNameParameters ++ positionalParameters).toList

          val result = for {
            _      <- State.modify[Context](_.frame.set(completeParameters, resolveUp = false))
            result <- partial.eval
          } yield Value.Str(result, safe = true)

          (callingContext, result.runA(context).value)
        }
      }

      val resolvedParameters = Value.Function.Parameters(arguments.map {
        case (k, v) =>
          Value.Function.Parameter(k.map(_.value), v.eval.runA(context).value)
      })

      expr.eval
        .runA(context)
        .value(resolvedParameters)
        .runA(context.frame.set("caller", body, resolveUp = false))
        .value
        .toStr
        .value
    }
  }

  final case class Include(expr: expression.syntax.AST.Expr, ignoreMissing: Boolean) extends Tag {

    override def eval: State[Context, String] = State.inspect[Context, String] { context =>
      val partial = expr.eval.runA(context).value.toStr.value
      context.environment
        .resolveAndLoad(partial, context.path.get)
        .map { resolvedTemplate =>
          resolvedTemplate.template.render.runA(context.path.set(Some(resolvedTemplate.path))).value
        }
        .leftMap { paths =>
          if (ignoreMissing) ""
          else throw new RuntimeException(s"missing template `$partial`, attempted paths: ${paths.mkString(", ")}")
        }
        .merge
    }
  }

  final case class Import(expr: expression.syntax.AST.Expr,
                          identifier: expression.syntax.AST.Identifier,
                          withContext: Boolean)
      extends Tag {

    override def eval: State[Context, String] =
      State
        .modify[Context] { context =>
          val partial = expr.eval.runA(context).value.toStr.value
          context.environment
            .resolveAndLoad(partial, context.path.get)
            .map { resolvedTemplate =>
              if (withContext) {

                val updatedContext = resolvedTemplate.template.render.runS {
                  context.path.set(Some(resolvedTemplate.path)).renderMode.set(RenderMode.Import(withContext = true))
                }.value

                updatedContext.frame.set(identifier.value, Value.Obj(updatedContext.exports.get), resolveUp = false)
              } else {

                val importedContext = resolvedTemplate.template.render.runS {
                  context.empty.path
                    .set(Some(resolvedTemplate.path))
                    .renderMode
                    .set(RenderMode.Import(withContext = false))
                }.value

                context.frame.set(identifier.value, Value.Obj(importedContext.exports.get), resolveUp = false)
              }
            }
            .leftMap { paths =>
              throw new RuntimeException(s"missing template `$partial`, attempted paths: ${paths.mkString(", ")}")
            }
            .merge
        }
        .map(_ => "")
  }

  final case class From(expr: expression.syntax.AST.Expr,
                        identifiers: Seq[(expression.syntax.AST.Identifier, Option[expression.syntax.AST.Identifier])],
                        withContext: Boolean)
      extends Tag {

    override def eval: State[Context, String] =
      State
        .modify[Context] { context =>
          val partial = expr.eval.runA(context).value.toStr.value
          context.environment
            .resolveAndLoad(partial, context.path.get)
            .map { resolvedTemplate =>
              if (withContext) {

                val updatedContext = resolvedTemplate.template.render.runS {
                  context.path.set(Some(resolvedTemplate.path)).renderMode.set(RenderMode.Import(withContext = true))
                }.value

                val values = identifiers.map {
                  case (key, preferred) =>
                    preferred.getOrElse(key).value -> {
                      val value = updatedContext.getContextValue(key.value)
                      if (value.isDefined) {
                        value
                      } else {
                        throw new RuntimeException(s"`$key` not defined in `${resolvedTemplate.path}`")
                      }
                    }
                }

                updatedContext.frame.set(values, resolveUp = false)
              } else {

                val importedContext = resolvedTemplate.template.render.runS {
                  context.empty.path
                    .set(Some(resolvedTemplate.path))
                    .renderMode
                    .set(RenderMode.Import(withContext = false))
                }.value

                val values = identifiers.map {
                  case (key, preferred) =>
                    preferred.getOrElse(key).value -> {
                      val value = importedContext.getContextValue(key.value)
                      if (value.isDefined) {
                        value
                      } else {
                        throw new RuntimeException(s"`$key` not defined in `${resolvedTemplate.path}`")
                      }
                    }
                }

                context.frame.set(values, resolveUp = false)
              }
            }
            .leftMap { paths =>
              throw new RuntimeException(s"missing template `$partial`, attempted paths: ${paths.mkString(", ")}")
            }
            .merge
        }
        .map(_ => "")
  }

  final case class Block(identifier: expression.syntax.AST.Identifier, partial: Partial) extends Tag {

    private def getBlocks(key: String): State[Context, Vector[Partial]] =
      State.inspect[Context, Vector[Partial]](_.blocks.get(key))

    private def pushBlock(key: String, block: Partial): State[Context, Unit] =
      State.modify[Context](_.blocks.push(key, block))

    private def isChild: State[Context, Boolean] =
      State.inspect[Context, Boolean](_.parent.get.isDefined)

    private def enterBlock: State[Context, Unit] =
      State.modify[Context](_.inBlock.set(true))

    private def exitBlock: State[Context, Unit] =
      State.modify[Context](_.inBlock.set(false))

    override def eval: State[Context, String] = {
      pushBlock(identifier.value, partial) >> Monad[State[Context, *]].ifM(isChild)(
        ifTrue = State.empty[Context, String],
        ifFalse = for {
          _      <- enterScope >> enterBlock
          blocks <- getBlocks(identifier.value)
          result <- blocks.foldLeft[State[Context, Value]](State.pure(Value.Undefined)) {
                     (result, partial) =>
                       for {
                         _ <- State.modify[Context] {
                               context =>
                                 val superFn = Value.Function {
                                   _ =>
                                     for {
                                       _      <- enterScope
                                       result <- result
                                       _      <- exitScope
                                     } yield
                                       if (result.isDefined) {
                                         result
                                       } else {
                                         throw new RuntimeException(
                                           s"no super block available for '${identifier.value}")
                                       }
                                 }
                                 context.frame.set("super", superFn, resolveUp = false)
                             }
                         result <- partial.eval
                       } yield Value.Str(result, safe = true)
                   }
          _ <- exitBlock >> exitScope
        } yield result.toStr.value
      )
    }
  }

  final case class Filter(identifier: expression.syntax.AST.Identifier,
                          args: Seq[(Option[expression.syntax.AST.Identifier], expression.syntax.AST.Expr)],
                          partial: Partial)
      extends Tag {

    override def eval: State[Context, String] =
      for {
        content <- partial.eval
        result <- State.inspect { context: Context =>
                   val parameters = Value.Function.Parameters(args.map {
                     case (k, v) =>
                       Value.Function.Parameter(k.map(_.value), v.eval.runA(context).value)
                   })

                   context.environment
                     .getFilter(identifier.value)
                     .map(_.apply(context.frame.get, expression.runtime.Value.Str(content), parameters))
                     .getOrElse(throw new RuntimeException(s"No filter with name: ${identifier.value}"))
                     .toStr
                     .value
                 }
      } yield result
  }

  final case class Extends(expr: expression.syntax.AST.Expr) extends Tag {

    override def eval: State[Context, String] = State { context =>
      val parentName = expr.eval.runA(context).value.toStr.value
      val parent = context.environment
        .resolveAndLoad(parentName, context.path.get)
        .leftMap { paths =>
          throw new RuntimeException(s"missing template `$parentName`, attempted paths: ${paths.mkString(", ")}")
        }
        .merge

      (context.parent.set(parent), "")
    }
  }
}

final case class Template(nodes: Partial) {

  def render: State[Context, String] = {

    for {
      rootResult <- nodes.eval
      c          <- State.get[Context]
      result <- c.parent.get match {
                 case Some(parent) =>
                   for {
                     _ <- State.modify[Context] {
                           _.parent.empty.path.set(Some(parent.path))
                         }
                     result <- parent.template.render
                   } yield result
                 case None =>
                   State.pure[Context, String](rootResult)
               }
    } yield result
  }
}
