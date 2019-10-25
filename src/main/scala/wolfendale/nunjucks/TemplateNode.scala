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


  final case class Switch(exprToMatch:expression.syntax.AST.Expr, matchConditions: Seq[Switch.ConditionalContent], default: Option[Partial]) extends Tag {

    require(default.isDefined || matchConditions.nonEmpty)

    override def eval: State[Context, String] = {

      val all: Seq[Switch.ConditionalContent] = default
        .map(matchConditions :+ Switch.ConditionalContent(Seq(exprToMatch), _))
        .getOrElse(matchConditions)

      val collapsedConditionals = all.foldRight(Seq.empty[Switch.ConditionalContent]){
        (m, n) => if(m.content.isEmpty && n.nonEmpty){
          val last = n.head
          Switch.ConditionalContent(last.condition ++ m.condition, last.content) +: n.tail
        }else {
          m +: n
        }
      }

      collapsedConditionals.foldLeft(State.pure[Context, Option[String]](None)) { (m, n) =>
        m.flatMap {
          case None =>
            Monad[State[Context, *]].ifM(State.inspect(context => n.condition.exists(_.eval(context).equals(exprToMatch.eval(context)))))(
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
              m.flatMap(_ => State.modify(_.set(n.value, value, resolveUp = true)))
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
        .modify[Context] { context =>
          val body = Value.Function { parameters =>
            State[Context, Value] { callingContext =>
              val defaultArguments = args.flatMap {
                case (id, value) =>
                  value.map(id.value -> _.eval.runA(context).value)
              }

              val byNameParameters = parameters.parameters.toList
                .mapFilter(param => param.name.map(_ -> param.value))
                .toMap

              val positionalParameters = args.keys
                .map(_.value)
                .filterNot(byNameParameters.isDefinedAt)
                .zip(parameters.parameters.toList.mapFilter(param =>
                  if (param.name.isEmpty) Some(param.value) else None))

              val completeParameters =
                (defaultArguments ++ byNameParameters ++ positionalParameters).toList

              val result = for {
                c      <- State.get[Context]
                _      <- State.modify[Context](_.frame.empty.frame.set(completeParameters, resolveUp = false))
                _      <- State.modify[Context](_.frame.set("caller", c.frame.get("caller"), resolveUp = false))
                result <- content.eval
                _      <- State.modify[Context](_.frame.set(c.frame.get))
              } yield Value.Str(result, safe = true)

              result.run(callingContext).value
            }
          }

          context.set(identifier.value, body, resolveUp = false)
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
              val scope = resolvedTemplate.template.render
                .runS {
                  context.path
                    .set(Some(resolvedTemplate.path))
                    .frame
                    .set(if (withContext) context.frame.get else Frame.empty)
                }
                .value
                .frame
                .get
                .value
              context.frame.set(identifier.value, scope, resolveUp = false)
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
              val scope = resolvedTemplate.template.render
                .runS {
                  context.path
                    .set(Some(resolvedTemplate.path))
                    .frame
                    .set(if (withContext) context.frame.get else Frame.empty)
                }
                .value
                .frame
                .get
                .value
              val values = identifiers.map {
                case (key, preferred) =>
                  preferred.getOrElse(key).value -> scope.get(key.value)
              }
              context.frame.set(values, resolveUp = false)
            }
            .leftMap { paths =>
              throw new RuntimeException(s"missing template `$partial`, attempted paths: ${paths.mkString(", ")}")
            }
            .merge
        }
        .map(_ => "")
  }

  final case class Block(identifier: expression.syntax.AST.Identifier, partial: Partial) extends Tag {

    override def eval: State[Context, String] = State { context =>
      val rendered = (partial +: context.blocks.get(identifier.value))
        .foldLeft[Value](Value.Null) { (result, partial) =>
          val superFn = Value.Function { _ =>
            State.pure[Context, Value](result)
          }

          Value.Str(partial.eval.runA(context.frame.set("super", superFn, resolveUp = false)).value, safe = true)
        }
        .toStr
        .value

      val newContext = context.blocks.push(identifier.value, partial)

      (newContext, rendered)
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

                   context.filters
                     .get(identifier.value)
                     .map(_.apply(context.frame.get, expression.runtime.Value.Str(content), parameters))
                     .getOrElse(throw new RuntimeException(s"No filter with name: ${identifier.value}"))
                     .toStr
                     .value
                 }
      } yield result
  }

  final case class Extends(expr: expression.syntax.AST.Expr)
}

sealed abstract class Template {

  def render: State[Context, String]
}

final case class RootTemplate(partial: Option[Partial]) extends Template {

  override def render: State[Context, String] =
    partial.map(_.eval).getOrElse(State.pure(""))
}

final case class ChildTemplate(parent: expression.syntax.AST.Expr, partial: Option[Partial]) extends Template {

  override def render: State[Context, String] = State.inspect[Context, String] { context =>
    val parentTemplate = parent.eval.runA(context).value.toStr.value
    val newContext =
      partial.map(_.eval.runS(context).value).getOrElse(context)
    context.environment
      .resolveAndLoad(parentTemplate, context.path.get)
      .map { resolvedTemplate =>
        resolvedTemplate.template.render.runA(newContext.path.set(Some(resolvedTemplate.path))).value
      }
      .leftMap { paths =>
        throw new RuntimeException(s"missing template `$parentTemplate`, attempted paths: ${paths.mkString(", ")}")
      }
      .merge
  }
}

final case class ComplexTemplate(rootTemplate: RootTemplate, childTemplate: ChildTemplate) extends Template {

  override def render: State[Context, String] =
    for {
      root  <- rootTemplate.render
      child <- childTemplate.render
    } yield root + child
}
