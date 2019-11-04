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
    State.modify(_.scope.push)

  private def enterScope(isolate: Boolean): State[Context, Unit] =
    State.modify(_.scope.push(isolate))

  private def exitScope: State[Context, Unit] =
    State.modify(_.scope.pop)

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
                   elseCase
                     .map(_.eval)
                     .getOrElse(State.empty[Context, String])
                 } else {
                   value.zipWithIndex.toList
                     .map {
                       case (subValues, i) =>
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
                           identifiers
                             .map(_.value)
                             .zip(subValues.destructure) :+ ("loop" -> loop)

                         State.modify[Context](_.scope.setAll(parameters, resolveUp = false)) >> partial.eval
                     }
                     .sequence
                     .map(Monoid.combineAll(_))
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
          m.flatMap(_ => State.modify(_.scope.set(n.value, Value.Str(content), resolveUp = true)))
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

    private val defaultArguments = args.toList.flatMap {
      case (key, value) =>
        value.map(_.eval.map(key.value -> _))
    }.sequence

    private def withoutContext(arguments: List[(String, Value)]): State[Context, String] =
      State.inspect[Context, String] { context =>
        val c = context.empty
            .path.set(context.path.get)
            .scope.setAll(arguments, resolveUp = false)
        content.eval.runA(c).value
      }

    private def withContext(arguments: List[(String, Value)]): State[Context, String] = for {
      oldPosition <- State.inspect[Context, Int](_.scope.get.position)
      _           <- State.modify[Context](_.scope.newRoot)
      _           <- State.modify[Context](_.scope.setAll(arguments, resolveUp = false))
      result      <- content.eval
      _           <- State.modify[Context](_.scope.goTo(oldPosition).get)
    } yield result

    override def eval: State[Context, String] = {

      for {
        definingPath           <- State.inspect[Context, Option[String]](_.path.get)
        defaultArguments       <- defaultArguments
        importedWithoutContext <- State.inspect[Context, Boolean](_.renderMode.get == RenderMode.Import(false))
        fn = Value.Function { parameters =>
          val byNameParameters = parameters.parameters.toList
            .mapFilter(param => param.name.map(_ -> param.value))
            .toMap

          val positionalParameters = args.keys
            .map(_.value)
            .filterNot(byNameParameters.isDefinedAt)
            .zip(parameters.parameters.toList.mapFilter(param => if (param.name.isEmpty) Some(param.value) else None))

          def completeParameters(caller: Value) =
            (defaultArguments ++ byNameParameters ++ positionalParameters ++ List("caller" -> caller))

          for {
            callerPath  <- State.inspect[Context, Option[String]](_.path.get)
            _           <- State.modify[Context](_.path.set(definingPath))
            caller      <- State.inspect[Context, Value](_.scope.get("caller"))
            result      <- if (importedWithoutContext) {
              withoutContext(completeParameters(caller))
            } else {
              withContext(completeParameters(caller))
            }
            _           <- State.modify[Context](_.path.set(callerPath))
          } yield Value.Str(result, safe = true)
        }
        _ <- State.modify[Context](_.defineMacro(identifier.value, fn))
      } yield ""
    }
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
            _      <- State.modify[Context](_.scope.setAll(completeParameters, resolveUp = false))
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
        .runA(context.scope.set("caller", body, resolveUp = false))
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

    private def importWithContext(resolvedTemplate: Loader.ResolvedTemplate): State[Context, Unit] =
      for {
        oldPath       <- State.inspect[Context, Option[String]](_.path.get)
        oldRenderMode <- State.inspect[Context, RenderMode](_.renderMode.get)
        _             <- State.modify[Context](_.path.set(Some(resolvedTemplate.path)))
        _             <- State.modify[Context](_.renderMode.set(RenderMode.Import(true)))
        _             <- resolvedTemplate.template.render
        exports       <- State.inspect[Context, Value](context => Value.Obj(context.exports.get))
        _             <- State.modify[Context](_.scope.set(identifier.value, exports, resolveUp = false))
        _             <- State.modify[Context](_.path.set(oldPath))
        _             <- State.modify[Context](_.renderMode.set(oldRenderMode))
      } yield ()

    private def importWithoutContext(resolvedTemplate: Loader.ResolvedTemplate): State[Context, Unit] =
      for {
        oldContext <- State.get[Context]
        _          <- State.modify[Context](_.empty(RenderMode.Import(false)))
        _          <- State.modify[Context](_.path.set(Some(resolvedTemplate.path)))
        _          <- resolvedTemplate.template.render
        exports    <- State.inspect[Context, Value](context => Value.Obj(context.exports.get))
        _          <- State.set[Context](oldContext)
        _          <- State.modify[Context](_.scope.set(identifier.value, exports, resolveUp = false))
      } yield ()

    override def eval: State[Context, String] =
      for {
        value <- expr.eval.map(_.toStr.value)
        resolvedTemplate <- State.inspect[Context, Loader.ResolvedTemplate] { context =>
                             context.environment
                               .resolveAndLoad(value, context.path.get)
                               .leftMap { paths =>
                                 throw new RuntimeException(
                                   s"missing template `$value`, attempted paths: ${paths.mkString(", ")}")
                               }
                               .merge
                           }
        _ <- if (withContext) importWithContext(resolvedTemplate) else importWithoutContext(resolvedTemplate)
      } yield ""
  }

  final case class From(expr: expression.syntax.AST.Expr,
                        identifiers: Seq[(expression.syntax.AST.Identifier, Option[expression.syntax.AST.Identifier])],
                        withContext: Boolean)
      extends Tag {

    private def importWithContext(resolvedTemplate: Loader.ResolvedTemplate): State[Context, Unit] =
      for {
        oldPath       <- State.inspect[Context, Option[String]](_.path.get)
        oldRenderMode <- State.inspect[Context, RenderMode](_.renderMode.get)
        _             <- State.modify[Context](_.path.set(Some(resolvedTemplate.path)))
        _             <- State.modify[Context](_.renderMode.set(RenderMode.Import(true)))
        _             <- resolvedTemplate.template.render
        exports <- State.inspect[Context, Seq[(String, Value)]] { context =>
                    identifiers.map {
                      case (key, preferred) =>
                        preferred.getOrElse(key).value -> {
                          val value = context.getContextValue(key.value)
                          if (value.isDefined) {
                            value
                          } else {
                            throw new RuntimeException(s"`$key` not defined in `${resolvedTemplate.path}`")
                          }
                        }
                    }
                  }
        _ <- State.modify[Context](_.scope.setAll(exports, resolveUp = false))
        _ <- State.modify[Context](_.path.set(oldPath))
        _ <- State.modify[Context](_.renderMode.set(oldRenderMode))
      } yield ()

    private def importWithoutContext(resolvedTemplate: Loader.ResolvedTemplate): State[Context, Unit] =
      for {
        oldContext <- State.get[Context]
        _          <- State.modify[Context](_.empty(RenderMode.Import(false)))
        _          <- State.modify[Context](_.path.set(Some(resolvedTemplate.path)))
        _          <- resolvedTemplate.template.render
        exports <- State.inspect[Context, Seq[(String, Value)]] { context =>
                    identifiers.map {
                      case (key, preferred) =>
                        preferred.getOrElse(key).value -> {
                          val value = context.getContextValue(key.value)
                          if (value.isDefined) {
                            value
                          } else {
                            throw new RuntimeException(s"`$key` not defined in `${resolvedTemplate.path}`")
                          }
                        }
                    }
                  }
        _ <- State.set[Context](oldContext)
        _ <- State.modify[Context](_.scope.setAll(exports, resolveUp = false))
      } yield ()

    override def eval: State[Context, String] =
      for {
        value <- expr.eval.map(_.toStr.value)
        resolvedTemplate <- State.inspect[Context, Loader.ResolvedTemplate] { context =>
                             context.environment
                               .resolveAndLoad(value, context.path.get)
                               .leftMap { paths =>
                                 throw new RuntimeException(
                                   s"missing template `$value`, attempted paths: ${paths.mkString(", ")}")
                               }
                               .merge
                           }
        _ <- if (withContext) importWithContext(resolvedTemplate) else importWithoutContext(resolvedTemplate)
      } yield ""
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
          _      <- enterScope(isolate = true) >> enterBlock
          blocks <- getBlocks(identifier.value)
          result <- blocks.foldLeft[State[Context, Value]](State.pure(Value.Undefined)) { (result, partial) =>
                     for {
                       _ <- State.modify[Context] { context =>
                             val superFn = Value.Function { _ =>
                               for {
                                 oldPosition <- State.inspect[Context, Int](_.scope.get.position)
                                 _           <- State.modify[Context](_.scope.newRoot(isolate = true))
                                 result      <- result
                                 _           <- State.modify[Context](_.scope.goTo(oldPosition).get)
                               } yield
                                 if (result.isDefined) {
                                   result
                                 } else {
                                   throw new RuntimeException(s"no super block available for '${identifier.value}")
                                 }
                             }
                             context.scope.set("super", superFn, resolveUp = false)
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
                     .map(_.apply(context.scope.get, expression.runtime.Value.Str(content), parameters))
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
