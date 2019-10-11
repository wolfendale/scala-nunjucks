package wolfendale.nunjucks

import cats._
import cats.data._
import cats.implicits._
import wolfendale.nunjucks.TemplateNode.Partial
import wolfendale.nunjucks.expression.runtime.Value

// TODO: allow for custom Tags

sealed abstract class TemplateNode {

  def eval: State[Context, String]
}

object TemplateNode {

  final case class Partial(contents: Seq[TemplateNode]) extends TemplateNode {

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
      State.inspect { context =>
        expr.eval(context).toStr.value
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
            Monad[State[Context, ?]].ifM(State.inspect(context => n.condition.eval(context) == Value.True))(
              ifTrue = n.content.eval.map(_.some),
              ifFalse = State.pure(None))
          case some => State.pure(some)
        }
      }
    }.map(_.getOrElse(""))
  }

  object If {

    final case class ConditionalContent(condition: expression.syntax.AST, content: Partial)
  }

  final case class For(identifiers: Seq[expression.syntax.AST.Identifier],
                       expr: expression.syntax.AST.Expr,
                       partial: Partial,
                       elseCase: Option[Partial])
      extends Tag {

    override def eval: State[Context, String] = State.inspect[Context, String] { context =>
      val value = expr.eval(context).toArr.values

      if (value.nonEmpty) {
        value.zipWithIndex
          .map {
            case (subValues, i) =>
              val loop = Value.Obj(
                "index" -> Value.Number(i + 1),
                "index0" -> Value.Number(i),
                "revindex" -> Value.Number(value.length - i),
                "revindex0" -> Value.Number(value.length - i - 1),
                "first" -> (if (i == 0) Value.True else Value.False),
                "last" -> (if (i == value.length - 1) Value.True
                else Value.False),
                "length" -> Value.Number(value.length)
              )

              val parameters =
                identifiers.map(_.value).zip(subValues.destructure)
              val scope = context.scope
                .set("loop", loop)
                .set(parameters: _*)

              partial.eval.runA(context.setScope(scope))
          }
          .foldLeft("")(_ + _.value)
      } else {
        elseCase match {
          case Some(p) => p.eval.runA(context).value
          case None => ""
        }
      }
    }
  }

  final case class Set(names: NonEmptyList[expression.syntax.AST.Identifier], expr: expression.syntax.AST) extends Tag {

    override def eval: State[Context, String] =
      State
        .inspect[Context, Value](context => expr.eval(context))
        .flatMap { value =>
          names
            .foldLeft(State.pure[Context, Unit](())) { (m, n) =>
              m.flatMap(_ => State.modify(_.setScope(n.value, value)))
            }
            .map(_ => "")
        }
  }

  final case class SetBlock(names: NonEmptyList[expression.syntax.AST.Identifier], partial: Partial) extends Tag {

    override def eval: State[Context, String] = partial.eval.flatMap { content =>
      names
        .foldLeft(State.pure[Context, Unit](())) { (m, n) =>
          m.flatMap(_ => State.modify(_.setScope(n.value, Value.Str(content))))
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
          val body = Value.Function { (callingScope, parameters) =>
            val defaultArguments = args.flatMap {
              case (id, value) =>
                value.map(id.value -> _.eval(context))
            }

            val byNameParameters = parameters.parameters.toList
              .mapFilter(param => param.name.map(_ -> param.value))
              .toMap

            val positionalParameters = args.keys
              .map(_.value)
              .filterNot(byNameParameters.isDefinedAt)
              .zip(parameters.parameters.toList.mapFilter(param => if (param.name.isEmpty) Some(param.value) else None))

            val completeParameters =
              (defaultArguments ++ byNameParameters ++ positionalParameters).toList

            Value.Str(
              content.eval
                .runA(context.setScope(callingScope.set(completeParameters: _*)))
                .value)
          }

          context.setScope(identifier.value, body)
        }
        .map(_ => "")
  }

  final case class Call(identifier: expression.syntax.AST.Identifier,
                        parameters: Seq[(Option[expression.syntax.AST.Identifier], expression.syntax.AST.Expr)],
                        partial: Partial)
      extends Tag {

    override def eval: State[Context, String] = State.inspect[Context, String] { context =>
      val body = Value.Function { (scope, _) =>
        Value.Str(partial.eval.runA(context.setScope(scope)).value)
      }

      val resolvedParameters = Value.Function.Parameters(parameters.map {
        case (k, v) =>
          Value.Function.Parameter(k.map(_.value), v.eval(context))
      })

      context
        .getScope(identifier.value)(context.setScope("caller", body).scope, resolvedParameters)
        .toStr
        .value
    }
  }

  final case class Include(expr: expression.syntax.AST.Expr, ignoreMissing: Boolean) extends Tag {

    override def eval: State[Context, String] = State.inspect[Context, String] { context =>
      val partial = expr.eval(context).toStr.value
      context.environment.renderTemplate(partial, context.scope).getOrElse {
        if (ignoreMissing) ""
        else throw new RuntimeException(s"missing template `$partial`")
      }
    }
  }

  final case class Import(expr: expression.syntax.AST.Expr, identifier: expression.syntax.AST.Identifier) extends Tag {

    override def eval: State[Context, String] =
      State
        .modify[Context] { context =>
          val partial = expr.eval(context).toStr.value
          context.environment
            .importTemplate(partial)
            .map { scope =>
              context.setScope(identifier.value, scope)
            }
            .getOrElse(throw new RuntimeException(s"missing template `$partial`"))
        }
        .map(_ => "")
  }

  final case class From(expr: expression.syntax.AST.Expr,
                        identifiers: Seq[(expression.syntax.AST.Identifier, Option[expression.syntax.AST.Identifier])])
      extends Tag {

    override def eval: State[Context, String] =
      State
        .modify[Context] { context =>
          val partial = expr.eval(context).toStr.value
          context.environment
            .importTemplate(partial)
            .map { scope =>
              val values =
                identifiers.map {
                  case (key, preferred) =>
                    preferred.getOrElse(key).value -> scope.get(key.value)
                }
              context.setScope(values: _*)
            }
            .getOrElse(throw new RuntimeException(s"missing template `$partial`"))
        }
        .map(_ => "")
  }

  final case class Block(identifier: expression.syntax.AST.Identifier, partial: Partial) extends Tag {

    override def eval: State[Context, String] = State { context =>
      val rendered = (partial +: context.getBlocks(identifier.value))
        .foldLeft[Value](Value.Null) { (result, partial) =>
          val superFn = Value.Function { (_, _) =>
            result
          }

          Value.Str(partial.eval.runA(context.setScope("super", superFn)).value)
        }
        .toStr
        .value

      val newContext = context.setBlock(identifier.value, partial)

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
                       Value.Function.Parameter(k.map(_.value), v.eval(context))
                   })

                   context
                     .getFilter(identifier.value)
                     .map(_.apply(context.scope, expression.runtime.Value.Str(content), parameters))
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
    val parentTemplate = parent.eval(context).toStr.value
    val newContext =
      partial.map(_.eval.runS(context).value).getOrElse(context)
    context.environment
      .load(parentTemplate)
      .map { rootTemplate =>
        rootTemplate.render.runA(newContext).value
      }
      .getOrElse(throw new RuntimeException(s"missing template `$parentTemplate"))
  }
}

final case class ComplexTemplate(rootTemplate: RootTemplate, childTemplate: ChildTemplate) extends Template {

  override def render: State[Context, String] =
    for {
      root  <- rootTemplate.render
      child <- childTemplate.render
    } yield root + child
}
