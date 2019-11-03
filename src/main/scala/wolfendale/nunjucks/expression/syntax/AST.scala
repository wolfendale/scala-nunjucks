package wolfendale.nunjucks.expression.syntax

import cats._
import cats.data._
import cats.implicits._
import wolfendale.nunjucks.Context
import wolfendale.nunjucks.expression.runtime.Value

sealed abstract class AST {

  def eval: State[Context, Value]
}

object AST {

  sealed abstract class Expr extends AST

  abstract class Primitive(value: Value) extends Expr {

    override val eval: State[Context, Value] =
      State.pure(value)
  }

  case object Null extends Primitive(Value.Null)

  case object Undefined extends Primitive(Value.Undefined)

  sealed trait Bool extends Expr

  case object True extends Primitive(Value.True) with Bool

  case object False extends Primitive(Value.False) with Bool

  sealed trait Numeric extends Expr

  case object Infinity extends Primitive(Value.Infinity) with Numeric

  final case class Number(value: Double) extends Primitive(Value.Number(value)) with Numeric

  final case class Str(value: String) extends Primitive(Value.Str(value))

  final case class Arr(values: Vector[Expr]) extends Expr {

    override def eval: State[Context, Value] =
      values
        .foldLeft(State.pure[Context, Vector[Value]](Vector.empty)) {
          case (s, v) =>
            for {
              rest <- s
              next <- v.eval
            } yield rest :+ next
        }
        .map(Value.Arr(_))
  }

  final case class Obj(entries: Map[String, Expr]) extends Expr {

    override def eval: State[Context, Value] = {

      entries
        .foldLeft(State.pure[Context, Map[String, Value]](Map.empty)) {
          case (s, (k, v)) =>
            for {
              rest <- s
              next <- v.eval
            } yield rest + (k -> next)
        }
        .map(Value.Obj(_))
    }
  }

  final case class Identifier(value: String) extends Expr {

    override def eval: State[Context, Value] =
      State.inspect { context =>
        context.getContextValue(value)
      }
  }

  final case class Access(expr: Expr, identifier: Identifier) extends Expr {

    override def eval: State[Context, Value] =
      for {
        value <- expr.eval
      } yield value.access(identifier.value)
  }

  final case class ComputedAccess(expr: Expr, identifier: Expr) extends Expr {

    override def eval: State[Context, Value] =
      for {
        value      <- expr.eval
        identifier <- identifier.eval
      } yield value.access(identifier.toStr.value)
  }

  final case class UnaryMinus(operand: Expr) extends Expr {

    override def eval: State[Context, Value] =
      for {
        value <- operand.eval
      } yield -value
  }

  final case class UnaryPlus(operand: Expr) extends Expr {

    override def eval: State[Context, Value] =
      for {
        value <- operand.eval
      } yield +value
  }

  final case class Not(operand: Expr) extends Expr {

    override def eval: State[Context, Value] =
      for {
        value <- operand.eval
      } yield !value
  }

  final case class BinaryOperator(operator: BinaryOperator.Operation, left: Expr, right: Expr) extends Expr {

    override def eval: State[Context, Value] =
      for {
        left  <- left.eval
        right <- right.eval
      } yield operator.eval(left, right)
  }

  object BinaryOperator {

    sealed abstract class Operation {
      def eval(left: Value, right: Value): Value
    }

    case object Equality extends Operation {
      override def eval(left: Value, right: Value): Value =
        left `==` right
    }

    case object Inequality extends Operation {
      override def eval(left: Value, right: Value): Value =
        left `!=` right
    }

    case object StrictEquality extends Operation {
      override def eval(left: Value, right: Value): Value =
        left `===` right
    }

    case object StrictInequality extends Operation {
      override def eval(left: Value, right: Value): Value =
        left `!==` right
    }

    case object GT extends Operation {
      override def eval(left: Value, right: Value): Value =
        left > right
    }

    case object GTE extends Operation {
      override def eval(left: Value, right: Value): Value =
        left >= right
    }

    case object LT extends Operation {
      override def eval(left: Value, right: Value): Value =
        left < right
    }

    case object LTE extends Operation {
      override def eval(left: Value, right: Value): Value =
        left <= right
    }

    case object And extends Operation {
      override def eval(left: Value, right: Value): Value =
        left && right
    }

    case object Or extends Operation {
      override def eval(left: Value, right: Value): Value =
        left || right
    }

    case object Plus extends Operation {
      override def eval(left: Value, right: Value): Value =
        left + right
    }

    case object Minus extends Operation {
      override def eval(left: Value, right: Value): Value =
        left - right
    }

    case object Concat extends Operation {
      override def eval(left: Value, right: Value): Value =
        Value.Str(left.toStr.value + right.toStr.value)
    }

    case object Multiply extends Operation {
      override def eval(left: Value, right: Value): Value =
        left * right
    }

    case object Divide extends Operation {
      override def eval(left: Value, right: Value): Value =
        left / right
    }

    case object IDivide extends Operation {
      override def eval(left: Value, right: Value): Value =
        left idiv right
    }

    case object Remainder extends Operation {
      override def eval(left: Value, right: Value): Value =
        left % right
    }

    case object Power extends Operation {
      override def eval(left: Value, right: Value): Value =
        left ** right
    }

    case object In extends Operation {
      override def eval(left: Value, right: Value): Value = right match {
        case a: Value.Arr =>
          if (a.values.exists(i => (i `===` left) == Value.True)) Value.True else Value.False
        case o: Value.Obj =>
          if (o.values.keys.toList.contains(left.toStr.value)) Value.True else Value.False
        case s: Value.Str =>
          if (s.value.contains(left.toStr.value)) Value.True else Value.False
        case o =>
          throw new RuntimeException(s"cannot check contents of ${o.toStr.value}")
      }
    }

    case object NotIn extends Operation {
      override def eval(left: Value, right: Value): Value = right match {
        case a: Value.Arr =>
          if (a.values.exists(i => (i `===` left) == Value.True)) Value.False else Value.True
        case o: Value.Obj =>
          if (o.values.keys.toList.contains(left.toStr.value)) Value.False else Value.True
        case s: Value.Str =>
          if (s.value.contains(left.toStr.value)) Value.False else Value.True
        case o =>
          throw new RuntimeException(s"cannot check contents of ${o.toStr.value}")
      }
    }
  }

  final case class Call(expr: Expr, args: Seq[(Option[Identifier], Expr)]) extends Expr {

    override def eval: State[Context, Value] = {

      val parameters = args.foldLeft(State.pure[Context, Vector[Value.Function.Parameter]](Vector.empty)) {
        case (s, (i, v)) =>
          for {
            rest <- s
            next <- v.eval
          } yield rest :+ Value.Function.Parameter(i.map(_.value), next)
      }.map(Value.Function.Parameters(_))

      for {
        fn     <- expr.eval
        params <- parameters
        result <- fn(params)
      } yield result
    }
  }

  final case class FilterCall(expr: Expr,
                              identifier: Identifier,
                              args: Seq[(Option[Identifier], Expr)],
                              condition: Option[FilterCall.Condition])
      extends Expr {

    override def eval: State[Context, Value] = {

      lazy val parameters = args.foldLeft(State.pure[Context, Vector[Value.Function.Parameter]](Vector.empty)) {
        case (s, (i, v)) =>
          for {
            rest <- s
            next <- v.eval
          } yield rest :+ Value.Function.Parameter(i.map(_.value), next)
      }.map(Value.Function.Parameters(_))

      lazy val filtered = for {
        value  <- expr.eval
        params <- parameters
        result <- State.inspect[Context, Value] { context =>
          context.environment
            .getFilter(identifier.value)
            .map(_.apply(context.scope.get, value, params))
            .getOrElse(throw new RuntimeException(s"Filter not found: ${identifier.value}"))
        }
      } yield result

      condition
        .map { condition =>
          Monad[State[Context, *]].ifM(condition.condition.eval.map(_.toBool))(
            ifTrue = filtered,
            ifFalse = condition.otherValue
              .map(_.eval)
              .getOrElse(State.pure[Context, Value](Value.Undefined))
          )
        }
        .getOrElse(filtered)
    }
  }

  object FilterCall {

    final case class Condition(condition: Expr, otherValue: Option[Expr])
  }

  final case class If(body: Expr, condition: Expr, other: Option[Expr]) extends Expr {

    override def eval: State[Context, Value] =
      Monad[State[Context, *]].ifM(condition.eval.map(_.toBool))(
        ifTrue = body.eval,
        ifFalse = other.map(_.eval).getOrElse(State.pure(Value.Undefined))
      )
  }

  final case class In(item: Expr, container: Expr) extends Expr {

    override def eval: State[Context, Value] = for {
      container <- container.eval
      item <- item.eval
    } yield container match {
      case a: Value.Arr =>
        Value.Bool(a.values.exists(i => i `===` item))
      case o: Value.Obj =>
        Value.Bool(o.values.keys.toList.contains(item.toStr.value))
      case s: Value.Str =>
        Value.Bool(s.value.contains(item.toStr.value))
      case o =>
        throw new RuntimeException(s"cannot check contents of ${o.toStr.value}")
    }
  }

  final case class NotIn(item: Expr, container: Expr) extends Expr {

    override def eval: State[Context, Value] = for {
      container <- container.eval
      item <- item.eval
    } yield container match {
      case a: Value.Arr =>
        !Value.Bool(a.values.exists(i => i `===` item))
      case o: Value.Obj =>
        !Value.Bool(o.values.keys.toList.contains(item.toStr.value))
      case s: Value.Str =>
        !Value.Bool(s.value.contains(item.toStr.value))
      case o =>
        throw new RuntimeException(s"cannot check contents of ${o.toStr.value}")
    }
  }

  final case class Regex(pattern: String, flags: Set[RegexFlag] = Set.empty) extends Expr {
    override def eval: State[Context, Value] =
      State.pure(Value.Regex(pattern, flags.map(Value.RegexFlag.apply)))
  }

  sealed abstract class RegexFlag(val flag: String)

  object RegexFlag {
    def apply(allFlags: String): Set[RegexFlag] =
      allFlags.split("").toSet[String].collect {
        case ApplyGlobally.flag   => ApplyGlobally
        case CaseInsensitive.flag => CaseInsensitive
        case MultiLine.flag       => MultiLine
        case Sticky.flag          => Sticky
      }

    final case object ApplyGlobally extends RegexFlag("g")

    final case object CaseInsensitive extends RegexFlag("i")

    final case object MultiLine extends RegexFlag("m")

    final case object Sticky extends RegexFlag("y")

  }

}
