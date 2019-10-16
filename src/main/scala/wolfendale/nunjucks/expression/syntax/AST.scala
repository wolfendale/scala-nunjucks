package wolfendale.nunjucks.expression.syntax

import wolfendale.nunjucks.Context
import wolfendale.nunjucks.expression.runtime.Value

sealed abstract class AST {

  def eval(context: Context): Value
}

object AST {

  sealed abstract class Expr extends AST

  abstract class Primitive(value: Value) extends Expr {
    override def eval(context: Context): Value =
      value
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

    override def eval(context: Context): Value =
      Value.Arr(values.map(_.eval(context)))
  }

  final case class Obj(entries: Map[String, Expr]) extends Expr {

    override def eval(context: Context): Value =
      Value.Obj(entries.mapValues(_.eval(context)))
  }

  final case class Identifier(value: String) extends Expr {

    override def eval(context: Context): Value =
      context.getScope(value)
  }

  final case class Access(expr: Expr, identifier: Identifier) extends Expr {

    override def eval(context: Context): Value =
      expr.eval(context) access identifier.value
  }

  final case class ComputedAccess(expr: Expr, identifier: Expr) extends Expr {

    override def eval(context: Context): Value =
      expr.eval(context) access identifier.eval(context).toStr.value
  }

  final case class UnaryMinus(operand: Expr) extends Expr {

    override def eval(context: Context): Value =
      -operand.eval(context)
  }

  final case class UnaryPlus(operand: Expr) extends Expr {

    override def eval(context: Context): Value =
      +operand.eval(context)
  }

  final case class Not(operand: Expr) extends Expr {

    override def eval(context: Context): Value =
      !operand.eval(context)
  }

  final case class BinaryOperator(operator: BinaryOperator.Operation, left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      operator.eval(left.eval(context), right.eval(context))
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

  final case class Call(expr: Expr, args: Seq[(Option[Identifier], Expr)])
      extends Expr {

    override def eval(context: Context): Value = {

      val parameters = Value.Function.Parameters(args.map {
        case (k, v) =>
          Value.Function.Parameter(k.map(_.value), v.eval(context))
      })

      expr.eval(context)(context.scope, parameters)
    }
  }

  final case class FilterCall(expr: Expr, identifier: Identifier, args: Seq[(Option[Identifier], Expr)]) extends Expr {

    override def eval(context: Context): Value = {

      val parameters = Value.Function.Parameters(args.map {
        case (k, v) =>
          Value.Function.Parameter(k.map(_.value), v.eval(context))
      })

      context.getFilter(identifier.value).map(_.apply(context.scope, expr.eval(context), parameters))
        .getOrElse(throw new RuntimeException(s"No filter with name: ${identifier.value}"))
    }
  }

  final case class If(body: Expr, condition: Expr, other: Option[Expr]) extends Expr {

    override def eval(context: Context): Value =
      if (condition.eval(context) `==` Value.True) {
        body.eval(context)
      } else {
        other.map(_.eval(context)).getOrElse(Value.Undefined)
      }
  }
}
