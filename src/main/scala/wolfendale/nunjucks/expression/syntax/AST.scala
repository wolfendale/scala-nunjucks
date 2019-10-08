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

  final case class Equality(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) `==` right.eval(context)
  }

  final case class Inequality(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) `!=` right.eval(context)
  }

  final case class StrictEquality(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) `===` right.eval(context)
  }

  final case class StrictInequality(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) `!==` right.eval(context)
  }

  final case class GT(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) > right.eval(context)
  }

  final case class GTE(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) >= right.eval(context)
  }

  final case class LT(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) < right.eval(context)
  }

  final case class LTE(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) <= right.eval(context)
  }

  final case class And(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) && right.eval(context)
  }

  final case class Or(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) || right.eval(context)
  }

  final case class Plus(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) + right.eval(context)
  }

  final case class Minus(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) - right.eval(context)
  }

  final case class Multiply(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) * right.eval(context)
  }

  final case class Divide(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) / right.eval(context)
  }

  final case class IDivide(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) idiv right.eval(context)
  }

  final case class Remainder(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) % right.eval(context)
  }

  final case class Pow(left: Expr, right: Expr) extends Expr {

    override def eval(context: Context): Value =
      left.eval(context) ** right.eval(context)
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

  final case class If(condition: Expr, body: Expr, other: Option[Expr]) extends Expr {

    override def eval(context: Context): Value =
      if (condition.eval(context) == Value.True) {
        body.eval(context)
      } else {
        other.map(_.eval(context)).getOrElse(Value.Undefined)
      }
  }
}
