package wolfendale.nunjucks.expression

import fastparse._
import SingleLineWhitespace._
import wolfendale.nunjucks.expression.syntax.AST

object Parser {

  def expression[_: P]: P[AST.Expr] = {

    def recurBinop(previousTerm: => P[AST.Expr], operators: => P[AST.Expr => AST.Expr]): P[AST.Expr] = {

      val combine = (first: AST.Expr, rest: Seq[AST.Expr => AST.Expr]) =>
        rest.tail.foldLeft(rest.head(first)) { (m, n) =>
          n(m)
      }

      P((previousTerm ~ operators.rep(1)).map(combine.tupled) | previousTerm)
    }

    def binop[A <: AST.Expr](op: String, previousTerm: => P[AST.Expr], f: (AST.Expr, AST.Expr) => A): P[AST.Expr => A] =
      P(op ~ previousTerm).map(x => f(_, x))

    def primary =
      P(boolean | `null` | `undefined` | number | string | identifier | obj | arr)

    def group =
      P(("(" ~ expression ~ ")") | primary)

    def access = {
      sealed abstract class Access
      final case class DirectAccess(identifier: AST.Identifier)            extends Access
      final case class ComputedAccess(identifier: AST.Expr)                extends Access
      final case class Call(args: Seq[(Option[AST.Identifier], AST.Expr)]) extends Access
      def directAccess   = P("." ~ identifier).map(DirectAccess)
      def computedAccess = P("[" ~ expression ~ "]").map(ComputedAccess)
      def call =
        P(
          "(" ~ ((identifier ~ "=").? ~ expression)
            .rep(sep = ",") ~ ")").map(Call)
      P(group ~ (directAccess | computedAccess | call).rep).map {
        case (lhs, chunks) =>
          chunks.foldLeft(lhs) {
            case (l, DirectAccess(identifier)) =>
              AST.Access(l, identifier)
            case (l, ComputedAccess(expr)) =>
              AST.ComputedAccess(l, expr)
            case (l, Call(args)) =>
              AST.Call(l, args)
          }
      }
    }

    def unary: P[AST.Expr] = {
      def not   = P("not" ~ group).map(AST.Not)
      def minus = P("-" ~ group).map(AST.UnaryMinus)
      def plus  = P("+" ~ group).map(AST.UnaryPlus)
      P(not | plus | minus | access | group)
    }

    def power: P[AST.Expr] = {
      def pow = binop("**", unary, AST.Pow)
      recurBinop(unary, pow)
    }

    def multiplication = {
      def multiply  = binop("*", power, AST.Multiply)
      def divide    = binop("/", power, AST.Divide)
      def idivide   = binop("//", power, AST.IDivide)
      def remainder = binop("%", power, AST.Remainder)
      recurBinop(power, P(multiply | divide | idivide | remainder))
    }

    def addition = {
      def plus  = binop("+", multiplication, AST.Plus)
      def minus = binop("-", multiplication, AST.Minus)
      recurBinop(multiplication, P(plus | minus))
    }

    def comparison = {
      def gt  = binop(">", addition, AST.GT)
      def gte = binop(">=", addition, AST.GTE)
      def lt  = binop("<", addition, AST.LT)
      def lte = binop("<=", addition, AST.LTE)
      recurBinop(addition, P(gt | gte | lt | lte))
    }

    def equality = {
      def eq   = binop("==", comparison, AST.Equality)
      def neq  = binop("!=", comparison, AST.Inequality)
      def seq  = binop("===", comparison, AST.StrictEquality)
      def sneq = binop("!==", comparison, AST.StrictInequality)
      recurBinop(comparison, P(eq | neq | seq | sneq))
    }

    def and = {
      def and = binop("and", equality, AST.And)
      recurBinop(equality, and)
    }

    def or = {
      def or = binop("or", and, AST.Or)
      recurBinop(and, or)
    }

    def conditional = {
      def conditional = P(or ~ "if" ~ expression ~ ("else" ~ expression).?).map(AST.If.tupled)
      P(conditional | or)
    }

    // TODO Is this the right precedence for filter call?
    def filterCall = {
      P(
        conditional ~ ("|" ~ identifier ~ ("(" ~ ((identifier ~ "=").? ~ expression).rep(sep = ",") ~ ")").?.map(
          _.getOrElse(Seq.empty))).rep).map {
        case (lhs, chunks) =>
          chunks.foldLeft(lhs) {
            case (l, (id, params)) =>
              AST.FilterCall(l, id, params)
          }
      }
    }

    filterCall
  }

  def `null`[_: P] = P("null").map(_ => AST.Null)

  def `undefined`[_: P] = P("undefined").map(_ => AST.Undefined)

  def boolean[_: P] = {
    def `true`  = P("true").map(_ => AST.True)
    def `false` = P("false").map(_ => AST.False)
    P(`true` | `false`)
  }

  def identifier[_: P] =
    P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map(AST.Identifier)

  def number[_: P] = {
    def digits     = P(CharsWhileIn("0-9"))
    def integral   = P("0" | CharIn("1-9") ~ digits.?)
    def fractional = P("." ~ digits)
    def exponent   = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
    def number =
      P(integral ~ fractional.? ~ exponent.?).!.map(string => AST.Number(string.toDouble))
    def infinity = P("Infinity").map(_ => AST.Infinity)
    P(number | infinity)
  }

  def string[_: P] = {
    def withDelimiter(delim: Char) = {
      def stringChars = P(CharsWhile(c => c != delim && c != '\\'))
      def escaped     = P("\\" ~/ CharIn("\"\\\\"))
      P(s"$delim" ~ (stringChars | escaped).rep.! ~ s"$delim")
        .map(AST.Str)
    }
    withDelimiter('\"') | withDelimiter('\'')
  }

  def obj[_: P] = {
    def entry =
      P((identifier.map(_.value) | string.map(_.value)) ~/ ":" ~ expression)
    P("{" ~/ entry.rep(sep = ",") ~ "}")
      .map(entries => AST.Obj(entries.toMap))
  }

  def arr[_: P] =
    P("[" ~/ (expression.rep(sep = ",") ~ "]"))
      .map(entries => AST.Arr(entries.toVector))
}
