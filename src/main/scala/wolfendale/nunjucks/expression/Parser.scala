package wolfendale.nunjucks.expression

import fastparse.MultiLineWhitespace._
import fastparse._
import wolfendale.nunjucks.expression.syntax.AST
import wolfendale.nunjucks.expression.syntax.AST.RegexFlag

object Parser {

  def expression[_: P]: P[AST.Expr] = {

    def primary =
      P(boolean | `null` | `undefined` | number | string | regex | identifier | obj | arr)

    def group =
      P(("(" ~ expression ~ ")") | primary)

    def access = {
      sealed abstract class Access
      final case class DirectAccess(identifier: AST.Identifier)            extends Access
      final case class ComputedAccess(identifier: AST.Expr)                extends Access
      final case class Call(args: Seq[(Option[AST.Identifier], AST.Expr)]) extends Access
      def directAccess = P("." ~ identifier).map(DirectAccess)

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
      def not = P("not" ~ group).map(AST.Not)

      def minus = P("-" ~ group).map(AST.UnaryMinus)

      def plus = P("+" ~ group).map(AST.UnaryPlus)

      P(not | plus | minus | access | group)
    }

    def binop(previousTerm: => P[AST.Expr], op: => P[AST.BinaryOperator.Operation]) = P {
      (previousTerm ~ (op ~ previousTerm).rep).map {
        case (left, rest) =>
          rest.foldLeft(left) {
            case (l, (operator, r)) =>
              AST.BinaryOperator(operator, l, r)

          }
      }
    }

    def power: P[AST.Expr] = binop(unary, P("**").map(_ => AST.BinaryOperator.Power))

    def multiplication = {
      def multiply = P("*").map(_ => AST.BinaryOperator.Multiply)

      def idivide = P("//").map(_ => AST.BinaryOperator.IDivide)

      def divide = P("/").map(_ => AST.BinaryOperator.Divide)

      def remainder = P("%").map(_ => AST.BinaryOperator.Remainder)

      binop(power, P(multiply | idivide | divide | remainder))
    }

    def addition = {
      def plus = P("+").map(_ => AST.BinaryOperator.Plus)

      def minus = P("-").map(_ => AST.BinaryOperator.Minus)

      def concat = P("~").map(_ => AST.BinaryOperator.Concat)
      binop(multiplication, P(plus | minus | concat))
    }

    def comparison = {
      def gt = P(">").map(_ => AST.BinaryOperator.GT)

      def gte = P(">=").map(_ => AST.BinaryOperator.GTE)

      def lt = P("<").map(_ => AST.BinaryOperator.LT)

      def lte = P("<=").map(_ => AST.BinaryOperator.LTE)

      def in = P("in").map(_ => AST.BinaryOperator.In)

      def notIn = P("not " ~ "in").map(_ => AST.BinaryOperator.NotIn)

      binop(addition, P(gte | gt | lte | lt | in | notIn))
    }

    def equality = {
      def eq = P("==").map(_ => AST.BinaryOperator.Equality)

      def neq = P("!=").map(_ => AST.BinaryOperator.Inequality)

      def seq = P("===").map(_ => AST.BinaryOperator.StrictEquality)

      def sneq = P("!==").map(_ => AST.BinaryOperator.StrictInequality)

      binop(comparison, P(sneq | seq | neq | eq))
    }

    def and = {
      def and = P("and").map(_ => AST.BinaryOperator.And)

      binop(equality, and)
    }

    def or = {
      def or = P("or").map(_ => AST.BinaryOperator.Or)

      binop(and, or)
    }

    def conditional = {
      def conditional = P(NoCut(primary) ~ "if" ~ expression ~ ("else" ~ expression).?).map(AST.If.tupled)
      P(conditional | or)
    }

    // TODO Is this the right precedence for filter call?
    def filterCall =
      P(
        conditional ~ ("|" ~ identifier ~ ("(" ~ ((identifier ~ "=").? ~ expression).rep(sep = ",") ~ ")").?.map(
          _.getOrElse(Seq.empty))).rep ~ ("if" ~ expression ~ ("else" ~ expression).?).?.map(_.map(AST.FilterCall.Condition.tupled))
      ).map {
        case (lhs, chunks, condition) =>
          chunks.foldLeft(lhs) {
            case (l, (id, params)) =>
              AST.FilterCall(l, id, params, condition)
          }
      }

    filterCall
  }

  def `null`[_: P] = P("null" | "none").map(_ => AST.Null)

  def `undefined`[_: P] = P("undefined").map(_ => AST.Undefined)

  def boolean[_: P] = {
    def `true` = P("true").map(_ => AST.True)

    def `false` = P("false").map(_ => AST.False)

    P(`true` | `false`)
  }

  def identifier[_: P] =
    P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map(AST.Identifier)

  def number[_: P] = {
    def digits = P(CharsWhileIn("0-9"))

    def integral = P("0" | CharIn("1-9") ~ digits.?)

    def fractional = P("." ~ digits)

    def exponent = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)

    P(integral ~ fractional.? ~ exponent.?).!.map(string => AST.Number(string.toDouble))
  }

  def string[_: P] = {
    def withDelimiter(delim: Char) = {
      def stringChars = P(CharsWhile(c => c != delim && c != '\\'))

      def escaped = P("\\" ~ CharIn("\"\\\\"))

      P(s"$delim" ~ (stringChars | escaped).rep.!./ ~ s"$delim")
        .map(AST.Str)
    }

    withDelimiter('\"') | withDelimiter('\'')
  }

  def regex[_: P] = {

    def stringChars = P(CharsWhile(c => c != '/' && c != '\\'))

    def escaped = P("\\" ~~ "/")

    def flags = {
      def flag(flags: Set[Char]): P[List[Char]] = P {
        CharPred(flags.contains).!.?.flatMapX {
          case None =>
            Pass(Nil)
          case Some(f) =>
            flag(flags - f.charAt(0)).map(f.charAt(0) :: _)
        }
      }

      flag(Set('g', 'i', 'm', 'y'))
    }

    P("r/" ~~ (stringChars | escaped).repX.! ~~ "/" ~~ flags.!)
      .map {
        case (pattern, allFlags) =>
          AST.Regex(pattern, RegexFlag(allFlags))
      }
  }

  def obj[_: P] = {
    def entry =
      P((identifier.map(_.value) | string.map(_.value)) ~ ":" ~ expression)

    P("{" ~ entry.rep(sep = ",") ~ "}")
      .map(entries => AST.Obj(entries.toMap))
  }

  def arr[_: P] =
    P("[" ~ (expression.rep(sep = ",") ~ "]"))
      .map(entries => AST.Arr(entries.toVector))
}
