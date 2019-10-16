package wolfendale.nunjucks

import org.scalatest.{FreeSpec, MustMatchers}
import fastparse._
import SingleLineWhitespace._
import wolfendale.nunjucks.expression.syntax

class BinOpParser extends FreeSpec with MustMatchers {

  "no recursion" in {

    sealed abstract class AST {
      def process: Int
    }

    final case class Num(value: Int) extends AST {
      override def process: Int = value
    }

    final case class Plus(left: AST, right: AST) extends AST {
      override def process: Int =
        left.process + right.process
    }

    final case class Minus(left: AST, right: AST) extends AST {
      override def process: Int =
        left.process - right.process
    }

    final case class Multiply(left: AST, right: AST) extends AST {
      override def process: Int =
        left.process * right.process
    }

    def recurBinop[_: P](previousTerm: => P[AST], operators: => P[AST => AST]): P[AST] = {

      val combine = (first: AST, rest: Seq[AST => AST]) =>
        rest.tail.foldLeft(rest.head(first)) { (m, n) =>
          n(m)
        }

      P((previousTerm ~ operators.rep(1)).map[AST](combine.tupled) | previousTerm)
    }

    def binop[_: P, A <: AST](op: String, previousTerm: => P[AST], f: (AST, AST) => A): P[AST => A] =
      P(op ~ previousTerm).map(num => f(_, num))

    def expression[_: P]: P[AST] = addition

    def number[_: P]: P[Num] = P(CharPred(_.isDigit).rep.!).map(str => Num(str.toInt))

    def multiplication[_: P] = {
      def multiply: P[AST => Multiply] = binop("*", number, Multiply)
      recurBinop(number, multiply)
    }

    def addition[_: P] = {
      def plus: P[AST => Plus]   = binop("+", multiplication, Plus)
      def minus: P[AST => Minus] = binop("-", multiplication, Minus)
      recurBinop(multiplication, P(plus | minus))
    }

    println(parse("1 + 2 * 3", expression(_)).get.value)
  }
}
