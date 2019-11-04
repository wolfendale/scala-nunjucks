package wolfendale.nunjucks.expression

import fastparse._
import wolfendale.nunjucks.expression.runtime.Value
import wolfendale.nunjucks.expression.syntax.AST
import wolfendale.nunjucks._

class ExpressionTester(environment: Environment = new ProvidedEnvironment()) {

  def evaluate(expression: String, scope: Value.Obj = Value.Obj.empty): Value = {
    ast(expression).get.value.eval.runA(Context(environment, RenderMode.Template, Scope.fromObj(scope))).value
  }

  def ast(expression: String): Parsed[AST.Expr] = {
    import fastparse._
    import SingleLineWhitespace._

    def parser[_: P] = P(Parser.expression ~ End)

    ast(expression, parser(_))
  }

  def ast(expression: String, specificParser: P[_] => P[AST.Expr]): Parsed[AST.Expr] = {
    import fastparse._

    parse(expression, specificParser)
  }

}

object ExpressionTester {

  implicit class ParsedExtension[T](parsed: Parsed[T]) {
    def toEither: Either[Parsed.Failure, Parsed.Success[T]] =
      parsed match {
        case p: Parsed.Failure    => Left(p)
        case p: Parsed.Success[T] => Right(p)
      }

    def optFailure: Option[Parsed.Failure] =
      parsed match {
        case p: Parsed.Failure    => Some(p)
        case p: Parsed.Success[T] => None
      }
  }

}
