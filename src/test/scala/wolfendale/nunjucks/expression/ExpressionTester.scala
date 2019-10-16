package wolfendale.nunjucks.expression

import wolfendale.nunjucks.{Context, Environment, Frame, ProvidedEnvironment}
import wolfendale.nunjucks.expression.runtime.Value

class ExpressionTester(environment: Environment = new ProvidedEnvironment()) {

  def evaluate(expression: String, scope: Value.Obj = Value.Obj.empty): Value = {
    import fastparse._
    import SingleLineWhitespace._

    def parser[_: P] = P(Parser.expression ~ End)

    parse(expression, parser(_)).get.value.eval(Context(environment, Frame(scope)))
  }
}
