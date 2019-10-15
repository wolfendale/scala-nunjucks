package wolfendale.nunjucks.expression

import wolfendale.nunjucks.{Context, Environment, ProvidedEnvironment}
import wolfendale.nunjucks.expression.runtime.Value

class ExpressionTester(environment: Environment = new ProvidedEnvironment()) {

  def evaluate(expression: String, scope: Value.Obj = Value.Obj.empty): Value = {
    import fastparse._
    parse(expression, Parser.expression(_)).get.value.eval(Context(environment, scope))
  }
}