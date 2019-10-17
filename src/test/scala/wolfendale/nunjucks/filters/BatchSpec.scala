package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value

class BatchSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  val tester = new ExpressionTester()

  "batch filter" - {
    "must batch provided arrays" in {

      tester.evaluate("items|batch(3)", Value.Obj(
        "items" -> Value.Arr(Seq(Value.Str("a"), Value.Str("b"), Value.Str("c"), Value.Str("d"), Value.Str("e"), Value.Str("f"), Value.Str("g")))
      )) mustEqual
        Value.Arr(Seq(
            Value.Arr(Seq(Value.Str("a"), Value.Str("b"), Value.Str("c"))),
            Value.Arr(Seq(Value.Str("d"), Value.Str("e"), Value.Str("f"))),
            Value.Arr(Seq(Value.Str("g")))
        ))
    }

    "must fill to last batch size when provided a fill character" in {
      tester.evaluate("items|batch(3, '-')", Value.Obj(
        "items" -> Value.Arr(Seq(Value.Str("a"), Value.Str("b"), Value.Str("c"), Value.Str("d"), Value.Str("e"), Value.Str("f"), Value.Str("g")))
      )) mustEqual
        Value.Arr(Seq(
          Value.Arr(Seq(Value.Str("a"), Value.Str("b"), Value.Str("c"))),
          Value.Arr(Seq(Value.Str("d"), Value.Str("e"), Value.Str("f"))),
          Value.Arr(Seq(Value.Str("g"), Value.Str("-"), Value.Str("-")))
        ))
    }
  }
}
