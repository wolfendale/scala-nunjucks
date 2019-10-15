package wolfendale.nunjucks.expression

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.runtime.Value

class ArraySpec extends FreeSpec with MustMatchers {

  val tester = new ExpressionTester()

  "an array" - {

    "must have a length property" in {

      tester.evaluate("[1, 2, 3].length") mustEqual Value.Number(3)

      tester.evaluate("x.length", Value.Obj(
        "x" -> Value.Arr(Seq(Value.True, Value.False, Value.Str("foo"), Value.Str("bar")))
      )) mustEqual Value.Number(4)
    }
  }
}
