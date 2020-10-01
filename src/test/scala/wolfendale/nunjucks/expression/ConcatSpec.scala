package wolfendale.nunjucks.expression

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.expression.runtime.Value

class ConcatSpec extends AnyFreeSpec with Matchers {

  val tester = new ExpressionTester()

  "the ~ (concat) operator" - {

    "should concatenate strings" in {

      tester.evaluate("'hello' ~ 'world'") mustEqual Value.Str("helloworld")
    }

    "should concatenate numerics" in {

      tester.evaluate("4 ~ 5") mustEqual Value.Str("45")
    }

    "should concatenate mixed types" in {

      tester.evaluate("'foo' ~ 5") mustEqual Value.Str("foo5")
    }

    "should concatenate recursively" in {

      tester.evaluate("'a' ~ 'b' ~ 5") mustEqual Value.Str("ab5")
    }

    "should concatenate undefined" in {

      tester.evaluate("undefined ~ 'world'") mustEqual Value.Str("undefinedworld")
    }

    "should concatenate null" in {

      tester.evaluate("null ~ 'world'") mustEqual Value.Str("nullworld")
    }

    "should concatenate NaN" in {

      tester.evaluate("(+'asdf') ~ 'world'") mustEqual Value.Str("NaNworld")
    }

    "should concatenate Infinity" in {

      tester.evaluate("(1/0) ~ 'world'") mustEqual Value.Str("Infinityworld")
    }

  }

}
