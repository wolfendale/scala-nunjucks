package wolfendale.nunjucks.expression

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.runtime.Value

class ConcatSpec extends FreeSpec with MustMatchers {

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

  }

}
