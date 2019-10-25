package wolfendale.nunjucks.expression

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.runtime.Value

class StringSpec extends FreeSpec with MustMatchers {
    val tester = new ExpressionTester()

    "should evaluate simple string" in {
        tester.evaluate("\"hello\"") mustEqual Value.Str("hello")
    }

    "should evaluate string that starts with tab" in {
        tester.evaluate("\"\u0009hello\"") mustEqual Value.Str("\u0009hello")
    }

    "should evaluate string that ends with tab" in {
        tester.evaluate("\"hello\u0009\"") mustEqual Value.Str("hello\u0009")
    }

    "should evaluate string that starts with space" in {
        tester.evaluate("\" hello\"") mustEqual Value.Str(" hello")
    }

    "should evaluate string that ends with space" in {
        tester.evaluate("\"hello \"") mustEqual Value.Str("hello ")
    }
}
