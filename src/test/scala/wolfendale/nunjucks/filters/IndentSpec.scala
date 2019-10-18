package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value

 
class IndentSpec extends FreeSpec with MustMatchers {
    val tester = new ExpressionTester()

    "does not change single line string" in {
        tester.evaluate("\"dddd\" | indent") mustEqual Value.Str("dddd")
    }
}