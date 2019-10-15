package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value

class StringSpec extends FreeSpec with MustMatchers {

  val tester = new ExpressionTester()

  "string filter" - {

    "must turn entity into String" in {

      tester.evaluate("1 | string") mustEqual Value.Str("1")
    }
  }
}
