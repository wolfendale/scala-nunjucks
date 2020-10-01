package wolfendale.nunjucks.filters

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value

class StringSpec extends AnyFreeSpec with Matchers {

  val tester = new ExpressionTester()

  "string filter" - {

    "must turn entity into String" in {

      tester.evaluate("1 | string") mustEqual Value.Str("1")
    }
  }
}
