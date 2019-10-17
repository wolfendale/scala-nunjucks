package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value._

class DefaultSpec extends FreeSpec with MustMatchers {

  val tester = new ExpressionTester()

  "default filter" - {

    "without 'falsy' flag" - {

      "returns numeric as is" in {
        tester.evaluate("1 | default(2)") mustEqual Number(1)
      }

      "returns boolean true as is" in {
        tester.evaluate("true | default(2)") mustEqual True
      }

      "returns boolean false as is" in {
        tester.evaluate("false | default(2)") mustEqual False
      }

      "returns string as is" in {
        tester.evaluate("'hello' | default(2)") mustEqual Str("hello")
      }

      "returns default for undefined" in {
        tester.evaluate("function | default(3)") mustEqual Number(3)
      }

      "returns null as is" in {
        tester.evaluate("null | default(1)") mustEqual Null
      }

      "returns none as null" in {
        tester.evaluate("none | default(1)") mustEqual Null
      }
    }

    "with the 'falsy' flag as false" - {

      "returns numeric as is" in {
        tester.evaluate("1 | default(2, false)") mustEqual Number(1)
      }

      "returns boolean true as is" in {
        tester.evaluate("true | default(2, false)") mustEqual True
      }

      "returns boolean false as is" in {
        tester.evaluate("false | default(2, false)") mustEqual False
      }

      "returns string as is" in {
        tester.evaluate("'hello' | default(2, false)") mustEqual Str("hello")
      }

      "returns default for undefined" in {
        tester.evaluate("function | default(3, false)") mustEqual Number(3)
      }

      "returns null as is" in {
        tester.evaluate("null | default(1, false)") mustEqual Null
      }

      "returns none as null" in {
        tester.evaluate("none | default(1, false)") mustEqual Null
      }
    }

    "with the 'falsy' flag as true" - {

      "returns numeric as is" in {
        tester.evaluate("1 | default(2, true)") mustEqual Number(1)
      }

      "returns boolean true as is" in {
        tester.evaluate("true | default(2, true)") mustEqual True
      }

      "returns default for boolean false" in {
        tester.evaluate("false | default(2, true)") mustEqual Number(2)
      }

      "returns string as is" in {
        tester.evaluate("'hello' | default(2, true)") mustEqual Str("hello")
      }

      "returns default for undefined" in {
        tester.evaluate("function | default(3, true)") mustEqual Number(3)
      }

      "returns default for null" in {
        tester.evaluate("null | default(1, true)") mustEqual Number(1)
      }

      "returns default for none" in {
        tester.evaluate("none | default(1, true)") mustEqual Number(1)
      }
    }

    "should be aliased to 'd'" in {
      tester.evaluate("1 | d(2)") mustEqual Number(1)
    }

  }
}
