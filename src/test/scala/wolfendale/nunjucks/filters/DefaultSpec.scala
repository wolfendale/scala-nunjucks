package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value._

class DefaultSpec extends FreeSpec with MustMatchers {

  val tester = new ExpressionTester()

  "default filter" - {

    "without 'falsy' flag" - {

      "treats 0 as numeric" in {
        tester.evaluate("0 | default(1, false)") mustEqual Number(0)
      }

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
        tester.evaluate("undefined | default(3)") mustEqual Number(3)
      }

      "returns null as is" in {
        tester.evaluate("null | default(1)") mustEqual Null
      }

      "returns none as null" in {
        tester.evaluate("none | default(1)") mustEqual Null
      }

      "returns NaN as numeric" in {
        tester.evaluate("+'' | default(1)") mustEqual Number(0)
      }
    }

    "with the 'falsy' flag as false" - {

      "treats 0 as numeric" in {
        tester.evaluate("0 | default(1, false)") mustEqual Number(0)
      }

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
        tester.evaluate("undefined | default(3, false)") mustEqual Number(3)
      }

      "returns null as is" in {
        tester.evaluate("null | default(1, false)") mustEqual Null
      }

      "returns none as null" in {
        tester.evaluate("none | default(1, false)") mustEqual Null
      }

      "returns NaN as numeric" in {
        tester.evaluate("+'' | default(1, false)") mustEqual Number(0)
      }
    }

    "with the 'falsy' flag as true" - {

      "treats 0 as falsy" in {
        tester.evaluate("0 | default(1, true)") mustEqual Number(1)
      }

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
        tester.evaluate("undefined | default(3, true)") mustEqual Number(3)
      }

      "returns default for null" in {
        tester.evaluate("null | default(1, true)") mustEqual Number(1)
      }

      "returns default for none" in {
        tester.evaluate("none | default(1, true)") mustEqual Number(1)
      }

      "returns default for NaN" in {
        tester.evaluate("+'' | default(1, true)") mustEqual Number(1)
      }
    }

    "should be aliased to 'd'" in {
      tester.evaluate("1 | d(2)") mustEqual Number(1)
    }

    "should handle missing parens" - {

      "when value is defined" in {
        tester.evaluate("'Hello' | default") mustEqual Str("Hello")
      }

      "when value is undefined" in {
        tester.evaluate("undefined | default") mustEqual Undefined
      }

    }

    "should handle missing alternative argument" - {

      "when value is defined" in {
        tester.evaluate("'Hello' | default()") mustEqual Str("Hello")
      }

      "when value is undefined" in {
        tester.evaluate("undefined | default()") mustEqual Undefined
      }

    }

    "should ignore additional arguments" - {

      "when value is defined" in {
        tester.evaluate("'Hello' | default('world',true,true)") mustEqual Str("Hello")
      }

      "when value is undefined" in {
        tester.evaluate("undefined | default('world',true,true)") mustEqual Str("world")
      }

    }

  }
}
