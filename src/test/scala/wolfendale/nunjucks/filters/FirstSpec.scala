package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value._

class FirstSpec extends FreeSpec with MustMatchers {

  val tester = new ExpressionTester()

  "first filter" - {

    "for an Array" - {
      "returns the first element of the array" in {
        tester.evaluate("""[1,2,3] | first""") mustEqual Number(1)
      }

      "returns undefined when empty" in {

        tester.evaluate("""[] | first""") mustEqual Undefined
      }
    }

    "for a String" - {
      "returns the first element of the string" in {

        tester.evaluate(""""abc" | first""") mustEqual Str("a")
      }

      "returns undefined when empty" in {

        tester.evaluate("""'' | first""") mustEqual Undefined
      }
    }

    "return Undefined for" - {

      "an object" in {

        tester.evaluate("""{ key: 'value' } | first""") mustEqual Undefined
      }

      "a Number" in {

        tester.evaluate("""1337 | first""") mustEqual Undefined
      }

      "an expressions that evaluate to Infinity" in {

        tester.evaluate("""(1 / 0) | first""") mustEqual Undefined
      }


      "a Bool" in {
        tester.evaluate("""true | first""") mustEqual Undefined
        tester.evaluate("""false | first""") mustEqual Undefined
      }

      "NaN" in {

        tester.evaluate("""+ "asdf" | first""") mustEqual Undefined
      }

      "a Regex" ignore {

        tester.evaluate("""r/foo/ | first""") mustEqual Undefined
      }

    }

    "throw an exception" - {
      "for undefined " in {
        assertThrows[RuntimeException] {
          tester.evaluate("asdfasdfasdf | first")
        }

      }

      "for null" in {
        assertThrows[RuntimeException] {
          tester.evaluate("null | first")
        }
      }

    }
  }
}
