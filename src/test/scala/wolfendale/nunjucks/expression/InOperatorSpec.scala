package wolfendale.nunjucks.expression

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.runtime.Value

class InOperatorSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()
  val tester      = new ExpressionTester()

  "the in operator" - {

    "on an array" - {

      "must return true when an item exists" in {

        tester.evaluate("2 in [1, 2, 3]") mustEqual Value.True
      }

      "must return false when an item does not exist" in {

        tester.evaluate("2 in [1, 3]") mustEqual Value.False
      }
    }

    "on an object" - {

      "must return true when a key exists" in {

        tester.evaluate("'foo' in { foo: 1, bar: 2}") mustEqual Value.True
      }

      "must return false when a key does not exist" in {

        tester.evaluate("'foo' in { bar: 2 }") mustEqual Value.False
      }
    }

    "on a string" - {

      "must return true when the substring is contained" in {

        tester.evaluate("'foo' in 'foobar'") mustEqual Value.True
      }

      "must coerce the item to a string" in {

        tester.evaluate("1 in '1337'") mustEqual Value.True
      }

      "must return false when the substring is not contained" in {

        tester.evaluate("'nothing' in 'foobar'") mustEqual Value.False
      }
    }

    "must throw an exception on any other container type" in {

      assertThrows[RuntimeException] {
        tester.evaluate("'foo' in 1")
      }

      assertThrows[RuntimeException] {
        tester.evaluate("1 in false")
      }

      assertThrows[RuntimeException] {
        tester.evaluate("x in undefined")
      }

      assertThrows[RuntimeException] {
        tester.evaluate("1.3 in null")
      }
    }
  }

  "the not in operator" - {

    "on an array" - {

      "must return false when an item exists" in {

        tester.evaluate("2 not in [1, 2, 3]") mustEqual Value.False
      }

      "must return true when an item does not exist" in {

        tester.evaluate("2 not in [1, 3]") mustEqual Value.True
      }
    }

    "on an object" - {

      "must return false when a key exists" in {

        tester.evaluate("'foo' not in { foo: 1, bar: 2 }") mustEqual Value.False
      }

      "must return true when a key does not exist" in {

        tester.evaluate("'foo' not in { bar: 2 }") mustEqual Value.True
      }
    }

    "on a string" - {

      "must return false when the substring is contained" in {

        tester.evaluate("'foo' not in 'foobar'") mustEqual Value.False
      }

      "must coerce the item to a string" in {

        tester.evaluate("2 not in '1337'") mustEqual Value.True
      }

      "must return true when the substring is not contained" in {

        tester.evaluate("'nothing' not in 'foobar'") mustEqual Value.True
      }
    }

    "must throw an exception on any other container type" in {

      assertThrows[RuntimeException] {
        tester.evaluate("'foo' not in 1")
      }

      assertThrows[RuntimeException] {
        tester.evaluate("1 not in false")
      }

      assertThrows[RuntimeException] {
        tester.evaluate("x not in undefined")
      }

      assertThrows[RuntimeException] {
        tester.evaluate("1.3 not in null")
      }
    }
  }
}
