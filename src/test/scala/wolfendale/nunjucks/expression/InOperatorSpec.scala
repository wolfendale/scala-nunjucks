package wolfendale.nunjucks.expression

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class InOperatorSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "the in operator" - {

    "on an array" - {

      "must return true when an item exists" in {

        environment.render("{{ 2 in [1, 2, 3] }}") mustEqual "true"
      }

      "must return false when an item does not exist" in {

        environment.render("{{ 2 in [1, 3] }}") mustEqual "false"
      }
    }

    "on an object" - {

      "must return true when a key exists" in {

        environment.render("{{ 'foo' in { foo: 1, bar: 2 } }}") mustEqual "true"
      }

      "must return false when a key does not exist" in {

        environment.render("{{ 'foo' in { bar: 2 } }}") mustEqual "false"
      }
    }

    "on a string" - {

      "must return true when the substring is contained" in {

        environment.render("{{ 'foo' in 'foobar' }}") mustEqual "true"
      }

      "must coerce the item to a string" in {

        environment.render("{{ 1 in '1337' }}") mustEqual "true"
      }

      "must return false when the substring is not contained" in {

        environment.render("{{ 'nothing' in 'foobar' }}") mustEqual "false"
      }
    }

    "must throw an exception on any other container type" in {

      assertThrows[RuntimeException] {
        environment.render("{{ 'foo' in 1 }}")
      }

      assertThrows[RuntimeException] {
        environment.render("{{ 1 in false }}")
      }

      assertThrows[RuntimeException] {
        environment.render("{{ x in undefined }}")
      }

      assertThrows[RuntimeException] {
        environment.render("{{ 1.3 in null }}")
      }
    }
  }

  "the not in operator" - {

    "on an array" - {

      "must return false when an item exists" in {

        environment.render("{{ 2 not in [1, 2, 3] }}") mustEqual "false"
      }

      "must return true when an item does not exist" in {

        environment.render("{{ 2 not in [1, 3] }}") mustEqual "true"
      }
    }

    "on an object" - {

      "must return false when a key exists" in {

        environment.render("{{ 'foo' not in { foo: 1, bar: 2 } }}") mustEqual "false"
      }

      "must return true when a key does not exist" in {

        environment.render("{{ 'foo' not in { bar: 2 } }}") mustEqual "true"
      }
    }

    "on a string" - {

      "must return false when the substring is contained" in {

        environment.render("{{ 'foo' not in 'foobar' }}") mustEqual "false"
      }

      "must coerce the item to a string" in {

        environment.render("{{ 2 not in '1337' }}") mustEqual "true"
      }

      "must return true when the substring is not contained" in {

        environment.render("{{ 'nothing' not in 'foobar' }}") mustEqual "true"
      }
    }

    "must throw an exception on any other container type" in {

      assertThrows[RuntimeException] {
        environment.render("{{ 'foo' not in 1 }}")
      }

      assertThrows[RuntimeException] {
        environment.render("{{ 1 not in false }}")
      }

      assertThrows[RuntimeException] {
        environment.render("{{ x not in undefined }}")
      }

      assertThrows[RuntimeException] {
        environment.render("{{ 1.3 not in null }}")
      }
    }
  }
}
