package wolfendale.nunjucks

import java.nio.file.Paths

import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import wolfendale.nunjucks.expression.runtime.Value

class FrameSpec extends FreeSpec with MustMatchers with OptionValues {

  "a frame" - {

    "get" - {

      "must return None from a scope if there is nothing defined for the key in any scope" in {

        val scope = Frame.empty
          .set("foo", Value.Str("bar"), resolveUp = false)

        scope.get("baz") mustNot be (defined)
      }

      "must return a value from a scope if it exists" in {

        val scope = Frame.empty
          .set("foo", Value.Str("bar"), resolveUp = false)

        scope.get("foo") mustEqual Value.Str("bar")
      }

      "must return a value from the child scope even if it exists in the parent scope" in {

        val scope = Frame.empty
          .set("foo", Value.Str("rab"), resolveUp = false)
          .enter
          .set("foo", Value.Str("bar"), resolveUp = false)

        scope.get("foo") mustEqual Value.Str("bar")
      }

      "must return a value from the parent scope if it does not exist in child scope" in {

        val scope = Frame.empty
          .set("foo", Value.Str("rab"), resolveUp = false)
          .enter
          .set("bar", Value.Str("baz"), resolveUp = false)

        scope.get("foo") mustEqual Value.Str("rab")
      }
    }

    "set" - {

      "must set a value on the most specific scope" in {

        val result = Frame.empty
          .enter
          .set("foo", Value.Str("bar"), resolveUp = false)
          .exit
          .get("foo")

        result mustNot be (defined)
      }

      "must modify a value on the correct scope when resolveUp is true" in {

        val scope = Frame.empty
          .enter
          .set("foo", Value.Str("bar"), resolveUp = false)
          .enter
          .set("foo", Value.Str("rab"), resolveUp = true)

        scope.get("foo") mustEqual Value.Str("rab")
        scope.exit.get("foo") mustEqual Value.Str("rab")
      }

      // note: this seems odd but seems to be the behaviour we see in nunjucks
      "must not resolve up to root scope" in {

        val scope = Frame.empty
          .set("foo", Value.Str("bar"), resolveUp = false)
          .enter
          .set("foo", Value.Str("rab"), resolveUp = true)

        scope.get("foo") mustEqual Value.Str("rab")
        scope.exit.get("foo") mustEqual Value.Str("bar")
      }
    }

    "value" - {

      "must flatten scopes into an object value" in {

        val result = Frame.empty
          .set("baz", Value.Str("quux"), resolveUp = false)
          .enter
          .set("foo", Value.Str("bar"), resolveUp = false)

        result.value mustEqual Value.Obj(
          "foo" -> Value.Str("bar"),
          "baz" -> Value.Str("quux")
        )
      }

      "must resolve conflicts by preferring the most specific scope" in {

        val result = Frame.empty
          .set("foo", Value.Str("rab"), resolveUp = false)
          .set("fork", Value.Str("spoon"), resolveUp = false)
          .enter
          .set("foo", Value.Str("bar"), resolveUp = false)

        result.value mustEqual Value.Obj(
          "foo" -> Value.Str("bar"),
          "fork" -> Value.Str("spoon")
        )
      }
    }
  }
}
