package wolfendale.nunjucks

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.expression.runtime.Value

class ScopeSpec extends AnyFreeSpec with Matchers with OptionValues {

  "a scope" - {

    "get" - {

      "must return None from a scope if there is nothing defined for the key in any scope" in {

        val scope = Scope.empty
          .set("foo", Value.Str("bar"))

        scope.get("baz") mustNot be(defined)
      }

      "must return a value from a scope if it exists" in {

        val scope = Scope.empty
          .set("foo", Value.Str("bar"))

        println(scope)

        scope.get("foo") mustEqual Value.Str("bar")
      }

      "must return a value from the child scope even if it exists in the parent scope" in {

        val scope = Scope.empty
          .set("foo", Value.Str("rab"))
          .push
          .set("foo", Value.Str("bar"))

        scope.get("foo") mustEqual Value.Str("bar")
      }

      "must return a value from the parent scope if it does not exist in child scope" in {

        val scope = Scope.empty
          .set("foo", Value.Str("rab"))
          .push
          .set("bar", Value.Str("baz"))

        scope.get("foo") mustEqual Value.Str("rab")
      }
    }

    "set" - {

      "must set a value on the most specific scope" in {

        val result = Scope.empty.push
          .set("foo", Value.Str("bar"))
          .pop
          .get("foo")

        result mustNot be(defined)
      }

      "must modify a value on the correct scope when resolveUp is true" ignore {

        val scope = Scope.empty
          .set("foo", Value.Str("bar"), resolveUp = false)
          .push
          .set("foo", Value.Str("rab"), resolveUp = true)

        scope.get("foo") mustEqual Value.Str("rab")
        scope.pop.get("foo") mustEqual Value.Str("rab")
      }
    }
  }
}
