package wolfendale.nunjucks.expression

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.runtime.Value

class FilterCallSpec extends FreeSpec with MustMatchers {

  val tester = new ExpressionTester()

  "a filter call" - {

    "without extra arguments must evaluate properly" in {

      tester.evaluate("'ASDF' | lower") mustEqual Value.Str("asdf")
    }

    "with extra arguments must evaluate properly" in {

      tester.evaluate("[1, 2, 3, 4] | batch(2)") mustEqual Value.Arr(
        Seq(
          Value.Arr(Seq(Value.Number(1), Value.Number(2))),
          Value.Arr(Seq(Value.Number(3), Value.Number(4)))
        ))
    }

    "with superfluous arguments must evaluate properly" in {

      tester.evaluate("'ASDF' | lower(1, true, 'foo')") mustEqual Value.Str("asdf")
    }

    "with chained calls must evaluate properly" in {

      tester.evaluate("'  foo  ' | trim | upper") mustEqual Value.Str("FOO")
    }

    "must evaluate a conditional filter when the condition is true" in {

      tester.evaluate("-5 | abs if true") mustEqual Value.Number(5)
    }

    "must return undefined when a filter condition is false" in {

      tester.evaluate("-5 | abs if false") mustEqual Value.Undefined
    }

    "must return the default when a filter condition is false" in {

      tester.evaluate("-5 | abs if false else 'foo'") mustEqual Value.Str("foo")
    }
  }
}
