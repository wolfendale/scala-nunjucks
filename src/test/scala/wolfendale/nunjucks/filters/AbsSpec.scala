package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value

class AbsSpec extends FreeSpec with MustMatchers {

  val tester = new ExpressionTester()

  "abs filter" - {

    "must make a negative number positive" in {

      tester.evaluate("-5 | abs") mustEqual Value.Number(5)
    }

    "must leave a positive number positive" in {

      tester.evaluate("5 | abs") mustEqual Value.Number(5)
    }

    "must make negative infinity positive" in {

      tester.evaluate("-(1/0) | abs") mustEqual Value.Infinity
    }

    "must leave infinity positive" in {

      tester.evaluate("(1/0) | abs") mustEqual Value.Infinity
    }

    "must leave NaN as NaN" in {

      tester.evaluate("(+'asdf') | abs") mustEqual Value.NaN
    }
  }
}
