package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class AbsSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "abs filter" - {

    "must make a negative number positive" in {

      environment.render("{{ -5 | abs }}") mustEqual "5"
    }

    "must leave a positive number positive" in {

      environment.render("{{ 5 | abs }}") mustEqual "5"
    }

    "must make negative infinity positive" in {

      environment.render("{{ -Infinity | abs }}") mustEqual "Infinity"
    }

    "must leave infinity positive" in {

      environment.render("{{ Infinity | abs }}") mustEqual "Infinity"
    }

    "must leave NaN as NaN" in {

      environment.render("{{ NaN | abs }}") mustEqual "NaN"
    }
  }
}
