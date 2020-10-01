package wolfendale.nunjucks.filters

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment

class SafeSpec extends AnyFreeSpec with Matchers {

  val environment = new ProvidedEnvironment()

  "safe filter" - {

    "must mark a string as safe" in {

      environment.render("{{ '<div/>' | safe }}") mustEqual "<div/>"
    }

    "must coerce non-strings and escape them" in {

      environment.render("{{ 1 + (1 | safe) }}") mustEqual "11"
    }
  }
}
