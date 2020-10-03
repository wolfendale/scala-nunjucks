package wolfendale.nunjucks.filters

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment

class CapitalizeSpec extends AnyFreeSpec with Matchers {

  val environment = new ProvidedEnvironment()

  "capitalize filter" - {

    "must capitalize a string" in {

      environment.render("""{{ "fOObAr" | capitalize }}""") mustEqual "Foobar"
    }

    "must maintain the safe flag" in {

      environment.render("""{{ 'foo<br/>bar' | safe | capitalize }}""") mustEqual "Foo<br/>bar"
    }
  }
}
