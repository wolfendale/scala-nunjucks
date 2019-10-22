package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class CapitalizeSpec extends FreeSpec with MustMatchers {

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
