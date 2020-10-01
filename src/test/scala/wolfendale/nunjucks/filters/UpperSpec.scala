package wolfendale.nunjucks.filters

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment

class UpperSpec extends AnyFreeSpec with Matchers {

  val environment = new ProvidedEnvironment()

  "upper filter" - {

    "must convert a string to uppercase" in {

      environment.render("""{{ "fOObAr" | upper}}""") mustEqual "FOOBAR"
    }

    "must maintain the safe flag" in {

      environment.render("""{{ 'foo<br/>bar' | safe | upper }}""") mustEqual "FOO<BR/>BAR"
    }
  }
}
