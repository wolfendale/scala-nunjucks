package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class UpperSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "upper filter" - {

    "must convert a string to uppercase" in {

      environment.render("""{{ "fOObAr" | upper}}""") mustEqual "FOOBAR"
    }
  }
}
