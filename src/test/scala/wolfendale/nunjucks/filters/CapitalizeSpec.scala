package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class CapitalizeSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "lower filter" - {

    "must convert a string to lowercase" in {

      environment.render("""{{ "fOObAr" | lower}}""") mustEqual "foobar"
    }
  }
}
