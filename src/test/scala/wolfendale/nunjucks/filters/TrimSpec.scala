package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class TrimSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "trim filter" - {

    "must remove whitespace before and after a string" in {

      environment.render("""{{ "  hello  " | trim}}""") mustEqual "hello"
    }
  }
}
