package wolfendale.nunjucks.filters

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment

class TrimSpec extends AnyFreeSpec with Matchers {

  val environment = new ProvidedEnvironment()

  "trim filter" - {

    "must remove whitespace before and after a string" in {

      environment.render("""{{ "  hello  " | trim}}""") mustEqual "hello"
    }

    "maintain the safe flag" in {

      environment.render("""{{ "  foo<br/>bar  " | safe | trim }}""") mustEqual "foo<br/>bar"
    }
  }
}
