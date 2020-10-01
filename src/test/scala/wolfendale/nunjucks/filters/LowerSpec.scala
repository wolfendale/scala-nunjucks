package wolfendale.nunjucks.filters

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment

class LowerSpec extends AnyFreeSpec with Matchers {

  val environment = new ProvidedEnvironment()

  "lower filter" - {

    "must convert a string to lowercase" in {

      environment.render("""{{ "fOObAr" | lower}}""") mustEqual "foobar"
    }

    "must maintain the safe flag" in {

      environment.render("""{{ "fOO<br/>bAr" | safe | lower }}""") mustEqual "foo<br/>bar"
    }
  }
}
