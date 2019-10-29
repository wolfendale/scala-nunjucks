package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class TitleSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "title filter" - {

    "must title-case a given string" in {

      environment.render("{{ 'FOO bar   baz' | title }}") mustEqual "Foo Bar   Baz"
    }

    "must maintain the safe flag" in {

      environment.render("""{{ "FOO<br/> bar" | safe | title }}""") mustEqual "Foo<br/> Bar"
    }
  }
}