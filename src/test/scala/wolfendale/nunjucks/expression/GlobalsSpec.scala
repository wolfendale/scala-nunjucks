package wolfendale.nunjucks.expression

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.runtime.Value

class GlobalsSpec extends FreeSpec with MustMatchers {

  "global variables" - {

    "must be available in rendered templates" in {

      val environment = new ProvidedEnvironment(globals = Map("foo" -> Value.Str("bar")))

      environment.render("{{ foo }}") mustEqual "bar"
    }
  }
}
