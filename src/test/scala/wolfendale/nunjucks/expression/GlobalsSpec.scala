package wolfendale.nunjucks.expression

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.runtime.Value

class GlobalsSpec extends AnyFreeSpec with Matchers {

  "global variables" - {

    "must be available in rendered templates" in {

      val environment = new ProvidedEnvironment(globals = Map("foo" -> Value.Str("bar")))

      environment.render("{{ foo }}") mustEqual "bar"
    }
  }
}
