package wolfendale.nunjucks

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.runtime.Value

class ContextSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment(
    globals = Map("foo" -> Value.Str("global"))
  )

  "context" - {

    "should resolve a variable from a frame if it exists" in {

      environment.render("{% set foo = 'context' %}{% for i in [1] %}{% set foo = 'frame' %}{{ foo }}{% endfor %}") mustEqual "frame"
    }

    "should resolve a variable from the context" in {

      environment.render("{% set foo = 'context' %}{{ foo }}") mustEqual "context"
    }

    "should resolve a variable from globals" in {

      environment.render("{{ foo }}") mustEqual "global"
    }
  }
}
