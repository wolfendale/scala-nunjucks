package wolfendale.nunjucks.template

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class MacroTagSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "a macro" - {

    "must not be autoescaped when it's called" in {

      val result = environment.render("{% macro foo() %}<div/>{% endmacro %}{{ foo() }}")

      result mustEqual "<div/>"
    }
  }
}
