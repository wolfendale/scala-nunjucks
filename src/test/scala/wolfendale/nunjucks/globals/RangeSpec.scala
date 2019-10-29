package wolfendale.nunjucks.globals

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class RangeSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "range" - {
    "must iterate through range until it reaches stop" in {
      environment.render("{% for i in range(0, 5) -%}{{ i }},{%- endfor %}") mustEqual "0,1,2,3,4,"
    }

    "must step through range in steps until it reaches stop" in {
      environment.render("{% for i in range(0, 5, 3) -%}{{ i }},{%- endfor %}") mustEqual "0,3,"
    }

    "must step through range in steps until it reaches stop when stop is not divisible" in {
      environment.render("{% for i in range(0, 5, 3) -%}{{ i }},{%- endfor %}") mustEqual "0,3,"
    }

    "must step through range in steps until it reaches stop when stop is divisible" in {
      environment.render("{% for i in range(0, 6, 3) -%}{{ i }},{%- endfor %}") mustEqual "0,3,"
    }

    "must step through range until stop starting from zero as a default" in {
      environment.render("{% for i in range(6) -%}{{ i }},{%- endfor %}") mustEqual "0,1,2,3,4,5,"
    }
  }
}
