package wolfendale.nunjucks.globals

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class JoinerSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "joiner" - {

    "must output an empty string the first time it's called" in {

      environment.render("{% set comma = joiner() %}{{ comma() === '' }}") mustEqual "true"
    }

    "must output a separator every time it is called except the first time" in {

      environment.render("{% set comma = joiner() %}{{ comma() }}{{ comma() }}{{ comma() }}") mustEqual ",,"
    }

    "must use the first parameter as the separator" in {

      environment.render("{% set comma = joiner(':') %}{{ comma() }}{{ comma() }}{{ comma() }}") mustEqual "::"
    }
  }
}
