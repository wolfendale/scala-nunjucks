package wolfendale.nunjucks.template

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment

class IfTagSpec extends AnyFreeSpec with Matchers {

  val environment = new ProvidedEnvironment()

  "an if tag" - {

    "must output the contents of the tag when the condition is true" in {

      environment.render("""{% if true %}foobar{% endif %}""") mustEqual "foobar"
    }

    "must not output the contents of the tag when the condition is false" in {

      environment.render("""{% if false %}foobar{% endif %}""") mustEqual ""
    }

    "must output the contents of the else tag when the condition is false" in {

      environment.render("""{% if false %}foo{% else %}bar{% endif %}""") mustEqual "bar"
    }

    "must not output the contents of the else tag when the condition is true" in {

      environment.render("""{% if true %}foo{% else %}bar{% endif %}""") mustEqual "foo"
    }

    "must output the contents of the tag when the condition is truthy" in {

      environment.render("""{% if 1 %}foobar{% endif %}""") mustEqual "foobar"
      environment.render("""{% if 'foo' %}foobar{% endif %}""") mustEqual "foobar"
    }

    "must not output the contents of the tag when the condition is falsy" in {

      environment.render("""{% if 0 %}foobar{% endif %}""") mustEqual ""
      environment.render("""{% if '' %}foobar{% endif %}""") mustEqual ""
    }
  }
}
