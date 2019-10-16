package wolfendale.nunjucks.template

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.runtime.Value

class ForTagSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "a for tag" - {

    "must output nothing when the iterator is empty" in {

      val result = environment.render("{% for i in [] %}foobar{{ i }}{% endfor %}")

      result mustEqual ""
    }

    "must output nothing when the iterator is undefined" in {

      val result = environment.render("{% for i in undefined %}foobar{{ i }}{% endfor %}")

      result mustEqual ""
    }

    "must iterate an array" in {

      val result = environment.render("{% for i in [1, 2, 3] %}{{ i }}{% endfor %}")

      result mustEqual "123"
    }

    "must iterate an object" in {

      val result = environment.render("{% for k, v in { foo: 'bar', baz: 'quux' } %}{{ k }}: {{ v }}, {% endfor %}")

      result mustEqual "foo: bar, baz: quux, "
    }

    "must iterate a string" in {

      val result = environment.render("{% for i in 'foobar' %}{{ i }}{% endfor %}")

      result mustEqual "foobar"
    }

    // TODO fix issue parsing nested array literal
    "must destructure nested arrays" in {

      val result = environment.render("{% for i, j in [[1, 2], [3, 4]] %}{{ i + j }}{% endfor %}")

      result mustEqual "37"
    }

    "must ignore an else case when the iterator has values" in {

      val result = environment.render("{% for i in [1, 2, 3] %}{{ i }}{% else %}foobar{% endfor %}")

      result mustEqual "123"
    }

    "must output an else clause when the iterator is empty" in {

      val result = environment.render("{% for i in [] %}foobar{{ i }}{% else %}spoon{% endfor %}")

      result mustEqual "spoon"
    }

    "must output an else clause when the iterator is undefined" in {

      val result = environment.render("{% for i in undefined %}foobar{{ i }}{% else %}spoon{% endfor %}")

      result mustEqual "spoon"
    }

    "must support loop.index" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.index }}{% endfor %}") mustEqual "123"
    }

    "must support loop.index0" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.index0 }}{% endfor %}") mustEqual "012"
    }

    "must support loop.revindex" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.revindex }}{% endfor %}") mustEqual "321"
    }

    "must support loop.revindex0" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.revindex0 }}{% endfor %}") mustEqual "210"
    }

    "must support loop.first" in {

      val template = "{% for i in [7,3,6] %}" +
        "{% if loop.first %}{{ i }}{% endif %}" +
        "{% endfor %}"

      environment.render(template) mustEqual "7"
    }

    "must support loop.last" in {

      val template = "{% for i in [7,3,6] %}" +
        "{% if loop.last %}{{ i }}{% endif %}" +
        "{% endfor %}"

      environment.render(template) mustEqual "6"
    }

    "must support loop.length" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.length }}{% endfor %}") mustEqual "333"
    }

    "must scope variables correctly" in {

      val result = environment.render("""{% set foo = 2 %}{% for i in [1] %}{% set foo=1 %}{% endfor %}{{ foo }}
                                        |{% for i in [1] %}{% set x=1 %}{{ x }}{% endfor %}{{ x }}
                                        |{% set x=3 %}{{ x }}""".stripMargin, Value.Obj(
        "x" -> Value.Number(2)
      ))

      result mustEqual
        """1
          |12
          |3""".stripMargin
    }
  }
}
