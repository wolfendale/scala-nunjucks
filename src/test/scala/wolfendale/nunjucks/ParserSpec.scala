package wolfendale.nunjucks

import fastparse._
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import wolfendale.nunjucks.expression.Parser
import wolfendale.nunjucks.expression.runtime.Value

class ParserSpec extends FreeSpec with MustMatchers with OptionValues {

  "javascript parser" - {

    "must parse" in {

      val a = "1 + 1 | foo('dang', foo = 1337)"

      println(parse(a, Parser.expression(_)))
    }
  }

  "template parser" - {

    "must parse" in {

      val a =
        """{% block foo %}
          | a
          |{% endblock %}
        """.stripMargin

      val b =
        """{% extends 'a' %}
          |{% block foo %}
          | b
          | {{ super() }}
          |{% endblock %}
        """.stripMargin

      val c =
        """{% extends 'b' %}
          |{% block foo %}
          | c
          | {{ super() }}
          |{% endblock %}
        """.stripMargin

      val environment = new ProvidedEnvironment(Map.empty)
        .add("a", a)
        .add("b", b)
        .add("c", c)

      val result = environment
        .renderTemplate("c", Value.Obj.empty)
        .value

      println(result)
    }
  }

  "filters" in {

    val a =
      """
        |{% filter upper %}
        |{{ -Infinity | abs }}
        |{% endfilter %}
        |""".stripMargin

    val environment = new ProvidedEnvironment(Map.empty)
      .add("a", a)

    val result = environment.renderTemplate("a", Value.Obj.empty).value

    println(result)
  }

  "whitespace control" in {

    val a =
      """
        |
        |{{- 'foo' }}
        |
        |{{- 'bar' -}}
        |""".stripMargin

    val environment = new ProvidedEnvironment(Map.empty)
      .add("a", a)

    val result = environment.renderTemplate("a", Value.Obj.empty).value

    println(s"!$result!")
  }
}
