package wolfendale.nunjucks.template

import org.scalatest.freespec.AnyFreeSpec

import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.runtime.Value

class ExpressionTagSpec extends AnyFreeSpec with Matchers {

  val environment = new ProvidedEnvironment()

  "evaluating an expression tag" - {

    "must output null as empty" in {

      val result = environment.render("{{null}}")

      result mustEqual ""
    }

    "must output undefined as empty" in {

      val result = environment.render("{{undefined}}")

      result mustEqual ""
    }

    "must output strings unchanged" in {

      val result = environment.render("{{'world'}}")

      result mustEqual "world"
    }

    "must coerce boolean true" in {

      val result = environment.render("{{true}}")

      result mustEqual "true"
    }

    "must coerce boolean false" in {

      val result = environment.render("{{false}}")

      result mustEqual "false"
    }

    "must coerce numerics" in {

      val result = environment.render("{{10}}")

      result mustEqual "10"
    }

    "must output parsed Infinity as undefined" in {
      val result = environment.render("{{Infinity}}")

      result mustEqual ""
    }

    "must output evaluated result of Infinity" in {
      val result = environment.render("{{ 1 / 0 }}")

      result mustEqual "Infinity"
    }

    "must output parsed NaN as undefined" in {
      val result = environment.render("{{NaN}}")

      result mustEqual ""
    }

    "must output evaluated result of NaN" in {
      val result = environment.render("{{ +'asdf' }}")

      result mustEqual "NaN"
    }

    "must output arrays" in {
      val result = environment.render("{{[1,2,3]}}")

      result mustEqual "1,2,3"
    }

    "must output arrays with undefined elements" in {
      val result = environment.render("{{ [1,hello,3] }}")

      result mustEqual "1,,3"
    }

    "must output arrays with Infinity elements" in {
      val result = environment.render("{{ [1,(1/0),3] }}")

      result mustEqual "1,Infinity,3"
    }

    "must output arrays with NaN elements" in {
      val result = environment.render("{{ [1,(+'asdf'),3] }}")

      result mustEqual "1,NaN,3"
    }

    "must output arrays with null elements" in {
      val result = environment.render("{{ [1,null,3] }}")

      result mustEqual "1,,3"
    }

    "must output objects" in {
      val result = environment.render("{{ {key: 'value'} }}")

      result mustEqual "[object Object]"
    }

    "must output an escaped string" in {

      val result = environment.render("{{ '<div/>' }}")

      result mustEqual "&lt;div/&gt;"
    }

    "must allow for multiline expressions" in {

      val result = environment.render("""{{ {
          | foo: 'bar'
          |}.foo }}""".stripMargin)

      result mustEqual "bar"
    }
  }
}
