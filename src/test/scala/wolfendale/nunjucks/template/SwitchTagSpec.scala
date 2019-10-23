package wolfendale.nunjucks.template

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.runtime.Value

class SwitchTagSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "a switch tag" - {

    "must drop through to the default when provided with no value in the context to match" in {
      environment.render(
        "{% switch foo %}{% case \"bar\" %}BAR{% case \"baz\" %}BAZ{% default %}NEITHER FOO NOR BAR{% endswitch %}"
      ) mustEqual "NEITHER FOO NOR BAR"
    }

    "must match first case if provided in the context" in {
      environment.render(
        "{% switch foo %}{% case \"bar\" %}BAR{% case \"baz\" %}BAZ{% default %}NEITHER FOO NOR BAR{% endswitch %}",
        Value.Obj(
          "foo" -> Value.Str("bar")
        )) mustEqual "BAR"
    }

    "must match second case if provided in the context" in {
      environment.render(
        "{% switch foo %}{% case \"bar\" %}BAR{% case \"baz\" %}BAZ{% default %}NEITHER FOO NOR BAR{% endswitch %}",
        Value.Obj(
          "foo" -> Value.Str("baz")
        )) mustEqual "BAZ"
    }

    "must provide no content when a default is missing and no context supplied" in {
      environment.render(
        "{% switch foo %}{% case \"bar\" %}BAR{% case \"baz\" %}BAZ{% endswitch %}"
      ) mustEqual ""
    }

    "must fall through when no content supplied for first case" in {
      environment.render(
        "{% switch foo %}{% case \"bar\" %}{% case \"baz\" %}BAR{% endswitch %}",
        Value.Obj(
          "foo" -> Value.Str("bar")
      )) mustEqual "BAR"
    }

    "must match correctly when no content supplied for case above" in {
      environment.render(
        "{% switch foo %}{% case \"bar\" %}{% case \"baz\" %}BAR{% endswitch %}",
        Value.Obj(
          "foo" -> Value.Str("baz")
        )) mustEqual "BAR"
    }
  }
}