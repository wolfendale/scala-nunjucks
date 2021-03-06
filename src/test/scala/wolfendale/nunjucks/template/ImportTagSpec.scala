package wolfendale.nunjucks.template

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.ProvidedEnvironment

class ImportTagSpec extends AnyFreeSpec with Matchers with OptionValues {

  val environment = new ProvidedEnvironment()

  "an import tag" - {

    "must import values from the imported template" in {

      val env = environment
        .add("import.njk", "{% set foo = 'bar' %}")
        .add("test.njk", "{% import 'import.njk' as imp %}{{ imp.foo }}")

      env.renderTemplate("test.njk").value mustEqual "bar"
    }

    "must import values from the imported template with context" in {

      val env = environment
        .add("import.njk", "{% set foo = foo ~ 'bar' %}")
        .add("test.njk", "{% set foo = 'quux' %}{% import 'import.njk' as imp with context %}{{ imp.foo }}")

      env.renderTemplate("test.njk").value mustEqual "quuxbar"
    }

    "must import values from the imported template without context" in {

      val env = environment
        .add("import.njk", "{% set foo = foo ~ 'bar' %}")
        .add("test.njk", "{% set foo = 'quux' %}{% import 'import.njk' as imp without context %}{{ imp.foo }}")

      env.renderTemplate("test.njk").value mustEqual "undefinedbar"
    }

    "must import values from the imported template without context by default" in {

      val env = environment
        .add("import.njk", "{% set foo = foo ~ 'bar' %}")
        .add("test.njk", "{% set foo = 'quux' %}{% import 'import.njk' as imp %}{{ imp.foo }}")

      env.renderTemplate("test.njk").value mustEqual "undefinedbar"
    }

    "must modify the calling context when an importing with context" in {

      val env = environment
        .add("import.njk", "{% set foo = 'bar' %}")
        .add("test.njk", "{% set foo = 'foo' %}{% import 'import.njk' as imp with context %}{{ foo }}{{ imp.foo }}")

      env.renderTemplate("test.njk").value mustEqual "barbar"
    }

    "must not modify the calling context when importing without context" in {

      val env = environment
        .add("import.njk", "{% set foo = 'bar' %}")
        .add("test.njk", "{% set foo = 'foo' %}{% import 'import.njk' as imp without context %}{{ foo }}{{ imp.foo }}")

      env.renderTemplate("test.njk").value mustEqual "foobar"
    }

    "must fail when attempting to import a template which doesn't exist" in {

      assertThrows[RuntimeException] {
        environment.render("{% import 'foo.njk' as foo %}")
      }
    }
  }

  "a from tag" - {

    "must import values from the imported template" in {

      val env = environment
        .add("import.njk", "{% set foo = 'bar' %}")
        .add("test.njk", "{% from 'import.njk' import foo %}{{ foo }}")

      env.renderTemplate("test.njk").value mustEqual "bar"
    }

    "must import values from the imported template with context" in {

      val env = environment
        .add("import.njk", "{% set foo = foo ~ 'bar' %}")
        .add("test.njk", "{% set foo = 'quux' %}{% from 'import.njk' import foo with context %}{{ foo }}")

      env.renderTemplate("test.njk").value mustEqual "quuxbar"
    }

    "must import values from the imported template without context" in {

      val env = environment
        .add("import.njk", "{% set foo = foo ~ 'bar' %}")
        .add("test.njk", "{% set foo = 'quux' %}{% from 'import.njk' import foo without context %}{{ foo }}")

      env.renderTemplate("test.njk").value mustEqual "undefinedbar"
    }

    "must import values from the imported template without context by default" in {

      val env = environment
        .add("import.njk", "{% set foo = foo ~ 'bar' %}")
        .add("test.njk", "{% set foo = 'quux' %}{% from 'import.njk' import foo %}{{ foo }}")

      env.renderTemplate("test.njk").value mustEqual "undefinedbar"
    }

    "must import values from the imported template aliased" in {

      val env = environment
        .add("import.njk", "{% set foo = 1 %}{% set bar = 2 %}")
        .add("test.njk", "{% from 'import.njk' import foo, bar as baz %}{{ foo }}{{ baz }}")

      env.renderTemplate("test.njk").value mustEqual "12"
    }

    "must modify the calling context when an importing with context" in {

      val env = environment
        .add("import.njk", "{% set foo = 'bar' %}")
        .add("test.njk",
             "{% set foo = 'foo' %}{% from 'import.njk' import foo as bar with context %}{{ foo }}{{ bar }}")

      env.renderTemplate("test.njk").value mustEqual "barbar"
    }

    "must not modify the calling context when importing without context" in {

      val env = environment
        .add("import.njk", "{% set foo = 'bar' %}")
        .add("test.njk",
             "{% set foo = 'foo' %}{% from 'import.njk' import foo as bar without context %}{{ foo }}{{ bar }}")

      env.renderTemplate("test.njk").value mustEqual "foobar"
    }

    "must fail when attempting to import a template which doesn't exist" in {

      assertThrows[RuntimeException] {
        environment.render("{% from 'foo.njk' import foo %}")
      }
    }

    "must fail when attempting to import a value which doesn't exist in a template" in {

      val env = environment
        .add("import.njk", "{% set foo = 'bar' %}")

      assertThrows[RuntimeException] {
        env.render("{% from 'import.njk' import bar %}")
      }
    }
  }
}
