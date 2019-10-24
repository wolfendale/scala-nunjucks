package wolfendale.nunjucks.template

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.ProvidedEnvironment

class MacroTagSpec extends FreeSpec with MustMatchers {

  val environment = new ProvidedEnvironment()

  "a macro" - {

    "must not output anything when it's defined" in {

      environment.render("{% macro foo() %}foobar{% endmacro %}") mustEqual ""
    }

    "must not modify the context when it's defined" in {

      val result = environment.render("{% set x = 1 %}{% macro foo() %}{% set x = 2 %}{% endmacro %}{{ x }}")

      result mustEqual "1"
    }

    "must not modify the calling scope" in {

      val result = environment.render("{% set x = 1 %}{% macro foo() %}{% set x = 2 %}{% endmacro %}{{ foo() }}{{ x }}")

      result mustEqual "1"
    }

    "must allow optional args" in {

      val result = environment.render("{% macro foo(bar) %}{% if bar %}{{ bar }}{% else %}Nope{% endif %}{% endmacro %}{{ foo() }}{{ foo('baz') }}")

      result mustEqual "Nopebaz"
    }

    "must allow for default args" in {

      val result = environment.render("{% macro foo(bar = 'Nope') %}{{ bar }}{% endmacro %}{{ foo() }}{{ foo('baz') }}")

      result mustEqual "Nopebaz"
    }

    "must allow for keyword args" in {

      val result = environment.render("{% macro foo(bar, baz) %}{{ bar }}{{ baz }}{% endmacro %}{{ foo(baz=1, bar=2) }}")

      result mustEqual "21"
    }

    "must not make nested macros available to the global scope until the outer macro is executed" in {

      val result = environment.render("{% macro foo() %}{% macro bar() %}in bar{% endmacro %}{% endmacro %}{{ bar === undefined }}{{ foo() }}{{ bar === undefined }}")

      result mustEqual "truefalse"
    }

    "must make nested macros available to the global scope once the outer macro is executed" in {

      val result = environment.render("{% macro foo() %}{% macro bar() %}in bar {% endmacro %}{{ bar() | upper }}{% endmacro %}{{ foo() }}{{ bar() }}")

      result mustEqual "IN BAR in bar "
    }

    "must see variables from the global scope" in {

      val result = environment.render("{% macro foo() %}{{ var }}{% endmacro %}{% set var = 1 %}{{ foo() }}")

      result mustEqual "1"
    }

    "must not see variables from caller scope" in {

      val result = environment.render("{% set var = 2 %}{% macro foo() %}{{ var }}{% endmacro %}{% for i in [1] %}{% set var = 1 %}{{ foo() }}{% endfor %}")

      result mustEqual "2"
    }

    "must not be autoescaped when it's called" in {

      val result = environment.render("{% macro foo() %}<div/>{% endmacro %}{{ foo() }}")

      result mustEqual "<div/>"
    }

    "must be able to be called recursively" in {

      val result = environment.render("{% macro fib(num) %}{% if num < 10 %}{{ num }}{{ fib(num + 1) }}{% endif %}{% endmacro %}{{ fib(1) }}")

      result mustEqual "123456789"
    }
  }

  "a call block" - {

    "must call a macro" in {

      val result = environment.render("{% macro foo() %}{{ caller() }}{% endmacro %}{% call foo() %}foobar{% endcall %}")

      result mustEqual "foobar"
    }

    "must call a macro with arguments" in {

      val result = environment.render("{% macro foo(bar) %}{{ caller() }}{{ bar }}{% endmacro %}{% call foo('baz') %}foobar{% endcall %}")

      result mustEqual "foobarbaz"
    }

    "must call a macro from an import" in {

      val env = environment
        .add("import.njk", "{% macro foo() %}foobar{% endmacro %}")

      val result = env.render("{% import 'import.njk' as imp %}{% call imp.foo() %}{% endcall %}")

      result mustEqual "foobar"
    }

    "must be able to be called with args" in {

      val template =
        "{% macro list(items) %}" +
          "<ul>{% for i in items %}" +
          "<li>{{ caller(i) }}</li>" +
          "{% endfor %}</ul>" +
          "{% endmacro %}" +
          "{% call(item) list([\"a\", \"b\"]) %}{{ item }}{% endcall %}"

      environment.render(template) mustEqual "<ul><li>a</li><li>b</li></ul>"
    }

    "must be able to access scope from outside the call tag (global scope)" in {

      environment.render("{% macro foo() %}{{ caller() }}{% endmacro %}{% set x = 1 %}{% call foo() %}{{ x }}{% endcall %}") mustEqual "1"
    }

    "must be able to access scope from outside the call tag" in {

      environment.render("{% macro foo() %}{{ caller() }}{% endmacro %}{% for i in [1] %}{% set x = 1 %}{% call foo() %}{{ x }}{% endcall %}{% endfor %}") mustEqual "1"
    }

    "must not leak caller" in {

      environment.render("{% macro foo() %}{% endmacro %}{% call foo() %}foobar{% endcall %}{{ caller === undefined }}") mustEqual "true"
    }
  }
}
