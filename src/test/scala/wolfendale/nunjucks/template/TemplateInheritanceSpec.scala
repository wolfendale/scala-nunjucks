package wolfendale.nunjucks.template

import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.runtime.Value

class TemplateInheritanceSpec extends FreeSpec with MustMatchers with OptionValues {

  val environment = new ProvidedEnvironment()

  "a template inheriting another template" - {

    "must output the contents of the parent template" in {

      val env = environment
        .add("parent.njk", "Hello!")

      env.render("{% extends 'parent.njk' %} Hi there!") mustEqual "Hello!"
      env.render("Hi there! {% extends 'parent.njk' %}") mustEqual "Hello!"
    }

    "must conditionally extend a template" in {

      val env = environment
        .add("one.njk", "Hello!")
        .add("two.njk", "Hi there!")

      val template = "{% if foo %}{% extends 'one.njk' %}{% else %}{% extends 'two.njk' %}{% endif %}"

      env.render(template, Value.Obj("foo" -> Value.True)) mustEqual "Hello!"
      env.render(template, Value.Obj("foo" -> Value.False)) mustEqual "Hi there!"
    }

    "must fall back to a root template" in {

      val env = environment
        .add("one.njk", "Hello!")

      val template = "{% if foo %}{% extends 'one.njk' %}{% endif %}Hey there!"

      env.render(template, Value.Obj("foo" -> Value.False)) mustEqual "Hey there!"
    }

    "must override blocks in the parent template" in {

      val env = environment
        .add("parent.njk", "Hello, {% block name %}Noone{% endblock %}!")

      env.render("{% extends 'parent.njk' %}{% block name %}World{% endblock %}") mustEqual "Hello, World!"
    }

    "must allow super calls from blocks" in {

      val env = environment
        .add("parent.njk", "Hello, {% block name %}Noone{% endblock %}!")

      env.render("{% extends 'parent.njk' %}{% block name %}{{ super() }}{% endblock %}") mustEqual "Hello, Noone!"
    }

    "must throw an exception on super call from a root template" in {

      assertThrows[RuntimeException] {
        environment.render("{% block foo %}{{ super() }}{% endblock %}")
      }
    }

    "must allow template hierarchies" in {

      val env = environment
        .add("a.njk", "{% block foo %}A{% endblock %}")
        .add("b.njk", "{% extends 'a.njk' %}{% block foo %}{{ super() }}B{% endblock %}")
        .add("c.njk", "{% extends 'b.njk' %}{% block foo %}{{ super() }}C{% endblock %}")

      env.renderTemplate("c.njk").value mustEqual "ABC"
    }

    "must allow macros defined in blocks to be overridden by child blocks" in {

      val env = environment
        .add("parent.njk", "{% block a %}{% macro foo() %}foobar{% endmacro %}{% endblock %}{% block b %}{{ foo() }}{% endblock %}")
        .add("index.njk", "{% extends 'parent.njk' %}{% block a %}{% macro foo() %}barfoo{% endmacro %}{% endblock %}")

      env.renderTemplate("index.njk").value mustEqual "barfoo"
    }

    "must not leak super" in {

      environment.render("{% block foo %}{% endblock %}{{ super === undefined }}") mustEqual "true"
    }

    "must not let super see vars from the child block" in {

      val env = environment
        .add("parent.njk", "{% block a %}{{ foo }}{% endblock %}")
        .add("index.njk", "{% extends 'parent.njk' %}{% block a %}{% set foo = 'foo' %}{{ super() }}{% endblock %}")

      env.renderTemplate("index.njk").value mustEqual ""
    }
  }

  "a root template" - {

    "must not allow variables to be visible between blocks" in {

      environment.render("{% block a %}{% set foo = 'bar' %}{% endblock %}{% block b %}{{ foo }}{% endblock %}") mustEqual ""
    }

    "must allow macros to be shared between blocks" in {

      environment.render("{% block a %}{% macro foo() %}foobar{% endmacro %}{% endblock %}{% block b %}{{ foo() }}{% endblock %}") mustEqual "foobar"
    }
  }
}
