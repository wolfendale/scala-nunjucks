package wolfendale.nunjucks.equivalence

import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import wolfendale.nunjucks.ProvidedEnvironment
import wolfendale.nunjucks.expression.runtime.Value

class CompilerSpec extends FreeSpec with MustMatchers with OptionValues {

  val importNjk = """{% macro foo() %}Here's a macro{% endmacro %}
                    |
                    |{% set bar = 'baz' %}
                    |
                    |{% macro wrap(el) %}<{{ el }}>{{ caller() }}</{{ el }}>{% endmacro %}""".stripMargin

  val importContextSet = """{% set bar = "FOO" %}
                           |
                           |{# create a new scope #}
                           |{% for i in [1] %}
                           |  {% set buzz = "buzz" %}
                           |{% endfor %}""".stripMargin

  val baseInherit = """{% extends "base.njk" %}
                      |
                      |{% block block1 %}*{{ super() }}*{% endblock %}""".stripMargin

  val baseShow = """{% block main %}{{ var }}{% endblock %}""".stripMargin

  val environment = new ProvidedEnvironment()
    .add("item.njk", "showing {{ item }}")
    .add("base.njk", "Foo{% block block1 %}Bar{% endblock %}{% block block2 %}Baz{% endblock %}Fizzle")
    .add("include.njk", "FooInclude {{ name }}")
    .add("import.njk", importNjk)
    .add("import-context.njk", "{% macro foo() %}Here's {{ bar }}{% endmacro %}")
    .add("import-context-set.njk", importContextSet)
    .add("base-inherit.njk", baseInherit)
    .add("base-show.njk", baseShow)
    .add("base-set.njk", "{% set var = 'parent' %}{% block main %}{% set var = 'inner' %}{% endblock %}")
    .add("base-set-inside-block.njk", "{% block main %}{% set var = 'inner' %}{% endblock %}")
    .add("base-set-and-show.njk", "{% set var = 'parent' %}{% block main %}{% set var = 'inner' %}{% endblock %}{{ var }}")
    .add("includeMany.njk", "{% include \"include.njk\" %}\n" * 131)
    .add("include-set.njk", "{{ var }}{% set var = 2 %}{{ var }}\n")
    .add("include-in-loop.njk", "{{ loop.index }},{{ loop.index0 }},{{ loop.first }}\n")
    .add("base2.njk", "{% for item in [1,2] %}{% block item %}{{ item }}{% endblock %}{% endfor %}")
    .add("set.njk", "{% set foo = \"mumble\" %}")
    .add("base-set-wraps-block.njk", "{% set somevar %}{% block somevar %}{% endblock %}{% endset %}{{ somevar }}\n")
    .add("base3.njk", "{% block block1 %}<b>Foo</b>{% endblock %}")
    .add("undefined-macro.njk", "{{ undef() }}")
    .add("filter-block.njk", "may the {% filter replace(\"force\", \"forth\") %}{% block block1 %}bar{% endblock %}{% endfilter %} be with you\n")

  "compiler" - {

    "should compile templates" in {

      environment.render("Hello world") mustEqual "Hello world"

      environment.render("Hello world, {{ name }}", Value.Obj(
        "name" -> Value.Str("James"))
      ) mustEqual "Hello world, James"

      environment.render("Hello world, {{name}}{{suffix}}, how are you", Value.Obj(
        "name"   -> Value.Str("James"),
        "suffix" -> Value.Str(" Long")
      )) mustEqual "Hello world, James Long, how are you"
    }

    "should escape newlines" in {

      environment.render("foo\\nbar") mustEqual "foo\\nbar"
    }

    "should escape Unicode line separators" in {

      environment.render("\u2028") mustEqual "\u2028"
    }

    "should compile references" in {

      environment.render("{{ foo.bar }}", Value.Obj(
        "foo" -> Value.Obj(
          "bar" -> Value.Str("baz")
        )
      )) mustEqual "baz"

      environment.render("{{ foo[\"bar\"] }}", Value.Obj(
        "foo" -> Value.Obj(
          "bar" -> Value.Str("baz")
        )
      )) mustEqual "baz"
    }

    "should fail silently on undefined values" in {

      environment.render("{{ foo }}") mustEqual ""
      environment.render("{{ foo.bar }}") mustEqual ""
      environment.render("{{ foo.bar.baz }}") mustEqual ""
      environment.render("{{ foo.bar.baz[\"biz\"].mumble }}") mustEqual ""
    }

    "should not treat falsy values the same as undefined" in {

      environment.render("{{ foo }}", Value.Obj(
        "foo" -> Value.Number(0)
      )) mustEqual "0"

      environment.render("{{ foo }}", Value.Obj(
        "foo" -> Value.False
      )) mustEqual "false"
    }

    "should display none as empty string" in {

      environment.render("{{ none }}") mustEqual ""
    }

    "should compile none as falsy" in {

      environment.render("{% if not none %}yes{% endif %}") mustEqual "yes"
    }

    "should compile none as null not undefined" in {

      environment.render("{{ none|default(\"d\", false) }}") mustEqual ""
    }

    "should compile function calls" in {

      val fn = Value.Function {
        case (_, args) =>
          Value.Str(args.get(0).map(_.toStr.value).getOrElse("") + "hi")
      }

      environment.render("{{ foo(\"msg\") }}", Value.Obj(
        "foo" -> fn
      )) mustEqual "msghi"
    }

    "should compile switch statements" in {

      val template1 = "{% switch foo %}{% case \"bar\" %}BAR{% case \"baz\" %}BAZ{% default %}NEITHER FOO NOR BAR{% endswitch %}"
      val template2 = "{% switch foo %}{% case \"bar\" %}BAR{% case \"baz\" %}BAZ{% endswitch %}"
      val template3 = "{% switch foo %}{% case \"bar\" %}{% case \"baz\" %}BAR{% endswitch %}"

      environment.render(template1) mustEqual "NEITHER FOO NOR BAR"

      environment.render(template1, Value.Obj(
        "foo" -> Value.Str("bar")
      )) mustEqual "BAR"

      environment.render(template1, Value.Obj(
        "foo" -> Value.Str("baz")
      )) mustEqual "BAZ"

      environment.render(template2) mustEqual ""

      environment.render(template3, Value.Obj(
        "foo" -> Value.Str("bar")
      )) mustEqual "BAR"

      environment.render(template3, Value.Obj(
        "foo" -> Value.Str("baz")
      )) mustEqual "BAR"
    }

    "should compile if blocks" in {

      val template = "Give me some {% if hungry %}pizza{% else %}water{% endif %}"

      environment.render(template, Value.Obj(
        "hungry" -> Value.True
      )) mustEqual "Give me some pizza"

      environment.render(template, Value.Obj(
        "hungry" -> Value.False
      )) mustEqual "Give me some water"

      environment.render("{% if not hungry %}good{% endif %}", Value.Obj(
        "hungry" -> Value.False
      )) mustEqual "good"

      environment.render("{% if hungry and like_pizza %}good{% endif %}", Value.Obj(
        "hungry" -> Value.True,
        "like_pizza" -> Value.True
      )) mustEqual "good"

      environment.render("{% if hungry or like_pizza %}good{% endif %}", Value.Obj(
        "hungry" -> Value.False,
        "like_pizza" -> Value.True
      )) mustEqual "good"

      environment.render("{% if (hungry or like_pizza) and anchovies %}good{% endif %}", Value.Obj(
        "hungry" -> Value.False,
        "like_pizza" -> Value.True,
        "anchovies" -> Value.True
      )) mustEqual "good"

      environment.render("{% if food == \"pizza\" %}pizza{% endif %}{% if food ==\"beer\" %}beer{% endif %}", Value.Obj(
        "food" -> Value.Str("beer")
      )) mustEqual "beer"

      environment.render("{% if \"pizza\" in food %}yum{% endif %}", Value.Obj(
        "food" -> Value.Obj(
          "pizza" -> Value.True
        )
      )) mustEqual "yum"

      environment.render("{% if pizza %}yum{% elif anchovies %}yuck{% endif %}", Value.Obj(
        "pizza" -> Value.True
      )) mustEqual "yum"

      environment.render("{% if pizza %}yum{% elseif anchovies %}yuck{% endif %}", Value.Obj(
        "pizza" -> Value.True
      )) mustEqual "yum"

      environment.render("{% if pizza %}yum{% elif anchovies %}yuck{% endif %}", Value.Obj(
        "anchovies" -> Value.True
      )) mustEqual "yuck"

      environment.render("{% if pizza %}yum{% elseif anchovies %}yuck{% endif %}", Value.Obj(
        "anchovies" -> Value.True
      )) mustEqual "yuck"

      environment.render("{% if topping == \"pepperoni\" %}yum{% elseif topping == \"anchovies\" %}yuck{% else %}hmmm{% endif %}", Value.Obj(
        "topping" -> Value.Str("sausage")
      )) mustEqual "hmmm"
    }

    "should compile the ternary operator" in {

      environment.render("{{ \"foo\" if bar else \"baz\" }}") mustEqual "baz"

      environment.render("{{ \"foo\" if bar else \"baz\" }}", Value.Obj(
        "bar" -> Value.True
      )) mustEqual "foo"
    }

    "should compile inline conditionals" in {

      val template = "Give me some {{ \"pizza\" if hungry else \"water\" }}"

      environment.render(template, Value.Obj(
        "hungry" -> Value.True
      )) mustEqual "Give me some pizza"

      environment.render(template, Value.Obj(
        "hungry" -> Value.False
      )) mustEqual "Give me some water"

      environment.render("{{ \"good\" if not hungry }}", Value.Obj(
        "hungry" -> Value.False
      )) mustEqual "good"

      environment.render("{{ \"good\" if hungry and like_pizza }}", Value.Obj(
        "hungry" -> Value.True,
        "like_pizza" -> Value.True
      )) mustEqual "good"

      environment.render("{{ \"good\" if hungry or like_pizza }}", Value.Obj(
        "hungry" -> Value.False,
        "like_pizza" -> Value.True
      )) mustEqual "good"

      environment.render("{{ \"good\" if (hungry or like_pizza) and anchovies }}", Value.Obj(
        "hungry" -> Value.False,
        "like_pizza" -> Value.True,
        "anchovies" -> Value.True
      )) mustEqual "good"

      environment.render("{{ \"pizza\" if food == \"pizza\" }}{{ \"beer\" if food == \"beer\" }}", Value.Obj(
        "food" -> Value.Str("beer")
      )) mustEqual "beer"
    }
  }

  "the for tag" - {

    "should loop over simple arrays" in {

      environment.render("{% for i in arr %}{{ i }}{% endfor %}", Value.Obj(
        "arr" -> Value.Arr(Seq(Value.Number(1), Value.Number(2), Value.Number(3), Value.Number(4), Value.Number(5)))
      )) mustEqual "12345"
    }

    "should loop normally with an {% else %} tag and non-empty array" in {

      environment.render("{% for i in arr %}{{ i }}{% else %}empty{% endfor %}", Value.Obj(
        "arr" -> Value.Arr(Seq(Value.Number(1), Value.Number(2), Value.Number(3), Value.Number(4), Value.Number(5)))
      )) mustEqual "12345"
    }

    "should execute the {% else %} block when looping over an empty array" in {

      environment.render("{% for i in arr %}{{ i }}{% else %}empty{% endfor %}", Value.Obj(
        "arr" -> Value.Arr.empty
      )) mustEqual "empty"
    }

    "should support destructured looping" in {

      environment.render("{% for a, b, c in arr %}{{ a }},{{ b }},{{ c }}.{% endfor %}", Value.Obj(
        "arr" -> Value.Arr(Seq(
          Value.Arr(Seq(Value.Str("x"), Value.Str("y"), Value.Str("z"))),
          Value.Arr(Seq(Value.Str("1"), Value.Str("2"), Value.Str("3")))
        ))
      )) mustEqual "x,y,z.1,2,3."
    }

    "should do loop over key-values of a literal in-template Object" in {

      environment.render("{% for k, v in { one: 1, two: 2 } %}-{{ k }}:{{ v }}-{% endfor %}") mustEqual "-one:1--two:2-"
    }

    "should support loop.index" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.index }}{% endfor %}") mustEqual "123"
    }

    "should support loop.index0" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.index0 }}{% endfor %}") mustEqual "012"
    }

    "should support loop.revindex" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.revindex }}{% endfor %}") mustEqual "321"
    }

    "should support loop.revindex0" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.revindex0 }}{% endfor %}") mustEqual "210"
    }

    "should support loop.first" in {

      val template = "{% for i in [7,3,6] %}" +
      "{% if loop.first %}{{ i }}{% endif %}" +
      "{% endfor %}"

      environment.render(template) mustEqual "7"
    }

    "should support loop.last" in {

      val template = "{% for i in [7,3,6] %}" +
        "{% if loop.last %}{{ i }}{% endif %}" +
        "{% endfor %}"

      environment.render(template) mustEqual "6"
    }

    "should support loop.length" in {

      environment.render("{% for i in [7,3,6] %}{{ loop.length }}{% endfor %}") mustEqual "333"
    }

    "should fail silently when looping over an undefined variable" in {

      environment.render("{% for i in foo %}{{ i }}{% endfor %}") mustEqual ""
    }

    "should fail silently when looping over a null variable" in {

      environment.render("{% for i in foo %}{{ i }}{% endfor %}", Value.Obj(
        "foo" -> Value.Null
      )) mustEqual ""
    }

    "should loop over two-dimensional arrays" in {

      environment.render("{% for x, y in points %}[{{ x }},{{ y }}]{% endfor %}", Value.Obj(
        "points" -> Value.Arr(Seq(
          Value.Arr(Seq(Value.Number(1), Value.Number(2))),
          Value.Arr(Seq(Value.Number(3), Value.Number(4))),
          Value.Arr(Seq(Value.Number(5), Value.Number(6)))
        ))
      )) mustEqual "[1,2][3,4][5,6]"
    }

    "should loop over four-dimensional arrays" in {

      environment.render("{% for a, b, c, d in arr %}[{{ a }},{{ b }},{{ c }},{{ d }}]{% endfor %}", Value.Obj(
        "arr" -> Value.Arr(Seq(
          Value.Arr(Seq(Value.Number(1), Value.Number(2), Value.Number(3), Value.Number(4))),
          Value.Arr(Seq(Value.Number(5), Value.Number(6), Value.Number(7), Value.Number(8)))
        ))
      )) mustEqual "[1,2,3,4][5,6,7,8]"
    }

    "should support loop.index with two-dimensional loops" in {

      environment.render("{% for x, y in points %}{{ loop.index }}{% endfor %}", Value.Obj(
        "points" -> Value.Arr(Seq(
          Value.Arr(Seq(Value.Number(1), Value.Number(2))),
          Value.Arr(Seq(Value.Number(3), Value.Number(4))),
          Value.Arr(Seq(Value.Number(5), Value.Number(6)))
        ))
      )) mustEqual "123"
    }

    "should support loop.revindex with two-dimensional loops" in {

      environment.render("{% for x, y in points %}{{ loop.revindex }}{% endfor %}", Value.Obj(
        "points" -> Value.Arr(Seq(
          Value.Arr(Seq(Value.Number(1), Value.Number(2))),
          Value.Arr(Seq(Value.Number(3), Value.Number(4))),
          Value.Arr(Seq(Value.Number(5), Value.Number(6)))
        ))
      )) mustEqual "321"
    }

    "should support key-value looping over an Object variable" in {

      environment.render("{% for k, v in items %}({{ k }},{{ v }}){% endfor %}", Value.Obj(
        "items" -> Value.Obj(
          "foo" -> Value.Number(1),
          "bar" -> Value.Number(2)
        )
      )) mustEqual "(foo,1)(bar,2)"
    }

    "should support loop.index when looping over an Object's key-value pairs" in {

      environment.render("{% for k, v in items %}{{ loop.index }}{% endfor %}", Value.Obj(
        "items" -> Value.Obj(
          "foo" -> Value.Number(1),
          "bar" -> Value.Number(2)
        )
      )) mustEqual "12"
    }

    "should support loop.revindex when looping over an Object's key-value pairs" in {

      environment.render("{% for k, v in items %}{{ loop.revindex }}{% endfor %}", Value.Obj(
        "items" -> Value.Obj(
          "foo" -> Value.Number(1),
          "bar" -> Value.Number(2)
        )
      )) mustEqual "21"
    }

    "should support loop.length when looping over an Object's key-value pairs" in {

      environment.render("{% for k, v in items %}{{ loop.length }}{% endfor %}", Value.Obj(
        "items" -> Value.Obj(
          "foo" -> Value.Number(1),
          "bar" -> Value.Number(2)
        )
      )) mustEqual "22"
    }

    "should support include tags in the body of the loop" in {

      environment.render("{% for item, v in items %}{% include \"item.njk\" %}{% endfor %}", Value.Obj(
        "items" -> Value.Obj(
          "foo" -> Value.Number(1),
          "bar" -> Value.Number(2)
        )
      )) mustEqual "showing fooshowing bar"
    }

    "should work with {% set %} and {% include %} tags" in {

      val template = "{% set item = passed_var %}" +
      "{% include \"item.njk\" %}\n" +
      "{% for i in passed_iter %}" +
      "{% set item = i %}" +
      "{% include \"item.njk\" %}\n" +
      "{% endfor %}"

      environment.render(template, Value.Obj(
        "passed_var" -> Value.Str("test"),
        "passed_iter" -> Value.Arr(Seq(
          Value.Str("1"), Value.Str("2"), Value.Str("3")
        ))
      )) mustEqual "showing test\nshowing 1\nshowing 2\nshowing 3\n"
    }
  }

  "should allow overriding var with none inside nested scope" in {

    environment.render("{% set var = \"foo\" %}{% for i in [1] %}{% set var = none %}{{ var }}{% endfor %}") mustEqual ""
  }

  "should compile basic arithmetic operators" in {

    environment.render("{{ 3 + 4 - 5 * 6 / 10 }}") mustEqual "4"
  }

  "should compile the exponentiation (**) operator" in {

    environment.render("{{ 4**5 }}") mustEqual "1024"
  }

  "should compile the integer division (//) operator" in {

    environment.render("{{ 9//5 }}") mustEqual "1"
  }

  "should compile the modulus operator" in {

    environment.render("{{ 9%5 }}") mustEqual "4"
  }

  "should compile numeric negation operator" in {

    environment.render("{{ -5 }}") mustEqual "-5"
  }

  "should compile arrays" in {
    environment.render("{{ [1,2,3] }}") mustEqual "1,2,3"
  }

  "should compile arrays of functions" in {
    environment.render("{{ [1 + 1, 2 * 2, 3 ** 3] }}") mustEqual "2,4,27"
  }

  "should compile comparison operators" in {

    environment.render("{% if 3 < 4 %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if 3 > 4 %}yes{% endif %}") mustEqual ""
    environment.render("{% if 9 >= 10 %}yes{% endif %}") mustEqual ""
    environment.render("{% if 10 >= 10 %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if 9 <= 10 %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if 10 <= 10 %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if 11 <= 10 %}yes{% endif %}") mustEqual ""

    environment.render("{% if 10 != 10 %}yes{% endif %}") mustEqual ""
    environment.render("{% if 10 == 10 %}yes{% endif %}") mustEqual "yes"

    environment.render("{% if \"0\" == 0 %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if \"0\" === 0 %}yes{% endif %}") mustEqual ""
    environment.render("{% if \"0\" !== 0 %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if 0 == false %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if 0 === false %}yes{% endif %}") mustEqual ""

    val fn = Value.Function {
      case (_, args) =>
        args.get(0).map(_.toNumeric).getOrElse(Value.Number(0)) - Value.Number(1)
    }

    environment.render("{% if foo(20) > bar %}yes{% endif %}", Value.Obj(
      "foo" -> fn,
      "bar" -> Value.Number(15)
    )) mustEqual "yes"
  }

  "should compile python-style ternary operators" in {

    environment.render("{{ \"yes\" if 1 is odd else \"no\"  }}") mustEqual "yes"
    environment.render("{{ \"yes\" if 2 is even else \"no\"  }}") mustEqual "yes"
    environment.render("{{ \"yes\" if 2 is odd else \"no\"  }}") mustEqual "no"
    environment.render("{{ \"yes\" if 1 is even else \"no\"  }}") mustEqual "no"
  }

  "should compile the \"in\" operator for Arrays" in {

    environment.render("{% if 1 in [1, 2] %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if 1 in [2, 3] %}yes{% endif %}") mustEqual ""
    environment.render("{% if 1 not in [1, 2] %}yes{% endif %}") mustEqual ""
    environment.render("{% if 1 not in [2, 3] %}yes{% endif %}") mustEqual "yes"
    environment.render("{% if \"a\" in vals %}yes{% endif %}", Value.Obj(
      "vals" -> Value.Arr(Seq(
        Value.Str("a"), Value.Str("b")
      ))
    )) mustEqual "yes"
  }

  "should compile the \"in\" operator for objects" in {

    environment.render("{% if \"a\" in obj %}yes{% endif %}", Value.Obj(
      "obj" -> Value.Obj(
        "a" -> Value.True
      )
    )) mustEqual "yes"

    environment.render("{% if \"a\" in obj %}yes{% endif %}", Value.Obj(
      "obj" -> Value.Obj(
        "b" -> Value.True
      )
    )) mustEqual ""
  }

  "should compile the \"in\" operator for strings" in {

    environment.render("{% if \"foo\" in \"foobar\" %}yes{% endif %}") mustEqual "yes"
  }

  "should throw an error when using the \"in\" operator on unexpected types" in {

    assertThrows[RuntimeException] {
      environment.render("{% if \"a\" in 1 %}yes{% endif %}", Value.Obj.empty)
    }

    assertThrows[RuntimeException] {
      environment.render("{% if \"a\" in obj %}yes{% endif %}", Value.Obj.empty)
    }
  }

  "should compile string concatenations with tilde" in {

    environment.render("{{ 4 ~ 'hello' }}") mustEqual "4hello"
    environment.render("{{ 4 ~ 5 }}") mustEqual "45"
    environment.render("{{ 'a' ~ 'b' ~ 5 }}") mustEqual "ab5"
  }

  "should compile macros" in {

    environment.render("{% macro foo() %}This is a macro{% endmacro %}{{ foo() }}") mustEqual "This is a macro"
  }

  "should compile macros with optional args" in {

    environment.render("{% macro foo(x, y) %}{{ y }}{% endmacro %}{{ foo(1) }}") mustEqual ""
  }

  "should compile macros with args that can be passed to filters" in {

    environment.render("{% macro foo(x) %}{{ x|title }}{% endmacro %}{{ foo(\"foo\") }}") mustEqual "Foo"
  }

  "should compile macros with positional args" in {

    environment.render("{% macro foo(x, y) %}{{ y }}{% endmacro %}{{ foo(1, 2) }}") mustEqual "2"
  }

  "should compile macros with arg defaults" in {

    environment.render("{% macro foo(x, y, z=5) %}{{ y }}{% endmacro %}{{ foo(1, 2) }}") mustEqual "2"
    environment.render("{% macro foo(x, y, z=5) %}{{ z }}{% endmacro %}{{ foo(1, 2) }}") mustEqual "5"
  }

  "should compile macros with keyword args" in {

    environment.render("{% macro foo(x, y, z=5) %}{{ y }}{% endmacro %}{{ foo(1, y=2) }}") mustEqual "2"
  }

  "should compile macros with only keyword args" in {

    environment.render("{% macro foo(x, y, z=5) %}{{ x }}{{ y }}{{ z }}{% endmacro %}{{ foo(x=1, y=2) }}") mustEqual "125"
  }

  "should compile macros with out-of-order keyword args" in {

    environment.render("{% macro foo(x, y=2, z=5) %}{{ x }}{{ y }}{{ z }}{% endmacro %}{{ foo(1, z=3) }}") mustEqual "123"
  }

  "should compile macros (2)" in {

    environment.render("{% macro foo(x, y=2, z=5) %}{{ x }}{{ y }}{{ z }}{% endmacro %}{{ foo(1) }}") mustEqual "125"
  }

  "should compile macros with multiple overridden arg defaults" in {

    environment.render("{% macro foo(x, y=2, z=5) %}{{ x }}{{ y }}{{ z }}{% endmacro %}{{ foo(1, 10, 20) }}") mustEqual "11020"
  }

  "should compile macro calls inside blocks" in {

    val template =
      "{% extends \"base.njk\" %}" +
      "{% macro foo(x, y=2, z=5) %}{{ x }}{{ y }}{{ z }}" +
      "{% endmacro %}" +
      "{% block block1 %}" +
      "{{ foo(1) }}" +
      "{% endblock %}"

    environment.render(template) mustEqual "Foo125BazFizzle"
  }

  "should compile macros defined in one block and called in another" in {

    val template =
      "{% block bar %}" +
      "{% macro foo(x, y=2, z=5) %}{{ x }}{{ y }}{{ z }}" +
      "{% endmacro %}" +
      "{% endblock %}" +
      "{% block baz %}" +
      "{{ foo(1) }}" +
      "{% endblock %}"

    environment.render(template) mustEqual "125"
  }

  "should compile macros that include other templates" in {

    environment.render("{% macro foo() %}{% include \"include.njk\" %}{% endmacro %}{{ foo() }}", Value.Obj(
      "name" -> Value.Str("james")
    )) mustEqual "FooInclude james"
  }

  "should compile macros that set vars" in {

    val template =
      "{% macro foo() %}{% set x = \"foo\"%}{{ x }}{% endmacro %}" +
      "{% set x = \"bar\" %}" +
      "{{ x }}" +
      "{{ foo() }}" +
      "{{ x }}"

    environment.render(template) mustEqual "barfoobar"
  }

  "should not leak variables set in macro to calling scope" in {

    val template =
      "{% macro setFoo() %}" +
      "{% set x = \"foo\" %}" +
      "{{ x }}" +
      "{% endmacro %}" +
      "{% macro display() %}" +
      "{% set x = \"bar\" %}" +
      "{{ setFoo() }}" +
      "{{ x }}" +
      "{% endmacro %}" +
      "{{ display() }}"

    environment.render(template) mustEqual "foobar"
  }

  "should not leak variables set in nested scope within macro out to calling scope" in {

    val template =
      "{% macro setFoo() %}" +
      "{% for y in [1] %}{% set x = \"foo\" %}{{ x }}{% endfor %}" +
      "{% endmacro %}" +
      "{% macro display() %}" +
      "{% set x = \"bar\" %}" +
      "{{ setFoo() }}" +
      "{{ x }}" +
      "{% endmacro %}" +
      "{{ display() }}"

    environment.render(template) mustEqual "foobar"
  }

  "should compile macros without leaking set to calling scope" in {

    val template =
      "{% macro foo(topLevel, prefix=\"\") %}" +
      "{% if topLevel %}" +
      "{% set x = \"\" %}" +
      "{% for i in [1,2] %}" +
      "{{ foo(false, x) }}" +
      "{% endfor %}" +
      "{% else %}" +
      "{% set x = prefix + \"foo\" %}" +
      "{{ x }}" +
      "{% endif %}" +
      "{% endmacro %}" +
      "{{ foo(true) }}"

    environment.render(template) mustEqual "foofoo"
  }

  "should compile macros that cannot see variables in caller scope" in {

    val template =
      "{% macro one(var) %}{{ two() }}{% endmacro %}" +
      "{% macro two() %}{{ var }}{% endmacro %}" +
      "{{ one(\"foo\") }}"

    environment.render(template) mustEqual ""
  }

  "should compile call blocks" in {

    val template =
      "{% macro wrap(el) %}" +
      "<{{ el }}>{{ caller() }}</{{ el }}>" +
      "{% endmacro %}" +
      "{% call wrap(\"div\") %}Hello{% endcall %}"

    environment.render(template) mustEqual "<div>Hello</div>"
  }

  "should compile call blocks with args" in {

    val template =
      "{% macro list(items) %}" +
      "<ul>{% for i in items %}" +
      "<li>{{ caller(i) }}</li>" +
      "{% endfor %}</ul>" +
      "{% endmacro %}" +
      "{% call(items) list([\"a\", \"b\"]) %}{{ item }}{% endcall %}"

    environment.render(template) mustEqual "<ul><li>a</li><li>b</li></ul>"
  }

  "should compile call blocks using imported macros" in {

    val template =
      "{% import \"import.njk\" as imp %}" +
      "{% call imp.wrap(\"span\") %}Hey{% endcall %}"

    environment.render(template) mustEqual "<span>Hey</span>"
  }

  "should import templates" in {

    environment.render("{% import \"import.njk\" as imp %}{{ imp.foo() }} {{ imp.bar }}") mustEqual "Here's a macro baz"
    environment.render("{% from \"import.njk\" import foo as baz, bar %}{{ bar }} {{ baz() }}") mustEqual "baz Here's a macro"

    environment.render(
      "{% for i in [1,2] %}" +
      "start: {{ num }}" +
      "{% from \"import.njk\" import bar as num %}" +
      "end: {{ num }}" +
      "{% endfor %}" +
      "final: {{ num }}"
    ) mustEqual "start: end: bazstart: bazend: bazfinal: "
  }

  "should import templates with context" in {

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% import \"import-context.njk\" as imp with context %}" +
      "{{ imp.foo() }}"
    ) mustEqual "Here's BAR"

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% from \"import-context.njk\" import foo with context %}" +
      "{{ foo() }}"
    ) mustEqual "Here's BAR"

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% import \"import-context-set.njk\" as imp %}" +
      "{{ bar }}"
    ) mustEqual "BAR"

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% import \"import-context-set.njk\" as imp %}" +
      "{{ imp.bar }}"
    ) mustEqual "FOO"

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% import \"import-context-set.njk\" as imp with context %}" +
      "{{ bar }}{{ buzz }}"
    ) mustEqual "FOO"

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% import \"import-context-set.njk\" as imp with context %}" +
      "{{ imp.bar }}{{ buzz }}"
    ) mustEqual "FOO"
  }

  "should import templates without context" in {

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% import \"import-context.njk\" as imp without context %}" +
      "{{ imp.foo() }}"
    ) mustEqual "Here's "

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% from \"import-context.njk\" import foo without context %}" +
      "{{ foo() }}"
    ) mustEqual "Here's "
  }

  "should default to importing without context" in {

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% import \"import-context.njk\" as imp %}" +
      "{{ imp.foo() }}"
    ) mustEqual "Here's "

    environment.render(
      "{% set bar = \"BAR\" %}" +
      "{% from \"import-context.njk\" import foo %}" +
      "{{ foo() }}"
    ) mustEqual "Here's "
  }

  "should inherit templates" in {

    environment.render("{% extends \"base.njk\" %}") mustEqual "FooBarBazFizzle"
    environment.render("hola {% extends \"base.njk\" %} hizzle mumble") mustEqual "FooBarBazFizzle"
    environment.render("{% extends \"base.njk\" %}{% block block1 %}BAR{% endblock %}") mustEqual "FooBARBazFizzle"

    environment.render(
      "{% extends \"base.njk\" %}" +
      "{% block block1 %}BAR{% endblock %}" +
      "{% block block2 %}BAZ{% endblock %}"
    ) mustEqual "FooBARBAZFizzle"

    environment.render("hola {% extends tmpl %} hizzle mumble", Value.Obj(
      "tmpl" -> Value.Str("base.njk")
    ))
  }

  "should not call blocks not defined from template inheritance" in {

    var count = 0

    val fn = Value.Function {
      (_, _) =>
        count += 1
        Value.Undefined
    }

    environment.render(
      "{% extends \"base.njk\" %}" +
      "{% block notReal %}{{ foo() }}{% endblock %}", Value.Obj(
        "foo" -> fn
      )
    )

    count mustBe 0
  }

  "should conditionally inherit templates" in {

    environment.render(
      "{% if false %}{% extends \"base.njk\" %}{% endif %}" +
      "{% block block1 %}BAR{% endblock %}"
    ) mustEqual "BAR"

    environment.render(
      "{% if true %}{% extends \"base.njk\" %}{% endif %}" +
      "{% block block1 %}BAR{% endblock %}"
    ) mustEqual "FooBARBazFizzle"

    environment.render(
      "{% if true %}" +
      "{% extends \"base.njk\" %}" +
      "{% else %}" +
      "{% extends \"base2.njk\" %}" +
      "{% endif %}" +
      "{% block block1 %}HELLO{% endblock %}"
    ) mustEqual "FooHELLOBazFizzle"

    environment.render(
      "{% if false %}" +
      "{% extends \"base.njk\" %}" +
      "{% else %}" +
      "{% extends \"base2.njk\" %}" +
      "{% endif %}" +
      "{% block item %}hello{{ item }}{% endblock %}"
    ) mustEqual "hello1hello2"
  }

  "should error if same block is defined multiple times" in {

    assertThrows[RuntimeException] {

      environment.render(
        "{% extends \"simple-base.njk\" %}" +
          "{% block test %}{% endblock %}" +
          "{% block test %}{% endblock %}"
      )
    }
  }

  "should render nested blocks in child template" in {

    environment.render(
      "{% extends \"base.njk\" %}" +
      "{% block block1 %}{% block nested %}BAR{% endblock %}{% endblock %}"
    ) mustEqual "FooBARBazFizzle"
  }

  "should render parent blocks with super()" in {

    environment.render(
      "{% extends \"base.njk\" %}" +
      "{% block block1 %}{{ super() }}BAR{% endblock %}"
    ) mustEqual "FooBarBARBazFizzle"

    environment.render(
      "{% extends \"base-inherit.njk\" %}" +
      "{% block block1 %}*{{ super() }}*{% endblock %}"
    ) mustEqual "Foo**Bar**BazFizzle"
  }

  "should let super() see global vars from child template" in {

    environment.render(
      "{% extends \"base-show.njk\" %}{% set var = \"child\" %}" +
      "{% block main %}{{ super() }}{% endblock %}"
    ) mustEqual "child"
  }

  "should not let super() see vars from child block" in {

    environment.render(
      "{% extends \"base-show.njk\" %}" +
      "{% block main %}{% set var = \"child\" %}{{ super() }}{% endblock %}"
    ) mustEqual ""
  }

  "should let child templates access parent global scope" in {

    environment.render(
      "{% extends \"base-set.njk\" %}" +
      "{% block main %}{{ var }}{% endblock %}"
    ) mustEqual "parent"
  }

  "should not let super() modify calling scope" in {

    environment.render(
      "{% extends \"base-set-inside-block.njk\" %}" +
      "{% block main %}{{ super() }}{{ var }}{% endblock %}"
    ) mustEqual ""
  }

  "should not let child templates set vars in parent scope" in {

    environment.render(
      "{% extends \"base-set-and-show.njk\" %}" +
      "{% block main %}{% set var = \"child\" %}{% endblock %}"
    ) mustEqual "parent"
  }

  "should render blocks in their own scope" in {

    environment.render(
      "{% set var = \"parent\" %}" +
      "{% block main %}{% set var = \"inner\" %}{% endblock %}" +
      "{{ var }}"
    ) mustEqual "parent"
  }

  "should include templates" in {

    environment.render("hello world {% include \"include.njk\" %}") mustEqual "hello world FooInclude "
  }

  "should include 130 templates without call stack size exceed" in {

    environment.render("{% include \"includeMany.njk\" %}") mustEqual ("FooInclude \\n" * 131)
  }

  "should include templates with context" in {

    environment.render("hello world {% include \"include.njk\" %}", Value.Obj(
      "name" -> Value.Str("james")
    )) mustEqual "hello world FooInclude james"
  }

  "should include templates that can see including scope, but not write to it" in {

    environment.render("{% set var = 1 %}{% include \"include-set.njk\" %}{{ var }}") mustEqual "12\n1"
  }

  "should include templates dynamically" in {

    environment.render("hello world {% include tmpl %}", Value.Obj(
      "name" -> Value.Str("thedude"),
      "tmpl" -> Value.Str("include.njk")
    )) mustEqual "hello world FooInclude thedude"
  }

  "should include templates dynamically based on a set var" in {

    environment.render("hello world {% set tmpl = \"include.njk\" %}{% include tmpl %}", Value.Obj(
      "name" -> Value.Str("thedude")
    )) mustEqual "hello world FooInclude thedude"
  }

  "should include templates dynamically based on an object attr" in {

    environment.render("hello world {% include data.tmpl %}", Value.Obj(
      "name" -> Value.Str("thedude"),
      "data" -> Value.Obj(
        "tmpl" -> Value.Str("include.njk")
      )
    )) mustEqual "hello world FooInclude thedude"
  }

  "should throw an error when including a file that does not exist" in {

    assertThrows[RuntimeException] {
      environment.render("{% include \"missing.njk\" %}")
    }
  }

  "should fail silently on missing templates if requested" in {

    environment.render("hello world {% include \"missing.njk\" ignore missing %}") mustEqual "hello world "
    environment.render("hello world {% include \"missing.njk\" ignore missing %}", Value.Obj(
      "name" -> Value.Str("thedude")
    )) mustEqual "hello world "
  }

  "should have access to \"loop\" inside an include" in {

    environment.render("{% for item in [1,2,3] %}{% include \"include-in-loop.njk\" %}{% endfor %}") mustEqual "1,0,true\n2,1,false\n3,2,false\n"
    environment.render("{% for k,v in items %}{% include \"include-in-loop.njk\" %}{% endfor %}", Value.Obj(
      "items" -> Value.Obj(
        "a" -> Value.Str("A"),
        "b" -> Value.Str("B")
      )
    )) mustEqual "1,0,true\n2,1,false\n"
  }

  "should maintain nested scopes" in {

    environment.render(
      "{% for i in [1,2] %}" +
      "{% for i in [3,4] %}{{ i }}{% endfor %}" +
      "{{ i }}{% endfor %}"
    ) mustEqual "341342"
  }

  "should allow blocks in for loops" in {

    environment.render(
      "{% extends \"base2.njk\" %}" +
      "{% block item %}hello{{ item }}{% endblock %}"
    ) mustEqual "hello1hello2"
  }

  "should make includes inherit scope" in {

    environment.render(
      "{% for item in [1,2] %}" +
      "{% include \"item.njk\" %}" +
      "{% endfor %}"
    ) mustEqual "showing 1showing 2"
  }

  "should compile a set block" in {

    environment.render("{% set username = \"foo\" %}{{ username }}", Value.Obj(
      "username" -> Value.Str("james")
    )) mustEqual "foo"

    environment.render("{% set x, y = \"foo\" %}{{ x }}{{ y }}") mustEqual "foofoo"

    environment.render("{% set x = 1 + 2 %}{{ x }}") mustEqual "3"

    environment.render("{% for i in [1] %}{% set foo=1 %}{% endfor %}{{ foo }}", Value.Obj(
      "foo" -> Value.Number(2)
    )) mustEqual "2"

    environment.render("{% include \"set.njk\" %}{{ foo }}", Value.Obj(
      "foo" -> Value.Str("bar")
    )) mustEqual "bar"

    environment.render("{% set username = username + \"pasta\" %}{{ username }}", Value.Obj(
      "username" -> Value.Str("basta")
    )) mustEqual "bastapasta"

    environment.render(
      "{% for i in [1] %}{% set val=5 %}{% endfor %}" +
      "{{ val }}"
    ) mustEqual ""

    environment.render(
      "{% for i in [1,2,3] %}" +
      "{% if not val %}{% set val=5 %}{% endif %}" +
      "{% set val=val+1 %}{{ val }}" +
      "{% endfor %}" +
      "afterwards: {{ val }}"
    ) mustEqual "678afterwards: "

    environment.render(
      "{% set val=1 %}" +
      "{% for i in [1] %}{% set val=5 %}{% endfor %}" +
      "{{ val }}"
    ) mustEqual "5"

    environment.render(
      "{% set val=5 %}" +
      "{% for i in [1,2,3] %}" +
      "{% set val=val+1 %}{{ val }}" +
      "{% endfor %}" +
      "afterwards: {{ val }}"
    ) mustEqual "678afterwards: 8"
  }

  "should compile set with frame references" in {

    environment.render("{% set username = user.name %}{{ username }}", Value.Obj(
      "user" -> Value.Obj(
        "name" -> Value.Str("james")
      )
    )) mustEqual "james"
  }

  "should compile set assignments of the same variable" in {

    environment.render(
      "{% set x = \"hello\" %}" +
      "{% if false %}{% set x = \"world\" %}{% endif %}" +
      "{{ x }}"
    ) mustEqual "hello"

    environment.render(
      "{% set x = \"blue\" %}" +
      "{% if true %}{% set x = \"green\" %}{% endif %}" +
      "{{ x }}"
    ) mustEqual "green"
  }

  "should compile block-set" in {

    environment.render(
      "{% set block_content %}{% endset %}" +
      "{{ block_content }}"
    ) mustEqual ""

    environment.render(
      "{%- macro foo(bar) -%}" +
      "{%- set test -%}foo{%- endset -%}" +
      "{{ bar }}{{ test }}" +
      "{%- endmacro -%}" +
      "{{ foo(\"bar\") }}"
    ) mustEqual "barfoo"

    environment.render(
      "{% set block_content %}test string{% endset %}" +
      "{{ block_content }}"
    ) mustEqual "test string"

    environment.render(
      "{% set block_content %}" +
      "{% for item in [1, 2, 3] %}" +
      "{% include \"item.njk\" %} " +
      "{% endfor %}" +
      "{% endset %}" +
      "{{ block_content }}"
    ) mustEqual "showing 1 showing 2 showing 3 "

    environment.render(
      "{% set block_content %}" +
      "{% set inner_block_content %}" +
      "{% for i in [1, 2, 3] %}" +
      "item {{ i }} " +
      "{% endfor %}" +
      "{% endset %}" +
      "{% for i in [1, 2, 3] %}" +
      "inner {{i}}: \"{{ inner_block_content }}\" " +
      "{% endfor %}" +
      "{% endset %}" +
      "{{ block_content | safe }}"
    ) mustEqual "inner 1: \"item 1 item 2 item 3 \" inner 2: \"item 1 item 2 item 3 \" inner 3: \"item 1 item 2 item 3 \" "

    environment.render(
      "{% set x,y,z %}" +
      "cool" +
      "{% endset %}" +
      "{{ x }} {{ y }} {{ z }}"
    ) mustEqual "cool cool cool"
  }

  "should compile block-set wrapping an inherited block" in {

    environment.render(
      "{% extends \"base-set-wraps-block.njk\" %}" +
      "{% block somevar %}foo{% endblock %}"
    ) mustEqual "foo\n"
  }

  "should throw errors" in {

    assertThrows[RuntimeException] {
      environment.render("{% from \"import.njk\" import boozle %}")
    }
  }

  "should autoescape by default" in {

    environment.render("{{ foo }}", Value.Obj(
      "foo" -> Value.Str("\"'<>&")
    )) mustEqual "&quot;&#39;&lt;&gt;&amp;"
  }

  "should not autoescape safe strings" in {

    environment.render("{{ foo|safe }}", Value.Obj(
      "foo" -> Value.Str("\"'<>&")
    )) mustEqual "\"'<>&"
  }

  "should not autoescape macros" in {

    environment.render(
      "{% macro foo(x, y) %}{{ x }} and {{ y }}{% endmacro %}" +
      "{{ foo(\"<>&\", \"<>\") }}"
    ) mustEqual "&lt;&gt;&amp; and &lt;&gt;"

    environment.render(
      "{% macro foo(x, y) %}{{ x|safe }} and {{ y }}{% endmacro %}" +
      "{{ foo(\"<>&\", \"<>\") }}"
    ) mustEqual "<>& and &lt;&gt;"
  }

  "should not autoescape super()" in {

    environment.render(
      "{% extends \"base3.njk\" %}" +
      "{% block block1 %}{{ super() }}{% endblock %}"
    ) mustEqual "<b>Foo</b>"
  }

  "should render regexs" in {

    environment.render("{{ r/name [0-9] \\\\// }}") mustEqual "/name [0-9] \\\\//"
    environment.render("{{ r/x/gi }}") mustEqual "/x/gi"
  }

  "should throw an error when {% call %} is passed an object that is not a function" in {

    assertThrows[RuntimeException] {
      environment.render("{% call foo() %}{% endcall %}", Value.Obj(
        "foo" -> Value.Str("bar")
      ))
    }
  }

  "should throw an error when including a file that calls an undefined macro" in {

    assertThrows[RuntimeException] {
      environment.render("{% include \"undefined-macro.njk\" %}")
    }
  }

  "should throw an error when including a file that calls an undefined macro even inside {% if %} tag" in {

    assertThrows[RuntimeException] {
      environment.render("{% if true %}{% include \"undefined-macro.njk\" %}{% endif %}")
    }
  }

  "should throw an error when including a file that imports macro that calls an undefined macro" in {

    assertThrows[RuntimeException] {
      environment.render("{% include \"import-macro-call-undefined-macro.njk\" %}", Value.Obj(
        "list" -> Value.Arr(Seq(Value.Number(1), Value.Number(2), Value.Number(3)))
      ))
    }
  }

  "should control whitespaces correctly" in {

    environment.render("{% if true -%}{{\"hello\"}} {{\"world\"}}{% endif %}") mustEqual "hello world"

    environment.render(
      "{% if true -%}{% if true %} {{\"hello\"}} {{\"world\"}}" +
      "{% endif %}{% endif %}"
    ) mustEqual " hello world"

    environment.render("{% if true -%}{# comment #} {{\"hello\"}}{% endif %}") mustEqual " hello"
  }

  "should control expression whitespaces correctly" in {

    environment.render("Well, {{- ' hello, ' -}} my friend") mustEqual "Well, hello, my friend"
    environment.render(" {{ 2 + 2 }} ") mustEqual " 4 "
    environment.render(" {{-2 + 2 }} ") mustEqual "4 "
    environment.render(" {{ -2 + 2 }} ") mustEqual " 0 "
    environment.render(" {{ 2 + 2 -}} ") mustEqual " 4"
  }

  "should get right value when macro parameter conflict with global macro name" in {

    environment.render(
      "{# macro1 and macro2 definition #}" +
      "{% macro macro1() %}" +
      "{% endmacro %}" +
      "{% macro macro2(macro1=\"default\") %}" +
      "{{macro1}}" +
      "{% endmacro %}" +
      "{# calling macro2 #}" +
      "{{macro2(\"this should be outputted\") }}"
    ).trim mustEqual "this should be outputted"
  }

  "should get right value when macro include macro" in {

    environment.render(
      "{# macro1 and macro2 definition #}" +
      "{% macro macro1() %} foo" +
      "{% endmacro %}" +
      "{% macro macro2(text=\"default\") %}" +
      "{{macro1()}}" +
      "{% endmacro %}" +
      "{# calling macro2 #}" +
      "{{macro2(\"this should not be outputted\") }}"
    ).trim mustEqual "foo"
  }

  "should allow access to outer scope in call blocks" in {

    environment.render(
      "{% macro inside() %}" +
      "{{ caller() }}" +
      "{% endmacro %}" +
      "{% macro outside(var) %}" +
      "{{ var }}\n" +
      "{% call inside() %}" +
      "{{ var }}" +
      "{% endcall %}" +
      "{% endmacro %}" +
      "{{ outside(\"foobar\") }}"
    ).trim mustEqual "foobar\nfoobar"
  }

  "should not leak scope from call blocks to parent" in {

    environment.render(
      "{% set var = \"expected\" %}" +
      "{% macro inside() %}" +
      "{% set var = \"incorrect-value\" %}" +
      "{{ caller() }}" +
      "{% endmacro %}" +
      "{% macro outside() %}" +
      "{% call inside() %}" +
      "{% endcall %}" +
      "{% endmacro %}" +
      "{{ outside() }}" +
      "{{ var }}"
    ).trim mustEqual "expected"
  }

  "should throw an error when invalid expression whitespaces are used" in {

    assertThrows[Exception] {
      environment.render(" {{ 2 + 2- }}")
    }
  }

  "the filter tag" - {

    "should apply the title filter to the body" in {

      environment.render("{% filter title %}may the force be with you{% endfilter %}") mustEqual "May The Force Be With You"
    }

    "should apply the replace filter to the body" in {

      environment.render("{% filter replace(\"force\", \"forth\") %}may the force be with you{% endfilter %}") mustEqual "may the forth be with you"
    }

    "should work with variables in the body" in {

      environment.render("{% set foo = \"force\" %}{% filter replace(\"force\", \"forth\") %}may the {{ foo }} be with you{% endfilter %}") mustEqual "may the forth be with you"
    }

    "should work with blocks in the body" in {

      environment.render(
        "{% extends \"filter-block.html\" %}" +
        "{% block block1 %}force{% endblock %}"
      ) mustEqual "may the forth be with you\n"
    }
  }
}
