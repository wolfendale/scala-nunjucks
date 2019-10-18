package wolfendale.nunjucks

import org.scalatest.{FreeSpec, MustMatchers, OptionValues}

class LoaderSpec extends FreeSpec with MustMatchers with OptionValues {

  def compile(template: String): Template = {
    import fastparse._
    parse(template, TemplateParser.template(_)).get.value
  }

  val loader: ProvidedLoader = new ProvidedLoader()

  "a loader" - {

    "must load a root template" in {

      val foo = compile("Hello, World!")

      loader
        .add("foo", foo)
        .load("foo", None).value mustEqual foo
    }

    "must load a sibling template" in {

      val foo = compile("Hello, World!")

      loader
        .add("foo", foo)
        .load("foo", Some("bar")).value mustEqual foo

      loader
        .add("bar/foo", foo)
        .load("foo", Some("bar/quux")).value mustEqual foo

      loader
        .add("bar/foo", foo)
        .load("./foo", Some("bar/quux")).value mustEqual foo
    }
  }
}
