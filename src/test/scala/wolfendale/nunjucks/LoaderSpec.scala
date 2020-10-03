package wolfendale.nunjucks

import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class LoaderSpec extends AnyFreeSpec with Matchers with OptionValues with EitherValues {

  def compile(template: String): Template = {
    import fastparse._
    parse(template, TemplateParser.template(_)).get.value
  }

  val loader: ProvidedLoader = new ProvidedLoader()

  "a loader" - {

    "must resolve a root template" in {

      val foo = compile("Hello, World!")

      loader
        .add("foo", foo)
        .resolve("foo", None)
        .right
        .value mustEqual "foo"
    }

    "must resolve a sibling template" in {

      val foo = compile("Hello, World!")

      loader
        .add("foo", foo)
        .resolve("foo", Some("bar"))
        .right
        .value mustEqual "foo"

      loader
        .add("bar/foo", foo)
        .resolve("foo", Some("bar/quux"))
        .right
        .value mustEqual "bar/foo"

      loader
        .add("bar/foo", foo)
        .resolve("./foo", Some("bar/quux"))
        .right
        .value mustEqual "bar/foo"
    }

    "must prioritise a relative template over a root one" in {

      val foo = compile("Hello, World!")

      loader
        .add("foo", foo)
        .add("bar/foo", foo)
        .resolve("foo", Some("bar/quux"))
        .right
        .value mustEqual "bar/foo"
    }

    "must fail with a list of attempted paths when a template cannot be found" in {

      loader.resolve("foo", Some("bar/quux")).left.value must contain only (
        "foo", "bar/foo"
      )
    }
  }
}
