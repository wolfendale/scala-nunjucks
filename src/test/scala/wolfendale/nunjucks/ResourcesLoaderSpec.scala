package wolfendale.nunjucks

import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import wolfendale.nunjucks.expression.runtime.Value

class ResourcesLoaderSpec extends FreeSpec with MustMatchers with OptionValues {

  val loader = new ResourcesLoader(Seq(
    "/",
    "/sub",
    "/META-INF/resources/webjars/govuk-frontend/3.1.0/govuk"
  ))

  val environment = new Environment(Seq(loader))

  "a resources loader" - {

    "must render a template from a resource" in {

      environment.renderTemplate("hello.njk", Value.Obj(
        "name" -> Value.Str("World")
      )).value mustEqual "Hello, World!"
    }

    "must render a template from a resource at a secondary root" in {

      environment.renderTemplate("hey.njk", Value.Obj(
        "name" -> Value.Str("World")
      )).value mustEqual "Hey, World!"
    }

    "must render a template from an external jar" in {

      val result = environment.renderTemplate("template.njk")

      result mustBe defined
    }
  }
}
