package wolfendale.nunjucks

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import wolfendale.nunjucks.expression.runtime.Value

class ResourcesLoaderSpec extends AnyFreeSpec with Matchers with OptionValues {

  val loader = new ResourcesLoader(
    List(
      "/",
      "/sub",
      "/META-INF/resources/webjars/govuk-frontend/3.1.0/govuk"
    ))

  val environment = new Environment(loader)

  "a resources loader" - {

    "must render a template from a resource" in {

      environment
        .renderTemplate("hello.njk",
                        Value.Obj(
                          "name" -> Value.Str("World")
                        ))
        .value mustEqual "Hello, World!"
    }

    "must render a template from a resource at a secondary root" in {

      environment
        .renderTemplate("hey.njk",
                        Value.Obj(
                          "name" -> Value.Str("World")
                        ))
        .value mustEqual "Hey, World!"
    }

    "must render a template from an external jar" in {

      val result = environment.renderTemplate("template.njk")

      result mustBe defined
    }
  }
}
