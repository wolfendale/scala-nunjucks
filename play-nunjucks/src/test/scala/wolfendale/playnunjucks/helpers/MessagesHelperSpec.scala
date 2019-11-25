package wolfendale.playnunjucks.helpers

import org.scalatest.{FreeSpec, MustMatchers}
import play.api.test.{FakeRequest, Helpers}
import wolfendale.nunjucks.ProvidedEnvironment

class MessagesHelperSpec extends FreeSpec with MustMatchers {

  private val messages = Helpers.stubMessagesApi(
    Map(
      "en" -> Map(
        "key"            -> "hello",
        "keyParams"      -> "hello {0}",
        "keyMultiParams" -> "hello {0}{1}"
      )
    )
  )
  private val helper   = new MessagesHelper(messages)
  private val nunjucks = new ProvidedEnvironment().addGlobal("messages", helper.value(FakeRequest()))

  "MessagesHelper" - {

    "should render valid message keys" in {
      val result = nunjucks.render("{{ messages('key') }}")

      result mustBe "hello"
    }

    "should render valid message keys with parameters" in {
      val result = nunjucks.render("{{ messages('keyParams', 'world') }}")

      result mustBe "hello world"
    }

    "should render valid message keys with multiple parameters" in {
      val result = nunjucks.render("{{ messages('keyMultiParams', 'world', '!') }}")

      result mustBe "hello world!"
    }

    "should render valid message keys with missing parameters" in {
      val result = nunjucks.render("{{ messages('keyParams') }}")

      result mustBe "hello {0}"
    }

    "should render valid message keys with excess parameters" in {
      val result = nunjucks.render("{{ messages('keyParams', 'world', '!') }}")

      result mustBe "hello world"
    }

    "should render invalid message keys as the key" in {
      val result = nunjucks.render("{{ messages('missingKey') }}")

      result mustBe "missingKey"
    }

  }

}
