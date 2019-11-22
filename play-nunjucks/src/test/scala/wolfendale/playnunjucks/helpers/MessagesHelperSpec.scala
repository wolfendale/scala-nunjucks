package wolfendale.playnunjucks.helpers

import org.scalatest.{FreeSpec, MustMatchers}
import play.api.test.{FakeRequest, Helpers}
import wolfendale.nunjucks.expression.runtime.Value
import wolfendale.nunjucks.expression.runtime.Value.Function.{Parameter, Parameters}
import wolfendale.nunjucks.{Context, ProvidedEnvironment, RenderMode}

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
  private val function = helper.value(FakeRequest()).asInstanceOf[Value.Function]
  private val ctx      = Context(new ProvidedEnvironment(), RenderMode.Template)

  def runHelper(params: Parameters): String =
    function.fn(params).runA(ctx).value.toStr.value

  "MessagesHelper" - {

    "should render valid message keys" in {
      val params = Parameters(
        Seq(
          Parameter(Value.Str("key"))
        ))

      val result = runHelper(params)

      result mustBe "hello"
    }

    "should render valid message keys with parameters" in {
      val params = Parameters(
        Seq(
          Parameter(Value.Str("keyParams")),
          Parameter(Value.Str("world"))
        ))

      val result = runHelper(params)

      result mustBe "hello world"
    }

    "should render valid message keys with multiple parameters" in {
      val params = Parameters(
        Seq(
          Parameter(Value.Str("keyMultiParams")),
          Parameter(Value.Str("world")),
          Parameter(Value.Str("!"))
        ))

      val result = runHelper(params)

      result mustBe "hello world!"
    }

    "should render valid message keys with missing parameters" in {
      val params = Parameters(
        Seq(
          Parameter(Value.Str("keyParams"))
        ))

      val result = runHelper(params)

      result mustBe "hello {0}"
    }

    "should render valid message keys with excess parameters" in {
      val params = Parameters(
        Seq(
          Parameter(Value.Str("keyParams")),
          Parameter(Value.Str("world")),
          Parameter(Value.Str("!"))
        ))

      val result = runHelper(params)

      result mustBe "hello world"
    }

    "should render invalid message keys as the key" in {
      val params = Parameters(
        Seq(
          Parameter(Value.Str("missingKey"))
        ))

      val result = runHelper(params)

      result mustBe "missingKey"
    }

  }

}
