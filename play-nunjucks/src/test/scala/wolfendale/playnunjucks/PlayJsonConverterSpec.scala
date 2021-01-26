package wolfendale.playnunjucks

import org.scalatest.{FreeSpec, MustMatchers}
import play.api.libs.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString}
import wolfendale.nunjucks.expression.runtime.Value
import wolfendale.nunjucks.expression.runtime.Value._

class PlayJsonConverterSpec extends FreeSpec with MustMatchers {

  val converter: PlayJsonConverter = new PlayJsonConverter

  "PlayJsonConverter" - {

    "should convert null" in {
      converter.convert(JsNull) mustEqual Null
    }

    "should convert boolean true" in {
      converter.convert(JsBoolean(true)) mustEqual Bool(true)
    }

    "should convert boolean false" in {
      converter.convert(JsBoolean(false)) mustEqual Bool(false)
    }

    "should convert zero" in {
      converter.convert(JsNumber(0)) mustEqual Number(0)
    }

    "should convert positive integers" in {
      converter.convert(JsNumber(1)) mustEqual Number(1)
    }

    "should convert negative integers" in {
      converter.convert(JsNumber(-1)) mustEqual Number(-1)
    }

    "should convert positive doubles" in {
      converter.convert(JsNumber(1.5)) mustEqual Number(1.5)
    }

    "should convert negative doubles" in {
      converter.convert(JsNumber(-1.5)) mustEqual Number(-1.5)
    }

    "should convert empty strings" in {
      converter.convert(JsString("")) mustEqual Str("")
    }

    "should convert non-empty strings" in {
      converter.convert(JsString("hello")) mustEqual Str("hello")
    }

    "should convert empty arrays" in {
      converter.convert(JsArray()) mustEqual Arr(Seq.empty)
    }

    "should convert non-empty arrays" in {
      converter.convert(JsArray(Seq(JsString("hello"), JsString("world")))) mustEqual Arr(Seq(Str("hello"), Str("world")))
    }

    "should convert simple objects" in {
      converter.convert(JsObject(Seq(
        ("string", JsString("string")),
        ("number", JsNumber(1)),
        ("null", JsNull)
      )))
    }

    "should convert nested objects" in {
      converter.convert(JsObject(Seq(
        ("string", JsString("string")),
        ("number", JsNumber(1)),
        ("null", JsNull),
        ("nested", JsObject(Seq(
          ("string", JsString("string")),
          ("number", JsNumber(1)),
          ("null", JsNull)
        )))
      )))
    }

  }

}
