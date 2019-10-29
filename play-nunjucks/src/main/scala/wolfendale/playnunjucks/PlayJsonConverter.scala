package wolfendale.playnunjucks

import play.api.libs.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}
import wolfendale.nunjucks.expression.runtime.Value
import wolfendale.nunjucks.expression.runtime.Value._

class PlayJsonConverter {

  def convert(js: JsValue): Value = js match {
      case JsNull => Null
      case boolean: JsBoolean => Bool(boolean.value)
      case JsNumber(value) => Number(value.doubleValue())
      case JsString(value) => Str(value)
      case JsArray(value) => Arr(value.map(convert))
      case JsObject(underlying) =>
        val map = underlying.map({case (key, value) => (key, convert(value))})
        Obj(map.toArray:_*)
  }

}
