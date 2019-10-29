package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value
import cats._
import cats.data._
import cats.implicits._

package object globals {

  val joiner: Value.Function = Value.Function { parameters =>
    val separator = parameters.get(0).getOrElse(Value.Str(","))
    State.pure {
      var first = true
      Value.Function { _ =>
        State.pure {
          if (first) {
            first = false
            Value.Str("")
          } else {
            separator
          }
        }
      }
    }
  }

  lazy val defaults: Map[String, Value] = Map(
    "joiner" -> joiner
  )
}
