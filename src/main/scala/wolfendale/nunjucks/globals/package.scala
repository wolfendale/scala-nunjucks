package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value
import cats._
import cats.data._
import cats.implicits._
import wolfendale.nunjucks.expression.runtime.Value.{Arr, Number}

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

  val range: Value.Function = Value.Function{ parameters =>

    State.pure {
      if (parameters.parameters.forall(_.value.isInstanceOf[Number])) {
        val (start, stop, step): (Value.Number, Value.Number, Value.Number) = parameters.parameters.length match {
          case 1 => (Number(0), parameters.get(0).get.asInstanceOf[Number], Number(1))
          case 2 => (parameters.get(0).get.asInstanceOf[Number], parameters.get(1).get.asInstanceOf[Number], Number(1))
          case 3 => (parameters.get(0).get.asInstanceOf[Number], parameters.get(1).get.asInstanceOf[Number], parameters.get(2).get.asInstanceOf[Number])
        }

        Arr.from((start.value until stop.value by step.value).map(Number): _*)
      } else {
        Arr.empty
      }
    }
  }

  lazy val defaults: Map[String, Value] = Map(
    "joiner" -> joiner,
    "range"  -> range
  )
}
