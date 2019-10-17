package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value
import wolfendale.nunjucks.expression.runtime.Value._

package object filters {

  val abs: Filter = Filter { value =>
    value.toNumeric match {
      case Number(x)   => Number(Math.abs(x))
      case Infinity    => Infinity
      case `-Infinity` => Infinity
      case NaN         => NaN
    }
  }

  // TODO: sensible failures
  val batch: Filter = Filter { (arr, args) =>
    args.get(0).map(_.toNumeric).getOrElse(Number(0)) match {
      case Number(size) =>
        val paddingChar = args.get(1)
        val it = arr.toArr.values.iterator.grouped(size.toInt)
        val groupedArray = paddingChar
          .map(padding => it.withPadding(padding.toStr))
          .getOrElse(it).toList.map(Arr(_))
        Arr(groupedArray)
      case Infinity    => arr.toArr
      case `-Infinity` => arr.toArr
      case NaN         => arr.toArr
    }
  }

  val capitalize: Filter = Filter { string =>
    Str(string.toStr.value.capitalize)
  }

  val upper: Filter = Filter { string =>
    Str(string.toStr.value.toUpperCase)
  }

  val lower: Filter = Filter { string =>
    Str(string.toStr.value.toLowerCase)
  }

  val trim: Filter = Filter { string: Value =>
    Str(string.toStr.value.trim)
  }

  val first: Filter = Filter {
    _ match {
      case arr: Arr =>
        arr.destructure.headOption.getOrElse(Undefined)
      case str: Str =>
        str.value.headOption.map(_.toString).map(Str.apply).getOrElse(Undefined)
      case Infinity | `-Infinity` =>
        Undefined
      case Value.Obj(_) | Number(_) | Value.True | Value.False | NaN =>
        Undefined
      case x =>
        throw new RuntimeException(s"Cannot read property '0' of $x")
    }
  }

  val string: Filter = Filter { string: Value =>
    string.toStr
  }

  lazy val defaults: Map[String, Filter] = Map(
    "abs"        -> abs,
    "batch"      -> batch,
    "capitalize" -> capitalize,
    "upper"      -> upper,
    "lower"      -> lower,
    "trim"       -> trim,
    "first"      -> first,
    "string"     -> string
  )
}
