package wolfendale.nunjucks.expression.runtime

import scala.annotation.tailrec
import scala.util.Try

sealed abstract class Value {

  import Value._

  def `==`(other: Value): Bool

  final def `!=`(other: Value): Bool =
    !(this `==` other)

  final def `===`(other: Value): Bool =
    if (this == other) True else False
  final def `!==`(other: Value): Bool =
    !(this `===` other)

  def <(other: Value): Bool =
    toNumeric < other

  final def <=(other: Value): Bool =
    (this < other) || (this == other)

  def >(other: Value): Bool =
    toNumeric > other

  final def >=(other: Value): Bool =
    (this > other) || (this == other)

  def unary_! : Bool = !toBool

  def unary_- : Numeric = -toNumeric
  def unary_+ : Numeric = toNumeric

  def +(other: Value): Value =
    other match {
      case Null | Undefined | _: Bool | _: Numeric =>
        toNumeric + other
      case _: Str | _: Arr | _: Obj | _: Function =>
        toStr + other
    }

  final def -(other: Value): Value =
    this + -other

  def *(other: Value): Numeric =
    toNumeric * other

  def **(other: Value): Numeric =
    toNumeric ** other

  def /(other: Value): Numeric =
    toNumeric / other

  def idiv(other: Value): Numeric =
    toNumeric idiv other

  def %(other: Value): Numeric =
    toNumeric % other

  def &&(other: Value): Bool =
    toBool && other

  def ||(other: Value): Bool =
    toBool || other

  def properties: Map[String, Value] =
    Map.empty

  def access(identifier: String): Value =
    properties.getOrElse(identifier, Value.Undefined)

  def apply(context: Obj, args: Function.Parameters): Value =
    throw new RuntimeException(s"unable to call $this")

  def toArr: Arr              = Arr.empty
  def destructure: Seq[Value] = Seq(this)

  def toStr: Str
  def toBool: Bool
  def toNumeric: Numeric
}

object Value {

  case object Null extends Value {

    override def `==`(other: Value): Bool =
      other match {
        case Null | Undefined => True
        case _                => False
      }

    override def toStr: Str =
      Str("null")

    override def toBool: Bool =
      False

    override def toNumeric: Numeric =
      Number(0)
  }

  case object Undefined extends Value {

    override val toString: String = "undefined"

    override def `==`(other: Value): Bool =
      other match {
        case Null | Undefined => True
        case _                => False
      }

    override def toStr: Str =
      Str("")

    override def toBool: Bool =
      False

    override def toNumeric: Numeric =
      NaN
  }

  sealed abstract class Bool extends Value {

    final override def toBool: Bool = this
  }

  case object True extends Bool {

    override def `==`(other: Value): Bool =
      other match {
        case True | Number(1) => True
        case _                => False
      }

    override def ||(other: Value): Bool =
      this

    override def &&(other: Value): Bool =
      other.toBool

    override def unary_! : Bool =
      False

    override def toStr: Str =
      Str("true")

    override def toNumeric: Numeric =
      Number(1)
  }

  case object False extends Bool {

    override def `==`(other: Value): Bool =
      other match {
        case False | Number(0) | Str("") => True
        case _                           => False
      }

    override def ||(other: Value): Bool =
      other.toBool

    override def &&(other: Value): Bool =
      this

    override def unary_! : Bool =
      True

    override def toStr: Str =
      Str("false")

    override def toNumeric: Numeric =
      Number(0)
  }

  sealed abstract class Numeric extends Value

  case object NaN extends Numeric {

    override def `==`(other: Value): Bool =
      False

    override def <(other: Value): Bool =
      False

    override def >(other: Value): Bool =
      False

    override def +(other: Value): Value =
      other match {
        case _: Str | _: Arr | _: Obj => toStr + other
        case _                        => this
      }

    override def *(other: Value): Numeric =
      this

    override def **(other: Value): Numeric =
      this

    override def /(other: Value): Numeric =
      this

    override def idiv(other: Value): Numeric =
      this

    override def %(other: Value): Numeric =
      this

    override def unary_- : Numeric =
      this

    override def toStr: Str =
      Str("NaN")

    override def toBool: Bool =
      False

    override def toNumeric: Numeric =
      this
  }

  case object Infinity extends Numeric {

    override def `==`(other: Value): Bool =
      other.toNumeric match {
        case Infinity => True
        case _        => False
      }

    override def <(other: Value): Bool =
      False

    override def >(other: Value): Bool =
      other match {
        case Infinity | NaN => False
        case _              => True
      }

    override def +(other: Value): Value =
      other match {
        case NaN | `-Infinity`        => NaN
        case _: Numeric               => Infinity
        case _: Str | _: Arr | _: Obj => toStr + other
        case _                        => this + other.toNumeric
      }

    override def *(other: Value): Numeric =
      other.toNumeric match {
        case NaN | Number(0)    => NaN
        case Infinity           => Infinity
        case Number(o) if o > 0 => Infinity
        case _                  => -Infinity
      }

    override def **(other: Value): Numeric =
      other match {
        case NaN                => NaN
        case Infinity           => Infinity
        case Number(o) if o > 0 => Infinity
        case _                  => Number(0)
      }

    override def /(other: Value): Numeric =
      other.toNumeric match {
        case NaN | Infinity | `-Infinity` => NaN
        case Number(o) if o > 0           => Infinity
        case _                            => -Infinity
      }

    override def idiv(other: Value): Numeric =
      this / other

    override def %(other: Value): Numeric =
      NaN

    override def unary_- : Numeric =
      `-Infinity`

    override def toStr: Str =
      Str("Infinity")

    override def toBool: Bool =
      True

    override def toNumeric: Numeric =
      this
  }

  case object `-Infinity` extends Numeric {

    override def `==`(other: Value): Bool =
      other.toNumeric match {
        case `-Infinity` => True
        case _           => False
      }

    override def <(other: Value): Bool =
      other match {
        case `-Infinity` | NaN => False
        case _                 => True
      }

    override def >(other: Value): Bool =
      False

    @tailrec
    override def +(other: Value): Value =
      other match {
        case NaN | Infinity           => NaN
        case _: Numeric               => -Infinity
        case _: Str | _: Arr | _: Obj => toStr + other
        case _                        => this + other.toNumeric
      }

    override def *(other: Value): Numeric =
      other.toNumeric match {
        case NaN | Number(0)    => NaN
        case Number(o) if o > 0 => -Infinity
        case Infinity           => -Infinity
        case _                  => Infinity
      }

    override def **(other: Value): Numeric =
      other.toNumeric match {
        case NaN                => NaN
        case Number(0)          => Number(1)
        case Number(o) if o < 0 => Number(-0)
        case Infinity           => Infinity
        case _                  => -Infinity
      }

    override def /(other: Value): Numeric =
      other.toNumeric match {
        case NaN | Infinity | `-Infinity` => NaN
        case Number(o) if o > 0           => -Infinity
        case _                            => Infinity
      }

    override def idiv(other: Value): Numeric =
      this / other

    override def %(other: Value): Numeric =
      NaN

    override def unary_- : Numeric =
      Infinity

    override def toStr: Str =
      Str("-Infinity")

    override def toBool: Bool =
      True

    override def toNumeric: Numeric =
      this
  }

  final case class Number(value: Double) extends Numeric {

    override def `==`(other: Value): Bool =
      other.toNumeric match {
        case Number(o) => if (value == o) True else False
        case _         => False
      }

    override def <(other: Value): Bool =
      other.toNumeric match {
        case NaN | `-Infinity` => False
        case Infinity          => True
        case Number(o)         => if (value < o) True else False
      }

    override def >(other: Value): Bool =
      other.toNumeric match {
        case NaN | Infinity => False
        case `-Infinity`    => True
        case Number(o)      => if (value > o) True else False
      }

    @tailrec
    override def +(other: Value): Value =
      other match {
        case NaN                      => NaN
        case Infinity                 => Infinity
        case `-Infinity`              => -Infinity
        case Number(o)                => Number(value + o)
        case _: Str | _: Obj | _: Arr => toStr + other
        case _                        => this + other.toNumeric
      }

    override def *(other: Value): Numeric =
      other.toNumeric match {
        case NaN         => NaN
        case Number(o)   => Number(value * o)
        case Infinity    => if (value > 0) Infinity else -Infinity
        case `-Infinity` => if (value > 0) -Infinity else Infinity
      }

    override def **(other: Value): Numeric =
      other.toNumeric match {
        case NaN         => NaN
        case Number(o)   => Number(scala.math.pow(value, o))
        case Infinity    => if (value == 0) Number(0) else NaN
        case `-Infinity` => if (value == 0) Infinity else NaN
      }

    override def /(other: Value): Numeric =
      other.toNumeric match {
        case NaN                    => NaN
        case Number(0)              => Infinity
        case Number(o)              => Number(value / o)
        case Infinity | `-Infinity` => Number(0)
      }

    override def idiv(other: Value): Numeric =
      other.toNumeric match {
        case NaN                    => NaN
        case Number(0)              => Infinity
        case Number(o)              => Number((value / o).toLong.toDouble)
        case Infinity | `-Infinity` => Number(0)
      }

    override def %(other: Value): Numeric =
      other.toNumeric match {
        case NaN | Infinity | `-Infinity` => NaN
        case Number(o)                    => Number(value % o)
      }

    override def unary_- : Numeric =
      Number(-value)

    override def toStr: Str =
      Str(if (value.isWhole) value.toLong.toString else value.toString)

    override def toBool: Bool =
      if (value == 0) False else True

    override def toNumeric: Numeric =
      this
  }

  final case class Str(value: String) extends Value {

    override def `==`(other: Value): Bool =
      if (value == other.toStr.value) True else False

    override def +(other: Value): Value =
      Str(s"$value${other.toStr.value}")

    override def toArr: Arr =
      Arr(value.map(c => Str(c.toString)))

    override def toStr: Str =
      this

    override def toBool: Bool =
      if (value.isEmpty) False else True

    override def toNumeric: Numeric =
      value match {
        case ""          => Number(0)
        case "Infinity"  => Infinity
        case "-Infinity" => -Infinity
        case _           => Try(value.toDouble).map(Number).getOrElse(NaN)
      }
  }

  final case class Arr(values: Seq[Value]) extends Value {

    override def `==`(other: Value): Bool =
      other match {
        case Number(0) => if (values.isEmpty) True else False
        case _         => this `===` other
      }

    override def toArr: Arr =
      this

    override def toStr: Str =
      Str(values.mkString(","))

    override def toBool: Bool =
      True

    override def toNumeric: Numeric =
      toStr.toNumeric

    override def access(identifier: String): Value = {

      val result = for {
        index <- Try(identifier.toInt).toOption
        value <- values.lift(index)
      } yield value

      result.getOrElse(super.access(identifier))
    }

    override val properties: Map[String, Value] = Map(
      "length" -> Value.Number(values.length)
    )

    override def destructure: Seq[Value] =
      values
  }

  object Arr {

    def empty: Arr = Arr(Seq.empty)

    def from(values: Value*): Arr =
      Arr(values)
  }

  final case class Obj(values: Map[String, Value]) extends Value {

    override def `==`(other: Value): Bool =
      this `===` other

    override def toArr: Arr =
      Arr(values.toSeq.map {
        case (k, v) =>
          Arr.from(Str(k), v)
      })

    override def toStr: Str =
      Str("[object Object]")

    override def toBool: Bool =
      True

    override def toNumeric: Numeric =
      NaN

    override val properties: Map[String, Value] =
      values

    def get(name: String): Value =
      access(name)

    def set(entries: (String, Value)*): Obj =
      copy(values ++ entries.toMap)

    def set(name: String, value: Value): Obj =
      copy(values + (name -> value))
  }

  object Obj {

    def apply(entries: (String, Value)*): Obj =
      Obj(entries.toMap)

    def empty: Obj = apply()
  }

  final case class Function(fn: (Obj, Function.Parameters) => Value) extends Value {

    override def `==`(other: Value): Bool =
      this `===` other

    override def toStr: Str =
      Str("[object Function]")

    override def toBool: Bool =
      True

    override def toNumeric: Numeric =
      NaN

    override def apply(context: Obj, args: Function.Parameters): Value =
      fn(context, args)
  }

  object Function {

    final case class Parameter(name: Option[String], value: Value)

    object Parameter {

      def apply(value: Value): Parameter =
        Parameter(None, value)
    }

    final case class Parameters(parameters: Seq[Parameter]) {

      private lazy val keyed: Map[String, Value] =
        parameters.collect {
          case Parameter(Some(key), value) =>
            key -> value
        }.toMap

      def get(key: String, index: Int): Option[Value] =
        keyed.get(key) orElse get(index)

      def get(index: Int): Option[Value] =
        parameters.lift(index).map(_.value)
    }
  }
}
