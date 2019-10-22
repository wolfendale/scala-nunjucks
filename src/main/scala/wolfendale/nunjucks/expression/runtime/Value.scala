package wolfendale.nunjucks.expression.runtime

import wolfendale.nunjucks.Frame
import wolfendale.nunjucks.expression.syntax.AST

import scala.annotation.tailrec
import scala.language.implicitConversions
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
      case _: Str | _: Arr | _: Obj | _: Function | _: Regex =>
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

  def apply(scope: Frame, args: Function.Parameters): Value =
    throw new RuntimeException(s"unable to call $this")

  def isDefined: Boolean = true

  def toArr: Arr              = Arr.empty
  def destructure: Seq[Value] = Seq(this)

  def toStr: Str

  def toBool: Bool

  def toNumeric: Numeric

  final def orElse(other: Value): Value =
    if (isDefined) this else other
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
      Str("undefined")

    override def toBool: Bool =
      False

    override def toNumeric: Numeric =
      NaN

    override def isDefined: Boolean = false
  }

  sealed abstract class Bool extends Value {

    final override def toBool: Bool = this
  }

  object Bool {

    def apply(bool: Boolean): Bool =
      if (bool) Value.True else Value.False

    implicit def toBoolean(bool: Bool): Boolean =
      bool match {
        case True  => true
        case False => false
      }
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
        case False | Number(0) | Str("", _) => True
        case _                              => False
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
        case _: Str | _: Arr | _: Obj | _: Regex => toStr + other
        case _                                   => this
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

    @tailrec
    override def +(other: Value): Value =
      other match {
        case NaN | `-Infinity`                   => NaN
        case _: Numeric                          => Infinity
        case _: Str | _: Arr | _: Obj | _: Regex => toStr + other
        case _                                   => this + other.toNumeric
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
        case NaN | Infinity                      => NaN
        case _: Numeric                          => -Infinity
        case _: Str | _: Arr | _: Obj | _: Regex => toStr + other
        case _                                   => this + other.toNumeric
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
        case NaN                                 => NaN
        case Infinity                            => Infinity
        case `-Infinity`                         => -Infinity
        case Number(o)                           => Number(value + o)
        case _: Str | _: Obj | _: Arr | _: Regex => toStr + other
        case _                                   => this + other.toNumeric
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

  final case class Str(value: String, safe: Boolean = false) extends Value {

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

    def escaped: Str = copy(
      value = Str.escapeRegex.replaceAllIn(value, Str.escapeMap),
      safe = true
    )
  }

  object Str {

    import scala.util.matching.Regex.Match

    private val escapeRegex = "[&\"'<>]".r
    private val escapeMap = PartialFunction[Match, String] {
      case Match("&")  => "&amp;"
      case Match("\"") => "&quot;"
      case Match("'")  => "&#39;"
      case Match("<")  => "&lt;"
      case Match(">")  => "&gt;"
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

    def merge(other: Value.Obj): Value.Obj =
      Obj(values ++ other.values)
  }

  object Obj {

    def apply(entries: (String, Value)*): Obj =
      Obj(entries.toMap)

    def empty: Obj = apply()
  }

  final case class Function(fn: (Frame, Function.Parameters) => Value) extends Value {

    override def `==`(other: Value): Bool =
      this `===` other

    override def toStr: Str =
      Str("[object Function]")

    override def toBool: Bool =
      True

    override def toNumeric: Numeric =
      NaN

    override def apply(scope: Frame, args: Function.Parameters): Value =
      fn(scope, args)
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

  final case class Regex(pattern: String, flagSet: Set[RegexFlag] = Set.empty) extends Value {

    import java.util.regex._

    override def `==`(other: Value): Bool = Bool(eq(other))

    private def source: String = if (this.pattern.isEmpty) "(?:)" else this.pattern

    private def flags: String = flagSet.toSeq.map(_.flag).sorted.mkString

    override def toStr: Str = Value.Str(s"/$source/$flags")

    override def toBool: Bool = Value.True

    override def toNumeric: Numeric = Value.NaN

    private def javaFlagInt(flag: RegexFlag): Int = flag match {
      case RegexFlag.CaseInsensitive => Pattern.CASE_INSENSITIVE
      case _                         => 0
    }

    private lazy val javaRegexMatcher: Pattern = {
      val javaFlags = flagSet.foldLeft(0)((l, flag) => l | javaFlagInt(flag))
      Pattern.compile(pattern, javaFlags)
    }

    def global: Boolean = flagSet.contains(RegexFlag.ApplyGlobally)

    def multiline: Boolean = flagSet.contains(RegexFlag.MultiLine)

    def ignoreCase: Boolean = flagSet.contains(RegexFlag.CaseInsensitive)

    def sticky: Boolean = flagSet.contains(RegexFlag.Sticky)

    //TODO sticky flag (may need a var ...)
    def testFunction: Value = Value.Function { (_, params) =>
      val subjectUnderTest = params.get("str", 0)

      // TODO is there an easy way to do case insensitive without using Java?
      def matchSingleLine(str: String): Boolean = javaRegexMatcher.matcher(str).matches()

      def matchMultiLine(str: String): Boolean =
        if (multiline) str.split(System.lineSeparator).exists(matchSingleLine)
        else matchSingleLine(str)

      Bool(subjectUnderTest exists (sut => matchMultiLine(sut.toStr.value)))
    }

    override def properties: Map[String, Value] = Map(
      "flags"      -> Str(flags),
      "global"     -> Bool(global),
      "ignoreCase" -> Bool(ignoreCase),
      "multiline"  -> Bool(multiline),
      "test"       -> testFunction,
      "sticky"     -> Bool(sticky),
      "source"     -> Str(source)
    )
  }

  sealed abstract class RegexFlag(val flag: String)

  object RegexFlag {
    def apply(flag: AST.RegexFlag): RegexFlag = flag match {
      case AST.RegexFlag.ApplyGlobally   => ApplyGlobally
      case AST.RegexFlag.CaseInsensitive => CaseInsensitive
      case AST.RegexFlag.MultiLine       => MultiLine
      case AST.RegexFlag.Sticky          => Sticky
    }

    final case object ApplyGlobally extends RegexFlag(AST.RegexFlag.ApplyGlobally.flag)

    final case object CaseInsensitive extends RegexFlag(AST.RegexFlag.CaseInsensitive.flag)

    final case object MultiLine extends RegexFlag(AST.RegexFlag.MultiLine.flag)

    final case object Sticky extends RegexFlag(AST.RegexFlag.Sticky.flag)
  }
}
