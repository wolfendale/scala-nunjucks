package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value

sealed abstract class Frame {

  def get(key: String): Value
  def set(key: String, value: Value, resolveUp: Boolean): Frame
  def enter: Frame
  def exit: Frame

  final def set(entries: Seq[(String, Value)], resolveUp: Boolean): Frame =
    entries.foldLeft(this) {
      case (m, (k, v)) =>
        m.set(k, v, resolveUp)
    }

  def value: Value.Obj

  def resolve(key: String): Option[Frame]
}

final case class RootFrame(values: Map[String, Value]) extends Frame {

  override def enter: ChildFrame =
    Frame(Map.empty[String, Value], this)

  override def exit: RootFrame =
    throw new RuntimeException("escaped root scope")

  override def value: Value.Obj =
    Value.Obj(values)

  override def set(key: String, value: Value, resolveUp: Boolean): Frame =
    RootFrame(values + (key -> value))

  override def get(key: String): Value =
    values.getOrElse(key, Value.Undefined)

  override def resolve(key: String): Option[Frame] =
    if (values.get(key).isDefined) Some(this) else None
}

final case class ChildFrame(values: Map[String, Value], parent: Frame) extends Frame {

  override def enter: ChildFrame =
    Frame(Map.empty[String, Value], this)

  override def exit: Frame =
    parent

  override def get(key: String): Value =
    values.getOrElse(key, parent.get(key))

  override def set(key: String, value: Value, resolveUp: Boolean): ChildFrame =
    if (resolveUp) {
      resolve(key).filterNot(_ == this).map {
        case _: RootFrame =>
          // this case is strange
          Frame(values + (key -> value), parent)
        case _ =>
          Frame(values, parent.set(key, value, resolveUp = true))
      }.getOrElse(Frame(values + (key -> value), parent))
    } else {
      Frame(values + (key -> value), parent)
    }

  override def value: Value.Obj =
    parent.value merge Value.Obj(values)

  override def resolve(key: String): Option[Frame] =
    if (values.get(key).isDefined) Some(this) else parent.resolve(key)
}

object Frame {

  def apply(values: Map[String, Value]): RootFrame =
    new RootFrame(values)

  def apply(values: Value.Obj): RootFrame =
    apply(values.values)

  def apply(values: Map[String, Value], parent: Frame): ChildFrame =
    new ChildFrame(values, parent)

  def apply(values: Value.Obj, parent: Frame): ChildFrame =
    apply(values.values, parent)

  def empty: RootFrame =
    new RootFrame(Map.empty)
}
