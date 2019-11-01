package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value

sealed abstract class Frame {

  def get(key: String): Value
  def set(key: String, value: Value, resolveUp: Boolean): Frame
  def push: Frame
  def pop: Frame

  final def set(entries: Seq[(String, Value)], resolveUp: Boolean): Frame =
    entries.foldLeft(this) {
      case (m, (k, v)) =>
        m.set(k, v, resolveUp)
    }

  def resolve(key: String): Option[Frame]

  def isRoot: Boolean

  def values: Map[String, Value]
}

final case class RootFrame(values: Map[String, Value]) extends Frame {

  override def push: ChildFrame =
    Frame(Map.empty[String, Value], this)

  override def pop: RootFrame =
    this

  override def set(key: String, value: Value, resolveUp: Boolean): Frame =
    RootFrame(values + (key -> value))

  override def get(key: String): Value =
    values.getOrElse(key, Value.Undefined)

  override def resolve(key: String): Option[Frame] =
    if (values.get(key).isDefined) Some(this) else None

  override def isRoot: Boolean = true
}

final case class ChildFrame(values: Map[String, Value], parent: Frame) extends Frame {

  override def push: ChildFrame =
    Frame(Map.empty[String, Value], this)

  override def pop: Frame =
    parent

  override def get(key: String): Value =
    values.getOrElse(key, parent.get(key))

  override def set(key: String, value: Value, resolveUp: Boolean): ChildFrame =
    if (resolveUp) {
      resolve(key)
        .filterNot(_ == this)
        .map {
          _ =>
            Frame(values, parent.set(key, value, resolveUp = true))
        }
        .getOrElse(Frame(values + (key -> value), parent))
    } else {
      Frame(values + (key -> value), parent)
    }

  override def resolve(key: String): Option[Frame] =
    if (values.get(key).isDefined) Some(this) else parent.resolve(key)

  override def isRoot: Boolean = false
}

object Frame {

  def apply(values: Map[String, Value]): RootFrame =
    RootFrame(values)

  def apply(values: Value.Obj): RootFrame =
    apply(values.values)

  def apply(values: Map[String, Value], parent: Frame): ChildFrame =
    ChildFrame(values, parent)

  def apply(values: Value.Obj, parent: Frame): ChildFrame =
    apply(values.values, parent)

  def empty: RootFrame =
    RootFrame(Map.empty)
}
