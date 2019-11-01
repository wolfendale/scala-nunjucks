package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value
import cats._
import cats.data._
import cats.implicits._

final case class Scope private (
    private val frames: Map[Int, Scope.Frame],
    private val current: Scope.Frame,
    private val nextId: Int
) {

  def push: Scope = {

    val frame = Scope.Frame(
      id = nextId,
      values = Map.empty,
      parentId = Some(current.id)
    )

    copy(
      frames = frames + (nextId -> frame),
      current = frame,
      nextId = nextId + 1
    )
  }

  def pop: Scope = copy(
    current = current.parentId
      .flatMap { id =>
        frames.get(id)
      }
      .getOrElse(current) // if you try to pop the root frame you get the root frame, we could also throw an error?
  )

  def get(key: String): Value =
    resolve(key).flatMap { id =>
      frames.get(id).flatMap(_.values.get(key))
    }.getOrElse(Value.Undefined)

  def set(key: String, value: Value, resolveUp: Boolean = false): Scope = {

    val destinationFrame = if (resolveUp) {
      resolve(key)
        .flatMap(frames.get)
        .getOrElse(current)
    } else {
      current
    }

    setFrame(destinationFrame.set(key, value))
  }

  def setMultiple(entries: Seq[(String, Value)], resolveUp: Boolean = false): Scope =
    entries.foldLeft(this) {
      case (m, (k, v)) =>
        m.set(k, v, resolveUp)
    }

  private def resolve(key: String): Option[Int] = {

    def resolve(key: String, pointer: Int): Option[Int] =
      frames.get(pointer).flatMap { frame =>
        if (frame.values.get(key).isDefined) {
          Some(pointer)
        } else {
          frame.parentId.flatMap(resolve(key, _))
        }
      }

    ((current.get(key) >> Some(current.id)))
      .orElse(current.parentId.flatMap(resolve(key, _)))
  }

  private def setFrame(frame: Scope.Frame): Scope =
    copy(
      frames = frames + (frame.id -> frame),
      current = if (current.id == frame.id) frame else current
    )

  @deprecated
  def isRoot: Boolean =
    current.id == 0
}

object Scope {

  def empty: Scope = {
    val frame = Frame(
      id = 0,
      values = Map.empty,
      parentId = None
    )
    new Scope(
      frames = Map(0 -> frame),
      current = frame,
      nextId = 1
    )
  }

  def fromObj(obj: Value.Obj): Scope = {
    val frame = Frame(
      id = 0,
      values = obj.values,
      parentId = None
    )
    new Scope(
      frames = Map(0 -> frame),
      current = frame,
      nextId = 1
    )
  }

  final case class Frame(
      id: Int,
      values: Map[String, Value],
      parentId: Option[Int] = None
  ) {

    def get(key: String): Option[Value] =
      values.get(key)

    def set(key: String, value: Value): Frame =
      copy(values = values + (key -> value))
  }
}
