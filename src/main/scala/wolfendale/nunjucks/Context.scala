package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value

final case class Context(environment: Environment,
                         private val _frame: Frame = Frame.empty,
                         private val _variables: Map[String, Value] = Map.empty,
                         private val _blocks: Map[String, Vector[TemplateNode.Partial]] = Map.empty,
                         private val _filters: Map[String, Filter] = wolfendale.nunjucks.filters.defaults,
                         private val _path: Option[String] = None) {

  def variables: Context.VariablesProjection =
    Context.VariablesProjection(this)

  def frame: Context.FrameProjection =
    Context.FrameProjection(this)

  def blocks: Context.BlockProjection =
    Context.BlockProjection(this)

  def filters: Context.FilterProjection =
    Context.FilterProjection(this)

  def path: Context.PathProjection =
    Context.PathProjection(this)

  def set(key: String, value: Value, resolveUp: Boolean): Context =
    if (_frame.isRoot) {
      this
        .frame.set(key, value, resolveUp)
        .variables.set(key, value)
    } else {
      frame.set(key, value, resolveUp)
    }

  def get(key: String): Value =
    frame.get(key) orElse variables.get(key)
}

object Context {

  final case class ExportsProjection(context: Context) {

    def get: Map[String, Value] =
      context._variables.filterKeys(!_.startsWith("_"))

    def get(key: String): Value =
      get.getOrElse(key, Value.Undefined)
  }

  final case class VariablesProjection(context: Context) {

    def set(entries: Seq[(String, Value)]): Context =
      entries.foldLeft(context) {
        case (c, (k, v)) =>
          c.variables.set(k, v)
      }

    def set(key: String, value: Value): Context =
      context.copy(_variables = context._variables + (key -> value))

    def get(key: String): Value =
      context._variables.getOrElse(key, Value.Undefined)
  }

  final case class FrameProjection(context: Context) {

    def set(key: String, value: Value, resolveUp: Boolean): Context =
      context.copy(_frame = context._frame.set(key, value, resolveUp))

    def set(entries: Seq[(String, Value)], resolveUp: Boolean): Context =
      context.copy(_frame = context._frame.set(entries, resolveUp))

    def set(frame: Frame): Context =
      context.copy(_frame = frame)

    def get(key: String): Value =
      context._frame.get(key)

    def get: Frame =
      context._frame

    def push: Context =
      context.copy(_frame = context._frame.push)

    def pop: Context =
      context.copy(_frame = context._frame.pop)
  }

  final case class BlockProjection(context: Context) {

    def get(key: String): Vector[TemplateNode.Partial] =
      context._blocks.getOrElse(key, Vector.empty)

    def push(key: String, block: TemplateNode.Partial): Context =
      context.copy(_blocks = context._blocks + (key -> (block +: get(key))))
  }

  final case class FilterProjection(context: Context) {

    def get(key: String): Option[Filter] =
      context._filters.get(key)

    def set(key: String, filter: Filter): Context =
      context.copy(_filters = context._filters + (key -> filter))
  }

  final case class PathProjection(context: Context) {

    def get: Option[String] =
      context._path

    def set(path: Option[String]): Context =
      context.copy(_path = path)
  }
}