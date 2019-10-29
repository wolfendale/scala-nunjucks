package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value

final case class Context(environment: Environment,
                         private val _renderMode: RenderMode,
                         private val _frame: Frame = Frame.empty,
                         private val _variables: Map[String, Value] = Map.empty,
                         private val _blocks: Map[String, Vector[TemplateNode.Partial]] = Map.empty,
                         private val _path: Option[String] = None) {


  def empty: Context =
    empty(_renderMode)

  def empty(renderMode: RenderMode): Context =
    Context(environment, renderMode)

  def exports: Context.ExportsProjection =
    Context.ExportsProjection(this)

  def variables: Context.VariablesProjection =
    Context.VariablesProjection(this)

  def frame: Context.FrameProjection =
    Context.FrameProjection(this)

  def blocks: Context.BlockProjection =
    Context.BlockProjection(this)

  def path: Context.PathProjection =
    Context.PathProjection(this)

  def renderMode: Context.RenderModeProjection =
    Context.RenderModeProjection(this)

  def setFrameAndVariable(key: String, value: Value, resolveUp: Boolean): Context =
    if (_frame.isRoot) {
      this
        .frame.set(key, value, resolveUp)
        .variables.set(key, value)
    } else {
      frame.set(key, value, resolveUp)
    }

  def getContextValue(key: String): Value =
    frame.get(key) orElse variables.get(key) orElse environment.getGlobal(key)
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

    def getAll: Map[String, Value] =
      context._variables
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

    def empty: Context =
      context.copy(_frame = Frame.empty)

    def push: Context =
      context.copy(_frame = context._frame.push)

    def push(frame: Frame): Context =
      push(frame.values)

    def push(values: Map[String, Value]): Context =
      context.copy(_frame = Frame(values, context._frame))

    def pop: Context =
      context.copy(_frame = context._frame.pop)
  }

  final case class BlockProjection(context: Context) {

    def get(key: String): Vector[TemplateNode.Partial] =
      context._blocks.getOrElse(key, Vector.empty)

    def push(key: String, block: TemplateNode.Partial): Context =
      context.copy(_blocks = context._blocks + (key -> (block +: get(key))))
  }

  final case class PathProjection(context: Context) {

    def get: Option[String] =
      context._path

    def set(path: Option[String]): Context =
      context.copy(_path = path)
  }

  final case class RenderModeProjection(context: Context) {

    def get: RenderMode =
      context._renderMode

    def set(renderMode: RenderMode): Context =
      context.copy(_renderMode = renderMode)
  }
}

sealed abstract class RenderMode {

  def withContext: Boolean
}

object RenderMode {

  case object Template extends RenderMode {
    override val withContext: Boolean = true
  }

  final case class Import(withContext: Boolean) extends RenderMode
}
