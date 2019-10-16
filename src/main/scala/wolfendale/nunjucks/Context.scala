package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value

// TODO allow passing in custom filters
final case class Context(
    environment: Environment,
    scope: Frame = Frame.empty,
    blocks: Map[String, Vector[TemplateNode.Partial]] = Map.empty,
    filters: Map[String, Filter] = wolfendale.nunjucks.filters.defaults
) {

  def getScope(identifier: String): Value =
    scope.get(identifier)

  def setScope(scope: Frame): Context =
    copy(scope = scope)

  def setScope(entries: Seq[(String, Value)], resolveUp: Boolean): Context =
    entries.foldLeft(this) {
      case (m, (k, v)) =>
        m.setScope(k, v, resolveUp)
    }

  def setScope(key: String, value: Value, resolveUp: Boolean): Context =
    copy(scope = scope.set(key, value, resolveUp))

  def enterScope: Context =
    copy(scope = scope.enter)

  def exitScope: Context =
    copy(scope = scope.exit)

  def getBlocks(identifier: String): Vector[TemplateNode.Partial] =
    blocks.getOrElse(identifier, Vector.empty)

  def setBlock(identifier: String, block: TemplateNode.Partial): Context =
    copy(blocks = blocks + (identifier -> (block +: getBlocks(identifier))))

  def getFilter(identifier: String): Option[Filter] =
    filters.get(identifier)
}
