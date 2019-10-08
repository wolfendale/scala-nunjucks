package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value

// TODO allow passing in custom filters
final case class Context(environment: Environment,
                         scope: Value.Obj = Value.Obj.empty,
                         blocks: Map[String, Vector[TemplateNode.Partial]] = Map.empty,
                         filters: Map[String, Filter] = wolfendale.nunjucks.filters.defaults) {

  def getScope(identifier: String): Value =
    scope.get(identifier)

  def setScope(scope: Value.Obj): Context =
    copy(scope = scope)

  def setScope(identifier: String, value: Value): Context =
    setScope(identifier -> value)

  def setScope(entries: (String, Value)*): Context =
    copy(scope = scope.set(entries: _*))

  def getBlocks(identifier: String): Vector[TemplateNode.Partial] =
    blocks.getOrElse(identifier, Vector.empty)

  def setBlock(identifier: String, block: TemplateNode.Partial): Context =
    copy(blocks = blocks + (identifier -> (block +: getBlocks(identifier))))

  def getFilter(identifier: String): Option[Filter] =
    filters.get(identifier)
}

