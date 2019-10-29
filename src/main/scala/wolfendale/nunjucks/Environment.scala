package wolfendale.nunjucks

import cats._
import cats.data._
import cats.implicits._
import wolfendale.nunjucks.expression.runtime.Value

class Environment(
                   loaders: NonEmptyChain[Loader],
                   filters: Map[String, Filter],
                   globals: Map[String, Value]
                 ) {

  def this(loader: Loader, rest: Loader*) =
    this(
      loaders = NonEmptyChain(loader, rest: _*),
      filters = wolfendale.nunjucks.filters.defaults,
      globals = wolfendale.nunjucks.globals.defaults
    )

  def resolveAndLoad(path: String, caller: Option[String]): Either[List[String], Loader.ResolvedTemplate] =
    loaders.parTraverse(_.resolveAndLoad(path, caller)).map(_.head)

  def renderTemplate(path: String): Option[String] =
    renderTemplate(path, Value.Obj.empty)

  def renderTemplate(path: String, scope: Value.Obj): Option[String] = {
    resolveAndLoad(path, None).toOption.map { resolvedTemplate =>
      val context = Context(this, RenderMode.Template)
          .variables.set(scope.values.toSeq)
          .path.set(Some(resolvedTemplate.path))
      resolvedTemplate.template.render.runA(context).value
    }
  }

  def render(template: String): String =
    render(template, Value.Obj.empty)

  def render(template: String, scope: Value.Obj): String = {
    // TODO handle errors
    import fastparse._
    val context = Context(this, RenderMode.Template)
      .variables.set(scope.values.toSeq)
    parse(template, TemplateParser.template(_)).get.value.render.runA(context).value
  }

  def getFilter(identifier: String): Option[Filter] =
    filters.get(identifier)

  def getGlobal(identifier: String): Value =
    globals.getOrElse(identifier, Value.Undefined)
}

final class ProvidedEnvironment(
                                 loader: ProvidedLoader = new ProvidedLoader(),
                                 filters: Map[String, Filter] = wolfendale.nunjucks.filters.defaults,
                                 globals: Map[String, Value] = wolfendale.nunjucks.globals.defaults
                               ) extends Environment(NonEmptyChain.one(loader), filters, globals) {

  def add(name: String, template: Template): ProvidedEnvironment =
    new ProvidedEnvironment(loader.add(name, template))

  def add(name: String, template: String): ProvidedEnvironment =
    new ProvidedEnvironment(loader.add(name, template))
}
