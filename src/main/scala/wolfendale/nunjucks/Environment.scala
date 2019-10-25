package wolfendale.nunjucks

import cats._
import cats.data._
import cats.implicits._
import wolfendale.nunjucks.expression.runtime.Value

class Environment(loaders: NonEmptyChain[Loader]) {

  def this(loader: Loader, rest: Loader*) =
    this(NonEmptyChain(loader, rest: _*))

  def resolveAndLoad(path: String, caller: Option[String]): Either[List[String], Loader.ResolvedTemplate] =
    loaders.parTraverse(_.resolveAndLoad(path, caller)).map(_.head)

  def renderTemplate(path: String): Option[String] =
    renderTemplate(path, Value.Obj.empty)

  def renderTemplate(path: String, scope: Value.Obj): Option[String] = {
    val context = Context(this)
      .variables.set(scope.values.toSeq)
    resolveAndLoad(path, None).toOption.map(_.template.render.runA(context).value)
  }

  def render(template: String): String =
    render(template, Value.Obj.empty)

  def render(template: String, scope: Value.Obj): String = {
    // TODO handle errors
    import fastparse._
    val context = Context(this)
      .variables.set(scope.values.toSeq)
    parse(template, TemplateParser.template(_)).get.value.render.runA(context).value
  }
}

final class ProvidedEnvironment(loader: ProvidedLoader = new ProvidedLoader()) extends Environment(loader) {

  def add(name: String, template: Template): ProvidedEnvironment =
    new ProvidedEnvironment(loader.add(name, template))

  def add(name: String, template: String): ProvidedEnvironment =
    new ProvidedEnvironment(loader.add(name, template))
}
