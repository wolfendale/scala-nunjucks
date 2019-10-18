package wolfendale.nunjucks

import cats._
import cats.data._
import cats.implicits._
import wolfendale.nunjucks.expression.runtime.Value

class Environment(loaders: Seq[Loader]) {

  def load(to: String, from: Option[String]): Option[Template] =
    loaders.flatMap(_.load(to, from)).headOption

  def load(to: String): Option[Template] =
    load(to, None)

  def renderTemplate(path: String): Option[String] =
    renderTemplate(path, Value.Obj.empty)

  def renderTemplate(path: String, scope: Value.Obj): Option[String] = {
    load(path).map(_.render.runA(Context(this, Frame(scope).enter)).value)
  }

  def render(template: String): String =
    render(template, Value.Obj.empty)

  def render(template: String, scope: Value.Obj): String = {
    // TODO handle errors
    import fastparse._
    parse(template, TemplateParser.template(_)).get.value.render.runA(Context(this, Frame(scope).enter)).value
  }
}

final class ProvidedEnvironment(loader: ProvidedLoader = new ProvidedLoader()) extends Environment(Seq(loader)) {

  def add(name: String, template: Template): ProvidedEnvironment =
    new ProvidedEnvironment(loader.add(name, template))

  def add(name: String, template: String): ProvidedEnvironment =
    new ProvidedEnvironment(loader.add(name, template))
}
