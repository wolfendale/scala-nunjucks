package wolfendale.nunjucks

import cats._
import cats.data._
import cats.implicits._
import wolfendale.nunjucks.expression.runtime.Value

abstract class Environment {

  def load(path: String): Option[Template]

  def renderTemplate(path: String): Option[String] =
    renderTemplate(path, Value.Obj.empty)

  def renderTemplate(path: String, scope: Value.Obj): Option[String] = {
    load(path).map(_.render.runA(Context(this, scope)).value)
  }

  def render(template: String): String =
    render(template, Value.Obj.empty)

  def render(template: String, scope: Value.Obj): String = {
    // TODO handle errors
    import fastparse._
    parse(template, TemplateParser.template(_)).get.value.render.runA(Context(this, scope)).value
  }

  def importTemplate(path: String): Option[Value.Obj] =
    load(path).map(_.render.runS(Context(this, Value.Obj.empty)).value.scope)
}

final class ProvidedEnvironment(templates: Map[String, Template] = Map.empty) extends Environment {

  def add(name: String, template: Template): ProvidedEnvironment =
    new ProvidedEnvironment(templates + (name -> template))

  def add(name: String, template: String): ProvidedEnvironment = {
    // TODO: handle errors
    import fastparse._
    val compiledTemplate = parse(template, TemplateParser.template(_)).get.value
    add(name, compiledTemplate)
  }

  override def load(path: String): Option[Template] =
    templates.get(path)
}