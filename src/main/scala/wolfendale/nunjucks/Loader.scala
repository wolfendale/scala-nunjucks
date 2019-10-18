package wolfendale.nunjucks

import java.nio.file.Paths

abstract class Loader {

  protected def load(path: String): Option[Template]

  def load(to: String, from: Option[String] = None): Option[Template] =
    Loader.resolveSibling(to, from).flatMap(load)
}

object Loader {

  def resolveSibling(to: String, from: Option[String]): Option[String] =
    from
      .flatMap(f => Option(Paths.get(f)))
      .flatMap(f => Option(f.getParent))
      .map {
        parent =>
          Some(parent.resolve(to).normalize.toString)
      }.getOrElse(Some(to))

  def resolveChild(to: String, from: Option[String]): Option[String] =
    from
      .flatMap(f => Option(Paths.get(f)))
      .map {
        parent =>
        parent.resolve(to).normalize.toString
      }
}

class ProvidedLoader(templates: Map[String, Template] = Map.empty) extends Loader {

  override protected def load(path: String): Option[Template] =
    templates.get(path)

  private def compile(template: String): Template = {
    import fastparse._
    parse(template, TemplateParser.template(_)).get.value
  }

  def add(name: String, template: Template): ProvidedLoader =
    new ProvidedLoader(templates + (name -> template))

  def add(name: String, template: String): ProvidedLoader =
    new ProvidedLoader(templates + (name -> compile(template)))
}