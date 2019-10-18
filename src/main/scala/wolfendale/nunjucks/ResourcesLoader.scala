package wolfendale.nunjucks

import scala.io.Source

class ResourcesLoader(roots: Seq[String]) extends Loader {

  private def compile(template: String): Template = {
    import fastparse._
    parse(template, TemplateParser.template(_)).get.value
  }

  override protected def load(path: String): Option[Template] =
    roots
      .flatMap(root => Loader.resolveChild(path, Some(root)))
      .flatMap(path => Option(getClass.getResourceAsStream(path)))
      .headOption
      .map(source => compile(Source.fromInputStream(source).mkString))
}
