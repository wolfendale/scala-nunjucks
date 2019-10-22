package wolfendale.nunjucks

import scala.io.Source

class ResourcesLoader(roots: List[String]) extends Loader(roots) {

  private def compile(template: String): Template = {
    import fastparse._
    parse(template, TemplateParser.template(_)).get.value
  }

  override def load(path: String): Option[Template] =
    Option(getClass.getResourceAsStream(path)).map {
      stream =>
        compile(Source.fromInputStream(stream).mkString)
    }
}
