package wolfendale.nunjucks

import java.nio.file.Paths

abstract class Loader(roots: List[String]) {

  def load(path: String): Option[Template]

  final def resolveAndLoad(path: String, caller: Option[String]): Either[List[String], Loader.ResolvedTemplate] = {

    val relative = caller.flatMap {
      caller =>
        for {
          caller <- Option(Paths.get(caller))
          resolved <- Option(caller.resolveSibling(path))
        } yield resolved.normalize.toString
    }

    val prefixed = roots.flatMap {
      root =>
        for {
          root <- Option(Paths.get(root))
          resolved <- Option(root.resolve(path))
        } yield resolved.normalize.toString
    }

    val paths = relative.fold(prefixed)(_ :: prefixed)

    paths.flatMap {
      path =>
        load(path).map(Loader.ResolvedTemplate(path, _))
    }.headOption.toRight(paths)
  }

  final def resolve(path: String, caller: Option[String]): Either[List[String], String] =
    resolveAndLoad(path, caller).right.map(_.path)

  @deprecated
  def load(to: String, from: Option[String] = None): Option[Template] =
    Loader.resolveSibling(to, from).flatMap(load)
}

object Loader {

  final case class ResolvedTemplate(path: String, template: Template)

  @deprecated
  def resolveSibling(to: String, from: Option[String]): Option[String] =
    from
      .flatMap(f => Option(Paths.get(f)))
      .flatMap(f => Option(f.getParent))
      .map { parent =>
        Some(parent.resolve(to).normalize.toString)
      }
      .getOrElse(Option(Paths.get(to).normalize.toString))

  @deprecated
  def resolveChild(to: String, from: Option[String]): Option[String] =
    from
      .flatMap(f => Option(Paths.get(f)))
      .map { parent =>
        parent.resolve(to).normalize.toString
      }
}

class ProvidedLoader(templates: Map[String, Template] = Map.empty) extends Loader(List("")) {

  override def load(path: String): Option[Template] =
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
