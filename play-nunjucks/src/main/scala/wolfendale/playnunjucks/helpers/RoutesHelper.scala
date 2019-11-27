package wolfendale.playnunjucks.helpers
import better.files._
import cats.data.State
import javax.inject.Inject
import play.api.Environment
import play.api.mvc.{Call, RequestHeader}
import wolfendale.nunjucks.expression.runtime.Value

import scala.util.{Failure, Success, Try}

case class RouteCall(packageRoot: String, controller: String, method: String, arguments: Seq[String], urlType: String)

object RouteCall {

  def parseArgs(args: Option[String]): Seq[String] = {
    args match {
      case Some(x) => x.split(",").toList.map(_.trim)
      case None    => Seq.empty
    }
  }

}

class RoutesHelper @Inject()(environment: Environment) extends PlayHelper {

  override def value(implicit request: RequestHeader): Value = {
    Value.Function { params =>
      val input = params
        .get(0)
        .map(_.toStr.value)
        .getOrElse("")

      val extract = "routes\\.(.+)\\.(.+)\\.([\\w^(]+)(?:\\(\\)|(?:\\(([\\w, ]+)\\))?)\\.(.+)".r

      val routeCall = input match {
        case extract(packageRoot, controller, method, arguments, urlType) =>
          RouteCall(packageRoot, controller, method, RouteCall.parseArgs(Option(arguments)), urlType)
      }

      val path = environment.rootPath.toScala
        .globRegex("target/.*/routes/main/.*/routes.java".r)
        .toList
        .map(environment.rootPath.toScala.relativize)
        .map(_.toString.replaceFirst(".*?routes/main/", ""))
        .map(_.replaceAll("\\.java", ""))
        .map(_.replaceAll("/", "."))
        .find(_ == (routeCall.packageRoot + ".routes"))

      val result = path.map(p =>
        Try {
          val clazz       = Class.forName(p, false, environment.classLoader)
          val field       = clazz.getDeclaredField(routeCall.controller)
          val inst        = clazz.newInstance()
          val router      = field.get(inst)
          val routerClass = router.getClass
          val argsClasses = Array.fill(routeCall.arguments.length) { classOf[String] }
          val method      = routerClass.getMethod(routeCall.method, argsClasses: _*)
          val call        = method.invoke(router, routeCall.arguments: _*).asInstanceOf[Call]

          routeCall.urlType match {
            case "url"         => call.url
            case "relative"    => call.relative
            case "absoluteURL" => call.absoluteURL
          }
      })

      val url: String = result match {
        case Some(Success(url)) => url
        case Some(Failure(e))   => throw new RuntimeException(s"Unable to create route ($e)", e)
        case None => {
          throw new RuntimeException(s"Unable to locate route file in package: ${routeCall.packageRoot}")
        }
      }

      State.pure(Value.Str(url.toString))
    }

  }
}
