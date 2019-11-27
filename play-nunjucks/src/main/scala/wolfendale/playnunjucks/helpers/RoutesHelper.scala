package wolfendale.playnunjucks.helpers
import java.lang.reflect.Method

import better.files._
import cats.data.State
import cats.implicits._
import javax.inject.Inject
import play.api.Environment
import play.api.mvc.{Call, RequestHeader}
import wolfendale.nunjucks.expression.runtime.Value

import scala.util.{Failure, Success, Try}

case class RouteCallable(routerClass: Class[_], routerInstance: AnyRef, method: Method) {

  def asFunction(implicit request: RequestHeader): Value.Function = {
    Value.Function { params =>
      val args = params.getAll.map(_.toStr.value)

      val result = Try {
        method.invoke(routerInstance, args: _*).asInstanceOf[Call]
      }

      val obj = result match {
        case Success(call) => Value.Obj(
          Map(
            "url" -> Value.Str(call.url),
            "relative" -> Value.Str(call.relative),
            "absoluteURL" -> Value.Str(call.absoluteURL())
          )
        )
        case Failure(e)   => throw new RuntimeException(s"Unable to call route ($e)", e)
      }

      State.pure(obj)
    }
  }

}

class RoutesHelper @Inject()(environment: Environment) extends PlayHelper {

  val cache: Map[String, RouteCallable] = buildCache()

  override def value(implicit request: RequestHeader): Value = {
    // routes.controllers.PresentationOfficeController.onSubmit(mrn, mode).url
    // routes('controllers.PresentationOfficeController.onSubmit(mrn, mode).url')

    val list = cache.map {
      case (key: String, call: RouteCallable) => {
        val split = key.split("\\.").toList
        split.foldRight(call.asFunction: Value) {
          case (key, child) => Value.Obj(key -> child)
        }
      }
    }.toList

    list.foldRight(Map[String, Value]())((x, acc) => {
      x.properties
    })

    println(list)

    val result = Value.Obj()

//    println(result.properties)

    result
  }

  private def buildCache() = {
    val paths = environment.rootPath.toScala
      .globRegex("target/.*/routes/main/.*/routes.java".r)
      .toList
      .map(environment.rootPath.toScala.relativize)
      .map(_.toString.replaceFirst(".*?routes/main/", ""))
      .map(_.replaceAll("\\.java", ""))
      .map(_.replaceAll("/", "."))

    paths
      .flatMap { path =>
        val clazz  = Class.forName(path, false, environment.classLoader)
        val fields = clazz.getDeclaredFields.toList
        fields.map { field =>
          val inst        = clazz.newInstance()
          val router      = field.get(inst)
          val routerClass = router.getClass
          routerClass.getMethods.toList
            .filter(_.getReturnType == classOf[play.api.mvc.Call])
            .map(
              call =>
//                (s"controllers.${field.getName}.${call.getName}".replaceAll("\\.", ""),
                (s"controllers.${field.getName}.${call.getName}",
                 RouteCallable(
                   routerClass,
                   router,
                   call
                 )))
            .toMap
        }
      }
      .foldRight(Map[String, RouteCallable]())((x, acc) => acc ++ x)
  }
}
