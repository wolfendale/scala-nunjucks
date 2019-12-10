package wolfendale.playnunjucks.helpers
import java.lang.reflect.Method

import cats.data.State
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
        case Success(call) =>
          Value.Obj(
            Map(
              "url"         -> Value.Str(call.url),
              "relative"    -> Value.Str(call.relative),
              "absoluteURL" -> Value.Str(call.absoluteURL())
            )
          )
        case Failure(e) => throw new RuntimeException(s"Unable to call route ($e)", e)
      }

      State.pure(obj)
    }
  }

}

class RoutesHelper @Inject()(environment: Environment) extends PlayHelper {

  val cache: Tree[String, RouteCallable] = buildCache()

  override def value(implicit request: RequestHeader): Value = {
    convertToObj(cache)
  }

  private def buildCache() = {
    Package.getPackages.toList
      .map(_.getName)
      .flatMap(p =>
        Try[Class[$0] forSome { type $0 }] {
          Class.forName(s"$p.routes")
        }.toOption)
      .flatMap { clazz =>
        val fields = clazz.getDeclaredFields.toList
        fields
          .map { field =>
            val inst        = clazz.newInstance()
            val router      = field.get(inst)
            val routerClass = router.getClass
            routerClass.getMethods.toList
              .filter(_.getReturnType == classOf[play.api.mvc.Call])
              .map(
                call =>
                  (s"${routerClass.getPackage.getName}.${field.getName}.${call.getName}",
                   RouteCallable(
                     routerClass,
                     router,
                     call
                   )))
              .toMap
          }
      }
      .flatMap(_.map {
        case (key: String, call: RouteCallable) => {
          Tree[String, RouteCallable](key.split("\\.").toList, call)
        }
      })
      .foldRight(Tree.empty: Tree[String, RouteCallable]) { (tree, acc) =>
        acc.combine(tree)
      }
  }

  private def convertToObj(tree: Tree[String, RouteCallable])(implicit request: RequestHeader): Value = {
    tree match {
      case Leaf(value) => value.asFunction
      case Node(nodes) =>
        Value.Obj(nodes.map {
          case (key, tree) =>
            (key, convertToObj(tree))
        })
    }
  }

}

sealed trait Tree[A, B] {
  def get(): Option[B]         = get(List.empty)
  def get(keys: A*): Option[B] = get(keys.toList)
  def get(keys: List[A]): Option[B]
  def combine(another: Tree[A, B]): Tree[A, B]
}

case class Leaf[A, B](value: B) extends Tree[A, B] {
  override def get(keys: List[A]): Option[B] = keys match {
    case Nil => Some(value)
    case _   => None
  }

  override def combine(another: Tree[A, B]): Tree[A, B] = {
    throw new RuntimeException("Key collision: cannot merge with a leaf node")
  }
}

case class Node[A, B](nodes: Map[A, Tree[A, B]]) extends Tree[A, B] {
  override def get(keys: List[A]): Option[B] = keys match {
    case x :: xs => nodes.get(x).flatMap(_.get(xs))
    case _       => None
  }

  override def combine(another: Tree[A, B]): Tree[A, B] = {
    another match {
      case Leaf(_)          => throw new RuntimeException("Key collision: cannot merge with a leaf node")
      case Node(otherNodes) => Node(combineMaps(otherNodes))
    }
  }

  private def combineMaps(otherNodes: Map[A, Tree[A, B]]): Map[A, Tree[A, B]] = {
    val map = otherNodes.map {
      case (key: A, node: Tree[A, B]) => {
        (key, nodes.get(key).map(_.combine(node)).getOrElse(node))
      }
    }
    map ++ nodes.filter { case (key: A, _: Tree[A, B]) => !otherNodes.contains(key) }
  }
}

object Tree {

  def empty[A, B]: Tree[A, B] = Node[A, B](Map.empty)

  def apply[A, B](key: A, value: B): Tree[A, B] = Node(Map(key -> Leaf(value)))

  def apply[A, B](keys: List[A], value: B): Tree[A, B] = keys.foldRight(Leaf(value): Tree[A, B]) { (key, maps) =>
    Node(Map(key -> maps))
  }

}
