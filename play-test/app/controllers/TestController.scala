package controllers

import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import wolfendale.playnunjucks.NunjucksRenderer

import scala.concurrent.ExecutionContext

class TestController @Inject()(
    cc: ControllerComponents,
    renderer: NunjucksRenderer
)(implicit ec: ExecutionContext)
    extends AbstractController(cc)
    with I18nSupport {

  def obviousName: Action[AnyContent] = Action.async { implicit request =>
    renderer.render("test.njk", Json.obj("eventName" -> "date.title")).map(Ok(_))
  }

  def methodWithArgs(ref: String): Action[AnyContent] = Action.async { implicit request =>
    renderer.render("test.njk", Json.obj("eventName" -> "date.title")).map(Ok(_))
  }

  def methodWithTwoArgs(ref: String, ref2: String): Action[AnyContent] = Action.async { implicit request =>
    renderer.render("test.njk", Json.obj("eventName" -> "date.title")).map(Ok(_))
  }
}
