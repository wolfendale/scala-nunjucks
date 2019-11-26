package controllers.anotherpackage

import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import wolfendale.playnunjucks.NunjucksRenderer

import scala.concurrent.ExecutionContext

class AnotherController @Inject()(
    cc: ControllerComponents,
    renderer: NunjucksRenderer
)(implicit ec: ExecutionContext)
    extends AbstractController(cc)
    with I18nSupport {

  def anotherObviousName: Action[AnyContent] = Action.async { implicit request =>
    renderer.render("test.njk", Json.obj("eventName" -> "error.summary.title")).map(Ok(_))
  }
}
