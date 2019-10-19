package controllers

import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import play.twirl.api.Html
import wolfendale.nunjucks.{Environment, ResourcesLoader}

import scala.concurrent.ExecutionContext

class TestController @Inject()(
    cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends AbstractController(cc)
    with I18nSupport {

  private val loader = new ResourcesLoader(List(
    "/views",
    "/META-INF/resources/webjars/govuk-frontend/3.1.0"
  ))

  private val nunjucks = new Environment(loader)

  def get: Action[AnyContent] = Action { implicit request =>
    Ok(Html(nunjucks.renderTemplate("test.njk").get))
  }
}
