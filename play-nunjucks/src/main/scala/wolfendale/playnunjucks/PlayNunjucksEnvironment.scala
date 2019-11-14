package wolfendale.playnunjucks

import javax.inject.Inject
import play.api.libs.json.JsObject
import play.api.mvc.RequestHeader
import play.twirl.api.Html
import wolfendale.nunjucks.{Environment, ResourcesLoader}
import wolfendale.playnunjucks.helpers.PlayHelpers

class PlayNunjucksEnvironment @Inject()(
    configuration: NunjucksConfiguration,
    converter: PlayJsonConverter,
    helpers: PlayHelpers
) {

  private val loader = new ResourcesLoader(configuration.paths.toList)

  private val nunjucks = new Environment(loader)

  def render(template: String, context: JsObject)(implicit request: RequestHeader): Html = {

    val env = helpers.helpers.foldLeft(nunjucks) {
      case (env, (name, helper)) =>
        env.addGlobal(name, helper.value)
    }

    Html(env.renderTemplate(template, converter.convertObject(context)).get)
  }
}
