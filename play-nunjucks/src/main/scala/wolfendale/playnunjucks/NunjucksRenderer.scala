package wolfendale.playnunjucks

import java.util.concurrent.{ExecutorService, Executors}

import javax.inject.Inject
import play.api.libs.json.JsObject
import play.api.mvc.RequestHeader
import play.twirl.api.Html
import wolfendale.nunjucks.{Environment, ResourcesLoader}

import scala.concurrent.{ExecutionContext, Future}

class NunjucksRenderer @Inject()(
                                  configuration: NunjucksConfiguration,
                                  jsonConverter: PlayJsonConverter
                                ) {

  private val threadPool: ExecutorService = {
    Executors.newFixedThreadPool(configuration.threadCount)
  }

  private val executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(threadPool)

  private val loader = new ResourcesLoader(configuration.paths.toList)

  private val nunjucks = new Environment(loader)

  def render(template: String)(implicit request: RequestHeader): Future[Html] = {
    render(template, JsObject(Seq.empty))
  }

  def render(template: String, ctx: JsObject)(implicit request: RequestHeader): Future[Html] = {
    Future {
      Html(nunjucks.renderTemplate(template, jsonConverter.convertObject(ctx)).getOrElse(s"Missing template: $template"))
    }(executionContext)
  }

}
