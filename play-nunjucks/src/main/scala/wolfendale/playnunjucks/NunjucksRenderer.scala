package wolfendale.playnunjucks

import java.util.concurrent.{ExecutorService, Executors}

import javax.inject.Inject
import play.api.libs.json.JsObject
import play.api.mvc.{MessagesRequestHeader, RequestHeader}
import play.twirl.api.Html
import wolfendale.nunjucks.{Environment, ResourcesLoader}

import scala.concurrent.{ExecutionContext, Future}

class NunjucksRenderer @Inject()(
                                  configuration: NunjucksConfiguration,
                                  nunjucksEnvironment: PlayNunjucksEnvironment
                                ) {

  private val threadPool: ExecutorService = {
    Executors.newFixedThreadPool(configuration.threadCount)
  }

  private val executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(threadPool)

  def render(template: String)(implicit request: RequestHeader): Future[Html] = {
    render(template, JsObject(Seq.empty))
  }

  def render(template: String, ctx: JsObject)(implicit request: RequestHeader): Future[Html] = {
    Future {
      nunjucksEnvironment.render(template, ctx)
    }(executionContext)
  }
}
