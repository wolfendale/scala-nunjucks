package wolfendale.playnunjucks.helpers

import javax.inject.{Inject, Provider}

final case class PlayHelpers(helpers: Map[String, PlayHelper])

class PlayHelpersProvider @Inject()(
    messagesHelper: MessagesHelper,
    routesHelper: RoutesHelper
) extends Provider[PlayHelpers] {

  override def get(): PlayHelpers =
    PlayHelpers(
      Map(
        "messages" -> messagesHelper,
        "routes"   -> routesHelper
      ))
}
