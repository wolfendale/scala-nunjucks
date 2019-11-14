package wolfendale.playnunjucks

import play.api.inject.{Binding, Module}
import play.api.{Configuration, Environment}
import wolfendale.playnunjucks.helpers.{PlayHelpers, PlayHelpersProvider}

class NunjucksModule extends Module {

  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] =
    Seq(
      bind[NunjucksRenderer].toSelf.eagerly,
      bind[NunjucksConfiguration].to[DefaultNunjucksConfiguration],
      bind[PlayHelpers].toProvider[PlayHelpersProvider]
    )
}
