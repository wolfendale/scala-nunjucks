package wolfendale.playnunjucks

import play.api.inject.{Binding, Module}
import play.api.{Configuration, Environment, Mode}

class NunjucksModule extends Module {

  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] =
    Seq(
      bind[NunjucksRenderer].toSelf.eagerly,
      bind[NunjucksConfiguration].to[DefaultNunjucksConfiguration]
    )
}
