package wolfendale.playnunjucks

import javax.inject.Inject
import play.api.Configuration

class NunjucksConfiguration(
                             val threadCount: Int,
                             val paths: Seq[String]
                           )

class DefaultNunjucksConfiguration @Inject()(configuration: Configuration) extends NunjucksConfiguration(
  configuration.get[Int]("nunjucks.threadCount"),
  configuration.get[Seq[String]]("nunjucks.viewPaths")
)
