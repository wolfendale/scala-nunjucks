package wolfendale.playnunjucks.helpers

import play.api.mvc.RequestHeader
import wolfendale.nunjucks.expression.runtime.Value

abstract class PlayHelper {
  def value(implicit request: RequestHeader): Value
}
