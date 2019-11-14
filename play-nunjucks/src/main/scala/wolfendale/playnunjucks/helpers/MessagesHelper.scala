package wolfendale.playnunjucks.helpers

import cats.data.State
import javax.inject.Inject
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.RequestHeader
import wolfendale.nunjucks.expression.runtime.Value

class MessagesHelper @Inject()(
    val messagesApi: MessagesApi
) extends PlayHelper with I18nSupport {

  override def value(implicit request: RequestHeader): Value = {

    Value.Function { params =>
      val key = params.get(0).getOrElse(Value.Undefined).toStr.value
      State.pure(Value.Str(Messages(key)))
    }
  }
}
