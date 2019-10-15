package wolfendale.nunjucks

import wolfendale.nunjucks.expression.runtime.Value
import wolfendale.nunjucks.expression.runtime.Value.Function

abstract class Filter {

  def apply(scope: Frame, value: Value, args: Function.Parameters): Value
}

object Filter {

  def apply(f: Value => Value): Filter =
    new Filter {
      override def apply(scope: Frame, value: Value, args: Function.Parameters): Value =
        f(value)
    }

  def apply(f: (Value, Function.Parameters) => Value): Filter =
    new Filter {
      override def apply(scope: Frame, value: Value, args: Function.Parameters): Value =
        f(value, args)
    }
}
