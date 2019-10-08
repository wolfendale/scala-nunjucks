package wolfendale.nunjucks

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FreeSpec, MustMatchers}

class EqualitySpec
    extends FreeSpec
    with MustMatchers
    with TableDrivenPropertyChecks {

  import wolfendale.nunjucks.expression.runtime.Value._

  val table = Table(
    ("left", "right", "result"),
    (Undefined, Undefined, True),
    (Undefined, Null, True),
    (Undefined, Number(1), False),
    (Undefined, Str("foo"), False),
    (Undefined, True, False),
    (Undefined, Obj.empty, False),
    (Null, Undefined, True),
    (Null, Null, True),
    (Null, Number(1), False),
    (Null, Str("foo"), False),
    (Null, True, False),
    (Null, Obj.empty, False),
    (Number(1), Undefined, False),
    (Number(1), Null, False),
    (Number(1), Number(1), True),
    (Number(1), Number(2), False),
    (Number(1), Str("foo"), False),
    (Number(1), Str("1"), True),
    (Number(1), True, True),
    (Number(1), False, False),
    (Number(1), Obj.empty, False),
    (Str("foo"), Undefined, False),
    (Str("foo"), Null, False),
    (Str("1"), Number(1), True),
    (Str("foo"), Number(2), False),
    (Str("1.337"), Number(1.337), True),
    (Str("foo"), Str("foo"), True),
    (Str("foo"), Str("1"), False),
    (Str("true"), True, True),
    (Str("foo"), False, False),
    (Str("foo"), Obj.empty, False),
    (Str("[object Object]"), Obj.empty, True),
    (True, Undefined, False),
    (True, Null, False),
    (True, Number(1), True),
    (True, Number(0), False),
    (False, Number(1), False),
    (False, Number(0), True),
    (True, Str("foo"), False),
    (False, Str(""), True),
    (True, Obj.empty, False)
  )

  "equality must work as expected" in {

    forAll(table) { (left, right, result) =>
      (left `==` right) mustEqual result
    }
  }
}
