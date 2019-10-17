package wolfendale.nunjucks.expression

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.ExpressionTester._
import wolfendale.nunjucks.expression.runtime.Value
import wolfendale.nunjucks.expression.runtime.Value.RegexFlag._

class RegexSpec extends FreeSpec with MustMatchers {

  val tester = new ExpressionTester()
  val pattern = "test"

  "a Regex" - {
    "must implement the eval method to succeed on patterns without a flag" in {
      tester.evaluate(s"r/$pattern/") mustBe Value.Regex(pattern)
    }
    "must implement the eval method to succeed on valid flags" in {
      tester.evaluate(s"r/$pattern/gimy") mustBe Value.Regex(pattern, Set(
        ApplyGlobally,
        CaseInsensitive,
        MultiLine,
        Sticky
      ))
    }
    "must implement the eval method to return parse error for invalid flags" in {
      val optFailure = tester.ast(s"r/test/k").optFailure
      optFailure mustBe defined
      optFailure.get.msg must include("""found "k"""")
    }
    "must implement the eval method to return parse error for duplicate valid flags" in {
      val optFailure = tester.ast(s"r/$pattern/ii").optFailure
      optFailure mustBe defined
      optFailure.get.msg must include("""found "i"""")
    }
    "must implement the == operator as an instance equality check" in {
      tester.evaluate(s"r/$pattern/ == r/$pattern/") mustBe Value.False
      tester.evaluate("regex == regex", scope = Value.Obj("regex" -> Value.Regex(pattern))) mustBe Value.True
    }

    "must implement the source method to return the entire pattern" in {
      tester.evaluate(s"regex.source", scope = Value.Obj("regex" -> Value.Regex(pattern))).toStr mustBe Value.Str(pattern)
    }
    "must implement the source method to return an override for the empty pattern" in {
      tester.evaluate(s"regex.source", scope = Value.Obj("regex" -> Value.Regex(""))).toStr mustBe Value.Str("(?:)")
    }
    "must implement the toStr method to return the entire pattern" in {
      tester.evaluate(s"r/$pattern/").toStr mustBe Value.Str(s"/$pattern/")
    }
    "must implement the toStr method to return the entire pattern with all the flags in alphabetical order" in {
      tester.evaluate(s"r/$pattern/imyg").toStr mustBe Value.Str(s"/$pattern/gimy")
    }
    "must implement the toStr method to return an override for the empty pattern" in {
      tester.evaluate(s"r//").toStr mustBe Value.Str("/(?:)/")
    }
    "must work with the + operator and be treated as a string when all values are regex" in {
      tester.evaluate("regex + regex", scope = Value.Obj("regex" -> Value.Regex(""))) mustBe Value.Str("/(?:)//(?:)/")
    }
    "must work with the + operator and be treated as a string when other values are not regexes" in {
      tester.evaluate("1 + regex + 2", scope = Value.Obj("regex" -> Value.Regex(""))) mustBe Value.Str("1/(?:)/2")
    }
    "must implement the toBool method to always return true" in {
      tester.evaluate(s"r/$pattern/").toBool mustBe Value.True
    }
    "must implement the toNumeric method to always return NaN" in {
      tester.evaluate(s"r/$pattern/").toNumeric mustBe Value.NaN
    }
    "must output the correct flags set" - {
      val patternWithoutFlag = Value.Obj("regex" -> Value.Regex(pattern))
      val patternWithAllFlag = Value.Obj("regex" -> Value.Regex(pattern, Set(ApplyGlobally, CaseInsensitive, MultiLine, Sticky)))

      "must output flags correctly" in {
        tester.evaluate(s"""regex.flags""", scope = patternWithoutFlag) mustBe Value.Str("")
        tester.evaluate(s"""regex.flags""", scope = patternWithAllFlag) mustBe Value.Str("gimy")
      }
      "must output global correctly" in {
        tester.evaluate(s"""regex.global""", scope = patternWithoutFlag) mustBe Value.False
        tester.evaluate(s"""regex.global""", scope = patternWithAllFlag) mustBe Value.True
      }
      "must output ignoreCase correctly" in {
        tester.evaluate(s"""regex.ignoreCase""", scope = patternWithoutFlag) mustBe Value.False
        tester.evaluate(s"""regex.ignoreCase""", scope = patternWithAllFlag) mustBe Value.True
      }
      "must output multiline correctly" in {
        tester.evaluate(s"""regex.multiline""", scope = patternWithoutFlag) mustBe Value.False
        tester.evaluate(s"""regex.multiline""", scope = patternWithAllFlag) mustBe Value.True
      }
      "must output sticky correctly" in {
        tester.evaluate(s"""regex.sticky""", scope = patternWithoutFlag) mustBe Value.False
        tester.evaluate(s"""regex.sticky""", scope = patternWithAllFlag) mustBe Value.True
      }
    }
    "must implement the test function" - {
      val patternWithoutFlag = Value.Obj("regex" -> Value.Regex(pattern))
      "which evaluates correctly when called without a named parameter" in {
        tester.evaluate(s"""regex.test('$pattern') """, scope = patternWithoutFlag) mustBe Value.True
        tester.evaluate(s"""regex.test('') """, scope = patternWithoutFlag) mustBe Value.False
      }
      "which evaluates correctly when called with the named parameter str" in {
        tester.evaluate(s"""regex.test(str = "$pattern") """, scope = patternWithoutFlag) mustBe Value.True
        tester.evaluate(s"""regex.test(str = "") """, scope = patternWithoutFlag) mustBe Value.False
      }
      "which evaluates case insensitive flag correctly" in {
        val patternWithCaseInsensitiveFlag = Value.Obj("regex" -> Value.Regex(pattern, Set(CaseInsensitive)))
        tester.evaluate(s"""regex.test('${pattern.toUpperCase}')""", scope = patternWithoutFlag) mustBe Value.False
        tester.evaluate(s"""regex.test('${pattern.toUpperCase}')""", scope = patternWithCaseInsensitiveFlag) mustBe Value.True
      }
      "which outputs multiline flag correctly" in {
        val patternWithoutFlag = Value.Obj("regex" -> Value.Regex((s"^$pattern")))
        val patternWithMultiLineFlag = Value.Obj("regex" -> Value.Regex(s"^$pattern", Set(MultiLine)))
        tester.evaluate(s"regex.multiline", scope = patternWithoutFlag) mustBe Value.False
        tester.evaluate(s"regex.multiline", scope = patternWithMultiLineFlag) mustBe Value.True
      }
      "which evaluates multiline flag correctly" in {
        val patternWithoutFlag = Value.Obj("regex" -> Value.Regex((s"^$pattern")))
        val patternWithMultiLineFlag = Value.Obj("regex" -> Value.Regex(s"^$pattern", Set(MultiLine)))
        tester.evaluate(s"""regex.test('\n$pattern')""", scope = patternWithoutFlag) mustBe Value.False
        tester.evaluate(s"""regex.test('\n$pattern')""", scope = patternWithMultiLineFlag) mustBe Value.True
      }
    }
  }

}
