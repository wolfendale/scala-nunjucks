package wolfendale.nunjucks.filters

import org.scalatest.{FreeSpec, MustMatchers}
import wolfendale.nunjucks.expression.ExpressionTester
import wolfendale.nunjucks.expression.runtime.Value
import org.scalacheck.{Properties, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import wolfendale.nunjucks.Frame


object IndentProperties extends Properties("Indent filter") {
    val tester = new ExpressionTester()

    val singleLineStringLiteral: Gen[String] = 
        (Arbitrary.arbitrary[String].map(x => 
        x.replace("\n", "")
          .replace("\r", "")
          .replace("\\", "\\\\")
          .replace("\"", "\\\""))).suchThat(!_.contains("\n"))

    val singleOrMultiLineString: Gen[String] = (for {
        noOfLines <- Gen.choose(0, 100)
        lines <- Gen.listOfN(noOfLines, singleLineStringLiteral)
    } yield lines.mkString("\n"))

    val multiLineString: Gen[String] = singleOrMultiLineString suchThat (_.contains("\n"))

    def indentCallAsString(count: Int, indentFirst: Boolean) = s"indent(${count}, ${indentFirst})"

    property("Should not change single line strings when indentFirstLine is false") = 
    forAll(arbitrary[Int], singleLineStringLiteral) {(count, input) =>
        tester.evaluate("\"" + input + "\" | " + indentCallAsString(count, false)).toStr.value == input
    }

    property("When the input has multiple lines the output should contain more characters than the input") = 
    forAll(arbitrary[Int] suchThat(_ > 0), arbitrary[Boolean], multiLineString) {(count, indentFirstLine, input) =>
        tester.evaluate("\"" + input + "\" | " + indentCallAsString(count, indentFirstLine)).toStr.value.length() > input.length()
    }

    val countPairGen = for {
        a <- Gen.choose(0, 1000)
        b <- Gen.choose(0, 1000 - a)
    } yield (a, b)
    property("subsequent applications are the same as addition") = 
    forAll(countPairGen, arbitrary[Boolean], singleOrMultiLineString) {(counts, indentFirstLine, input) => 
        tester.evaluate("\"" + input + "\" | " + 
            indentCallAsString(counts._1, indentFirstLine) + " | " + 
            indentCallAsString(counts._2, indentFirstLine )).toStr.value == 
        tester.evaluate("\"" + input + "\" | " + 
            indentCallAsString(counts._1 + counts._2, indentFirstLine)).toStr.value
    }

    property("Adds only spaces") =
    forAll(arbitrary[Int], arbitrary[Boolean], singleOrMultiLineString suchThat(!_.contains(' '))) {(count, indentFirstLine, input) => 
        tester.evaluate("\"" + input + "\" | " + indentCallAsString(count, indentFirstLine)).toStr.value.replace(" ", "") == input
    }
    property("orignal lines are preserved as tails of new lines") = 
    forAll(arbitrary[Int], arbitrary[Boolean], singleOrMultiLineString) {(count, indentFirstLine, input) => {
        tester.evaluate(s""""${input}" | indent """)
            .toStr.value.split('\n')
            .zip(input.split('\n'))
            .forall {
                case (indented, original) => indented.takeRight(original.length) == original
            }
    }}

    property("indenting by more than 1000 characters is the same as indenting by 1000") = 
    forAll(Gen.chooseNum(1001, Int.MaxValue), arbitrary[Boolean], singleOrMultiLineString) {(count, indentFirstLine, input) => {
        val x = tester.evaluate(s""""${input}" | indent(${count}, ${indentFirstLine})""").toStr.value
        val y = tester.evaluate(s""""${input}" | indent(1000, ${indentFirstLine})""").toStr.value
        x == y
    }}
    
    property("indenting by less than 0 characters is the same as indenting by 0") =
    forAll(Gen.chooseNum(Int.MinValue, -1), arbitrary[Boolean], singleOrMultiLineString) {(count, indentFirstLine, input) => {
        val x = tester.evaluate(s""""${input}" | indent(${count}, ${indentFirstLine})""").toStr.value
        val y = tester.evaluate(s""""${input}" | indent(0, ${indentFirstLine})""").toStr.value
        x == y
    }}

    property("calling without parameters behaves exactly like calling with parameters (2, false)") = 
    forAll(singleLineStringLiteral) {(i:String) => {
        val x = tester.evaluate("\"" + i + "\" | indent").toStr.value 
        val y = tester.evaluate("\"" + i + "\" | indent(2, false)").toStr.value
        x == y
    }}

    property("calling without second parameter behaves exactly like calling with second parameter set to false") = 
    forAll(arbitrary[Int], singleLineStringLiteral) {(count : Int, i:String) => 
        tester.evaluate("\"" + i + "\" | indent(" + count  + ")").toStr.value == 
        tester.evaluate("\"" + i + "\" | indent(" + count + ", false)").toStr.value
    }
}

class IndentCaseTests extends FreeSpec with MustMatchers {
    val tester = new ExpressionTester()

    "Should indent second and subsequent lines" in {
        tester.evaluate("\"aaaa\nbbbb\ncccc\" | indent") mustEqual Value.Str("aaaa\n  bbbb\n  cccc")
    }

    "Cope with empty input" in {
        tester.evaluate("\"\" | indent") mustEqual Value.Str("")
    }

    "Can indent the first line" in {
        tester.evaluate("\"aaaa\nbbbb\nccc\" | indent(4, true)") mustEqual Value.Str("    aaaa\n    bbbb\n    cccc")
    }

    "cope with lines that start with tabs" in {
        tester.evaluate("\"\u0009boo\" | indent") mustEqual Value.Str("\u0009boo")
    }

    "cope with lines that start with tabs directly in the function" in {
        val emptyParams = Value.Function.Parameters(Nil)
        indent.apply(Frame.empty, Value.Str("\u0009boo"), emptyParams).toStr.value mustEqual "\u0009boo"
    }
}
