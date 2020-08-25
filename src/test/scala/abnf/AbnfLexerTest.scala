package abnf

import abnf.AssignmentDef._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AbnfLexerTest extends AnyFunSuite with Matchers {

  test("sentence empty") {
    AbnfLex.parse(AbnfLex.wsp, " ").get should be(WSP(" "))
  }

  test("assignment") {
    AbnfLex.parse(AbnfLex.assignment, "=").get should be(ASSIGNMENT(Assignment))
  }

  test("incremental assignment") {
    AbnfLex.parse(AbnfLex.incrementalAssignment, "=/").get should be(ASSIGNMENT(Incremental))
  }

  test("digit") {
    AbnfLex.parse(AbnfLex.char, "1").get should be(VCHAR("1"))
  }

  test("quoted string") {
    AbnfLex.parse(AbnfLex.quotedString, "\"this is a test\"").get should be(QUOTEDSTRING("this is a test"))
  }

  test("prose val") {
    AbnfLex.parse(AbnfLex.prose, "<this is a test>").get should be(PROSE("this is a test"))
  }

  test("visible char") {
    AbnfLex.parse(AbnfLex.char, "c").get should be(VCHAR("c"))
  }

  test("new line") {
    AbnfLex.parse(AbnfLex.crlf,
      """
        |
        |""".stripMargin).get should be(CRLF())
  }
}
