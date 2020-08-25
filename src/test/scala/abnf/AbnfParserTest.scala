package abnf

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class AbnfParserTest extends AnyFunSuite with Matchers {

  test("empty rule") {
    val Right(tokens) = AbnfLex(
      """t-1 =
        |""".stripMargin)
    AbnfParser(tokens) should be(Left(AbnfParserError(Location(1,6),"prose expected")))
  }

  test("invalid rule name first rule met") {
    val Right(tokens) = AbnfLex(
      """1t-1 =/ ASTE
        |v = Test
        |""".stripMargin)
    AbnfParser(tokens) should be(Left(AbnfParserError(Location(1,1),"alpha expected")))
  }

  test("invalid rule name") {
    val Right(tokens) = AbnfLex(
      """v = Test
        |1t-1 =/ ASTE
        |v = Test
        |""".stripMargin)
    AbnfParser(tokens) should be(Left(AbnfParserError(Location(2,1),"alpha expected")))
  }

  test("alternative rule with repetition and hex with lower case letter") {
    val Right(tokens) = AbnfLex(
      """UTF8-NONASCII   =  %xC0-DF 1UTF8-CONT
        |                /  %xF8-Fb 4UTF8-CONT
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("UTF8-NONASCII"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(192,Some(Left(223)))),
                                      ElementRep(Some(DigitRepeat(1)),Name("UTF8-CONT"))),
                                 List(ElementRep(None,Value(248,Some(Left(251)))),
                                      ElementRep(Some(DigitRepeat(4)),Name("UTF8-CONT")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("rules with incremental assignment") {
    val Right(tokens) = AbnfLex(
      """t-1 =/ ASTE
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                       AssignmentDef.Incremental,
                       List(List(ElementRep(None, Name("ASTE")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("rules with comment") {
    val Right(tokens) = AbnfLex(
      """digest-uri-value  =  rquest-uri ; Equal to request-uri as specified by HTTP/1.1
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("digest-uri-value"),
                       AssignmentDef.Assignment,
                       List(List(ElementRep(None, Name("rquest-uri")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("rules with numbers") {
    val Right(tokens) = AbnfLex(
      """t =/ test test1 / test2
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t"),
                            AssignmentDef.Incremental,
                            List(List(ElementRep(None, Name("test")), ElementRep(None, Name("test1"))),
                                 List(ElementRep(None, Name("test2")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("rule with surrounding comments") {
    val Right(tokens) = AbnfLex(
      """;this is a test
        |t =/ test test1 / test2 ; ca
        |""".stripMargin)
    val parsed = Rules( List(Rule(Name("t"),
                        AssignmentDef.Incremental,
                        List(List(ElementRep(None, Name("test")), ElementRep(None, Name("test1"))),
                             List(ElementRep(None, Name("test2")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("repetition rule - range - lower bound") {
    val Right(tokens) = AbnfLex(
      """t-1 = 1*test
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                       AssignmentDef.Assignment,
                       List(List(ElementRep(Some(RangeRepeat(Some(1), None)), Name("test")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("repetition rule - range - upper bound") {
    val Right(tokens) = AbnfLex(
      """t-1 = *1test
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                       AssignmentDef.Assignment,
                       List(List(ElementRep(Some(RangeRepeat(None, Some(1))), Name("test")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("repetition rule - range - lower and upper bound") {
    val Right(tokens) = AbnfLex(
      """t-1 = 1*2test
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                       AssignmentDef.Assignment,
                       List(List(ElementRep(Some(RangeRepeat(Some(1), Some(2))), Name("test")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("repetition rule - digit") {
    val Right(tokens) = AbnfLex(
      """t-1 = 1test
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                       AssignmentDef.Assignment,
                       List(List(ElementRep(Some(DigitRepeat(1)), Name("test")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("group rule") {
    val Right(tokens) = AbnfLex(
      """t-1 = (test test-cons)
        |""".stripMargin)
    val parsedGroup = Group(List(List(ElementRep(None, Name("test")), ElementRep(None, Name("test-cons")))))
    val parsed = Rules(List(Rule(Name("t-1"),
                       AssignmentDef.Assignment,
                       List( List(ElementRep(None, parsedGroup))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("optional rule") {
    val Right(tokens) = AbnfLex(
      """t-1 = [test test-cons] non-opt
        |""".stripMargin)
    val parsedOption = Opt(List(List(ElementRep(None, Name("test")), ElementRep(None, Name("test-cons")))))
    val parsed = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Assignment,
      List(List(ElementRep(None, parsedOption), ElementRep(None, Name("non-opt"))))))
    )
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("list of rules") {
    val Right(tokens) = AbnfLex(
      """t-1 =/ test
        |t-2 =/ test-1
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Incremental,
                            List(List(ElementRep(None, Name("test"))))),
                            Rule(Name("t-2"),
                            AssignmentDef.Incremental,
                            List(List(ElementRep(None, Name("test-1")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("comment between rules is discarded") {
    val Right(tokens) = AbnfLex(
      """t-1 =/ test
        |; this is a comment
        |
        |t-2 =/ test-1
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Incremental,
                            List(List(ElementRep(None, Name("test"))))),
                            Rule(Name("t-2"),
                            AssignmentDef.Incremental,
                            List(List(ElementRep(None, Name("test-1")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("quoted string") {
    val Right(tokens) = AbnfLex(
      """t-1 =/ "this is a test"
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Incremental,
                            List(List(ElementRep(None, CharVal("this is a test")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("alternatives on new line") {
    val Right(tokens) = AbnfLex(
      """Redirection  =  "300"  ;  Multiple Choices
        |            /   "301"  ;  Moved Permanently
        |            /   "302"  ;  Moved Temporarily
        |            /   "305"  ;  Use Proxy
        |            /   "380"  ;  Alternative Service
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("Redirection"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None, CharVal("300"))),
                                 List(ElementRep(None, CharVal("301"))),
                                 List(ElementRep(None, CharVal("302"))),
                                 List(ElementRep(None, CharVal("305"))),
                                 List(ElementRep(None, CharVal("380")))))))
    AbnfParser(tokens) should be(Right(parsed))
  }

  test("binary value rule") {
    val Right(tokens) = AbnfLex(
      """t-1 = %b010001001
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(137,None)))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("binary value rule concatenation") {
    val Right(tokens) = AbnfLex(
      """t-1 = %b010001001.0101.1111
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(137, Some(Right(List(5,15))))))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("binary value rule range") {
    val Right(tokens) = AbnfLex(
      """t-1 = %b010001001-11111
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(137,Some(Left(31)))))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("decimal value rule") {
    val Right(tokens) = AbnfLex(
      """t-1 = %d37484844
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(37484844,None)))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("decimal value rule concatenation") {
    val Right(tokens) = AbnfLex(
      """t-1 = %d37484844.10.30
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(37484844,Some(Right(List(10,30))))))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("decimal value rule range") {
    val Right(tokens) = AbnfLex(
      """t-1 = %d37484844-20
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(37484844,Some(Left(20)))))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("hex value rule") {
    val Right(tokens) = AbnfLex(
      """t-1 = %xAFDD23
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(11525411,None)))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("hex value rule concatenation") {
    val Right(tokens) = AbnfLex(
      """t-1 = %xAFDD23.F30.D2
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(11525411, Some(Right(List(3888,210))))))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("hex value rule range") {
    val Right(tokens) = AbnfLex(
      """t-1 = %x30-39
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,Value(48,Some(Left(57)))))))))
    AbnfParser(tokens) should be (Right(parsed))
  }

  test("prose") {
    val Right(tokens) = AbnfLex(
      """t-1 = <this is a test>
        |""".stripMargin)
    val parsed = Rules(List(Rule(Name("t-1"),
                            AssignmentDef.Assignment,
                            List(List(ElementRep(None,ProseValue("this is a test")))))))
    AbnfParser(tokens) should be (Right(parsed))
  }
}
