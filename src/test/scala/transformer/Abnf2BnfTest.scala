package transformer

import abnf._
import bnf._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Abnf2BnfTest extends AnyFunSuite with Matchers {

  test("translate range num value") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Incremental,
      List(List(ElementRep(None,Value(48,Some(Left(57)))))))))
    val expected = Right(BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                      "_0"  -> List(List(Terminal("0")),
                                                    List(Terminal("1")),
                                                    List(Terminal("2")),
                                                    List(Terminal("3")),
                                                    List(Terminal("4")),
                                                    List(Terminal("5")),
                                                    List(Terminal("6")),
                                                    List(Terminal("7")),
                                                    List(Terminal("8")),
                                                    List(Terminal("9"))))))
    Abnf2Bnf(abnf) should be(expected)
  }

  test("translate simple rule") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Incremental,
      List(List(ElementRep(None, Name("test")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map("t-1" -> List(List(BnfName("test")))))))
  }

  test("translate concatenation rule") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Incremental,
      List(List(ElementRep(None, Name("test")),ElementRep(None, Name("test")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map("t-1" -> List(List(BnfName("test"),BnfName("test")))))))
  }

  test("translate alternatives rule") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Incremental,
      List(List(ElementRep(None, Name("test"))),List(ElementRep(None, Name("test")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map("t-1" -> List(List(BnfName("test")),List(BnfName("test")))))))
  }

  test("translate digit repetition rule") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Assignment,
      List(List(ElementRep(Some(DigitRepeat(2)), Name("test")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                                "_0" -> List(List(BnfName("test"),BnfName("test")))))))
  }

  test("translate range with limits repetition rule") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Assignment,
      List(List(ElementRep(Some(RangeRepeat(Some(2),Some(3))), Name("test")))))))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                  "_0" -> List(List(BnfName("test"),BnfName("test")),
                                    List(BnfName("test"),BnfName("test"),BnfName("test")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate range with only lower bound limit repetition rule") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Assignment,
      List(List(ElementRep(Some(RangeRepeat(Some(2),None)), Name("test")))))))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                  "_0" -> List(List(BnfName("test"),BnfName("test")),
                                               List(BnfName("test"),BnfName("_0")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate range with no limit repetition rule") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Assignment,
      List(List(ElementRep(Some(RangeRepeat(None,None)), Name("test")))))))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                  "_0" -> List(List(Empty),
                                               List(BnfName("test"),BnfName("_0")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate range only upper bound limit repetition rule") {
    val abnf = Rules(List(Rule(Name("t-1"),
      AssignmentDef.Assignment,
      List(List(ElementRep(Some(RangeRepeat(None,Some(3))), Name("test")))))))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                   "_0" -> List(List(Empty),
                                                List(BnfName("test")),
                                                List(BnfName("test"),BnfName("test")),
                                                List(BnfName("test"),BnfName("test"),BnfName("test")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate simple group rule") {
    val firstRule = List(List(ElementRep(None,Name("t2")), ElementRep(None,Name("t3"))))
    val group = List(List(ElementRep(None,Group(firstRule)),ElementRep(None,Name("t4"))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, group)))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"),BnfName("t4"))),
                                   "_0" -> List(List(BnfName("t2"),BnfName("t3")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate group rule with repeat rule") {
    val firstRule = List(ElementRep(Some(RangeRepeat(None,None)),Name("t2")),
                         ElementRep(None,Name("t3")))
    val secondRule = List(ElementRep(None,Name("t4")))
    val group = List(List(ElementRep(None,Group(List(firstRule,secondRule)))))
    val abnf = Rules(List(Rule(Name("t-1"),
                     AssignmentDef.Assignment, group)))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                   "_0" -> List(List(BnfName("_1"),BnfName("t3")),List(BnfName("t4"))),
                                   "_1" -> List(List(Empty),
                                                List(BnfName("t2"),BnfName("_1")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate simple optional rule") {
    val firstRule = List(List(ElementRep(None,Name("t2")), ElementRep(None,Name("t3"))))
    val optional = List(List(ElementRep(None,Opt(firstRule)),ElementRep(None,Name("t4"))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, optional)))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"),BnfName("t4"))),
                                   "_0" -> List(List(Empty),List(BnfName("t2"),BnfName("t3")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate optional rule containing no lower bound range") {
    val firstRule = List(List(ElementRep(Some(RangeRepeat(None,None)),Name("t2"))))
    val opt = List(List(ElementRep(None,Opt(firstRule)),ElementRep(None,Name("t4"))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, opt)))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"),BnfName("t4"))),
                                   "_0" -> List(List(BnfName("_1"))),
                                   "_1" -> List(List(Empty),List(BnfName("t2"),BnfName("_1")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate nested optional rule") {
    val firstRule = List(List(ElementRep(Some(RangeRepeat(None,None)),Name("t2")),ElementRep(None,Name("t4"))))
    val opt2 = List(List(ElementRep(None,Opt(firstRule))))
    val opt1 = List(List(ElementRep(None,Opt(opt2))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, opt1)))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                    "_0" -> List(List(Empty),List(BnfName("_1"))),
                                    "_1" -> List(List(BnfName("_2"),BnfName("t4"))),
                                    "_2" -> List(List(Empty),List(BnfName("t2"),BnfName("_2")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate repeat nested optional rule") {
    val firstRule = List(List(ElementRep(None,Name("t2")),ElementRep(None,Name("t4"))))
    val opt1 = List(List(ElementRep(Some(RangeRepeat(None,None)),Opt(firstRule))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, opt1)))
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
      "_0" -> List(List(Empty),List(BnfName("_1"),BnfName("_0"))),
      "_1" -> List(List(Empty),List(BnfName("t2"),BnfName("t4")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate prose not supported") {
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(None,ProseValue("t2")))))))
    Abnf2Bnf(abnf) should be(Left("Prose value not supported yet"))
  }

  test("translate empty range throws error") {
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(Some(RangeRepeat(Some(3),Some(2))),Name("t2")))))))
    transformer.Abnf2Bnf(abnf) should be(Left("Cannot construct empty range"))
  }
}
