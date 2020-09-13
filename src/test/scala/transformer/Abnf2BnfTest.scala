package transformer

import abnf._
import bnf._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Abnf2BnfTest extends AnyFunSuite with Matchers {

  test("translate range num value") {
    /*
    * t-1 = %x30-39
    *
    * t-1 := _0
    * _0  := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Incremental,
                          List(List(ElementRep(None,Value(48,Some(Left(57)))))))))
    val expected = Right(BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                      BnfName("_0")  -> List(List(Terminal("0")),
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

  test("translate string with case insensitive options") {
    /*
    * t-1 = "te-"
    *
    * t-1 := _t _e "-"
    * _t  := "t" | "T"
    * _e  := "e" | "E"
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Incremental,
                          List(List(ElementRep(None, CharVal("te-")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_t"),BnfName("_e"), Terminal("-"))),
                                                BnfName("_t") -> List(List(Terminal("t")),
                                                                      List(Terminal("T"))),
                                                BnfName("_e") -> List(List(Terminal("e")),
                                                                      List(Terminal("E")))))))
  }

  test("translate string with case insensitive options in multiple rules") {
    /*
    * t-1 = "te-"
    * t-2 = "te-"
    *
    * t-1 := _t _e "-"
    * t-2 := _t _e "-"
    * _t  := "t" | "T"
    * _e  := "e" | "E"
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                               AssignmentDef.Incremental,
                               List(List(ElementRep(None, CharVal("te-"))))),
                          Rule(Name("t-2"),
                               AssignmentDef.Incremental,
                               List(List(ElementRep(None, CharVal("te-"))))))
    )
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_t"),BnfName("_e"), Terminal("-"))),
                                                BnfName("t-2") -> List(List(BnfName("_t"),BnfName("_e"), Terminal("-"))),
                                                BnfName("_t") -> List(List(Terminal("t")),
                                                                      List(Terminal("T"))),
                                                BnfName("_e") -> List(List(Terminal("e")),
                                                                      List(Terminal("E")))))))
  }

  test("translate simple rule") {
    /*
    * t-1 = test
    *
    * t-1 := test
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Incremental,
                          List(List(ElementRep(None, Name("test")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map(BnfName("t-1") -> List(List(BnfName("test")))))))
  }

  test("translate concatenation rule") {
    /*
    * t-1 = test test
    *
    * t-1 := test test
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Incremental,
                          List(List(ElementRep(None, Name("test")),ElementRep(None, Name("test")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map(BnfName("t-1") -> List(List(BnfName("test"),BnfName("test")))))))
  }

  test("translate alternatives rule") {
    /*
    * t-1 = test / test
    *
    * t-1 := test | test
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Incremental,
                          List(List(ElementRep(None, Name("test"))),List(ElementRep(None, Name("test")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map(BnfName("t-1") -> List(List(BnfName("test")),List(BnfName("test")))))))
  }

  test("translate digit repetition rule") {
    /*
    * t-1 = 2test
    *
    * t-1 := _0
    * _0  := test test
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(Some(DigitRepeat(2)), Name("test")))))))
    Abnf2Bnf(abnf) should be(Right(BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                                BnfName("_0")  -> List(List(BnfName("test"),BnfName("test")))))))
  }

  test("translate range with limits repetition rule") {
    /*
    * t-1 = 2*3test
    *
    * t-1 := _0
    * _0  := test test | test test test
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(Some(RangeRepeat(Some(2),Some(3))), Name("test")))))))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                   BnfName("_0")  -> List(List(BnfName("test"),BnfName("test")),
                                                          List(BnfName("test"),BnfName("test"),BnfName("test")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate range with only lower bound limit repetition rule") {
    /*
    * t-1 = 2*test
    *
    * t-1 := _0
    * _0  := test test | test _0
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(Some(RangeRepeat(Some(2),None)), Name("test")))))))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                   BnfName("_0")  -> List(List(BnfName("test"),BnfName("test")),
                                                          List(BnfName("test"),BnfName("_0")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate range with no limit repetition rule") {
    /*
    * t-1 = *test
    *
    * t-1 := _0
    * _0  := ε | test _0
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(Some(RangeRepeat(None,None)), Name("test")))))))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                   BnfName("_0")  -> List(List(Empty),
                                                          List(BnfName("test"),BnfName("_0")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate range only upper bound limit repetition rule") {
    /*
    * t-1 = *3test
    *
    * t-1 := _0
    * _0  := ε | test | test test | test test test
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(Some(RangeRepeat(None,Some(3))), Name("test")))))))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                   BnfName("_0")  -> List(List(Empty),
                                                          List(BnfName("test")),
                                                          List(BnfName("test"),BnfName("test")),
                                                          List(BnfName("test"),BnfName("test"),BnfName("test")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate simple group rule") {
    /*
    * t-1 = (t2 t3) t4
    *
    * t-1 := _0 t4
    * _0  := t2 t3
    * */
    val firstRule = List(List(ElementRep(None,Name("t2")), ElementRep(None,Name("t3"))))
    val group = List(List(ElementRep(None,Group(firstRule)),ElementRep(None,Name("t4"))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, group)))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"),BnfName("t4"))),
                                   BnfName("_0")  -> List(List(BnfName("t2"),BnfName("t3")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate group rule with repeat rule") {
    /*
    * t-1 = (*t2 t3 / t4)
    *
    * t-1 := _0
    * _0  := _1 t3 | t4
    * _1  :=  ε | t2 _1
    * */
    val firstRule = List(ElementRep(Some(RangeRepeat(None,None)),Name("t2")),
                         ElementRep(None,Name("t3")))
    val secondRule = List(ElementRep(None,Name("t4")))
    val group = List(List(ElementRep(None,Group(List(firstRule,secondRule)))))
    val abnf = Rules(List(Rule(Name("t-1"),
                     AssignmentDef.Assignment, group)))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                   BnfName("_0")  -> List(List(BnfName("_1"),BnfName("t3")),List(BnfName("t4"))),
                                   BnfName("_1")  -> List(List(Empty),
                                                          List(BnfName("t2"),BnfName("_1")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate simple optional rule") {
    /*
    * t-1 = [t2 t3] t4
    *
    * t-1 := _0 t4
    * _0  := ε | t2 t3
    * */
    val firstRule = List(List(ElementRep(None,Name("t2")), ElementRep(None,Name("t3"))))
    val optional = List(List(ElementRep(None,Opt(firstRule)),ElementRep(None,Name("t4"))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, optional)))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"),BnfName("t4"))),
                                   BnfName("_0")  -> List(List(Empty),List(BnfName("t2"),BnfName("t3")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate optional rule containing no lower bound range") {
    /*
    * t-1 = [*t2] t4
    *
    * t-1 := _0 t4
    * _0  := _1
    * _1  := ε | t2 _1
    * */
    val firstRule = List(List(ElementRep(Some(RangeRepeat(None,None)),Name("t2"))))
    val opt = List(List(ElementRep(None,Opt(firstRule)),ElementRep(None,Name("t4"))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, opt)))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"),BnfName("t4"))),
                                   BnfName("_0")  -> List(List(BnfName("_1"))),
                                   BnfName("_1")  -> List(List(Empty),List(BnfName("t2"),BnfName("_1")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate nested optional rule") {
    /*
    * t-1 = [[*t2 t4]]
    *
    * t-1 := _0
    * _0  := ε | _1
    * _1  := _2 t4
    * _2  := ε | t2 _2
    * */
    val firstRule = List(List(ElementRep(Some(RangeRepeat(None,None)),Name("t2")),ElementRep(None,Name("t4"))))
    val opt2 = List(List(ElementRep(None,Opt(firstRule))))
    val opt1 = List(List(ElementRep(None,Opt(opt2))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, opt1)))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                   BnfName("_0") -> List(List(Empty),List(BnfName("_1"))),
                                   BnfName("_1") -> List(List(BnfName("_2"),BnfName("t4"))),
                                   BnfName("_2") -> List(List(Empty),List(BnfName("t2"),BnfName("_2")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate repeat nested optional rule") {
    /*
    * t-1 = *[t2 t4]
    *
    * t-1 := _0
    * _0  := ε | _1 _0
    * _1  := t2 t4
    * */
    val firstRule = List(List(ElementRep(None,Name("t2")),ElementRep(None,Name("t4"))))
    val opt1 = List(List(ElementRep(Some(RangeRepeat(None,None)),Opt(firstRule))))
    val abnf = Rules(List(Rule(Name("t-1"), AssignmentDef.Assignment, opt1)))
    val transformed = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_0"))),
                                   BnfName("_0")  -> List(List(Empty),List(BnfName("_1"),BnfName("_0"))),
                                   BnfName("_1")  -> List(List(BnfName("t2"),BnfName("t4")))))
    Abnf2Bnf(abnf) should be(Right(transformed))
  }

  test("translate prose not supported") {
    /*
    * t-1 = <t2>
    *
    * Error : Prose value not supported yet
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(None,ProseValue("t2")))))))
    Abnf2Bnf(abnf) should be(Left("Prose value not supported yet"))
  }

  test("translate empty range throws error") {
    /*
    * t-1 = 3*2t2
    *
    * Error : Cannot construct empty range
    * */
    val abnf = Rules(List(Rule(Name("t-1"),
                          AssignmentDef.Assignment,
                          List(List(ElementRep(Some(RangeRepeat(Some(3),Some(2))),Name("t2")))))))
    transformer.Abnf2Bnf(abnf) should be(Left("Cannot construct empty range"))
  }
}
