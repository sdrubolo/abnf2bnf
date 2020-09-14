package bnf

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PrintTest extends AnyFunSuite with Matchers {

  test("multiple rules") {
    val bfn = BnfRules(Map(BnfName("t-1") -> List(List(BnfName("_t"),BnfName("_e"), Terminal("-"))),
                           BnfName("_t")  -> List(List(Terminal("t")),
                                                  List(Terminal("T"))),
                           BnfName("_e")  -> List(List(Terminal("e")),
                                                  List(Terminal("E")))))
    val expected = """t-1 ::= _t _e "-"
                      |_t ::= "t" | "T"
                      |_e ::= "e" | "E"""".stripMargin
    Print(bfn) should be(expected)
  }

  test("empty rules") {
    val bfn = BnfRules(Map(BnfName("t-1") -> List(List(Empty),List(BnfName("_t"))),
                           BnfName("_t")  -> List(List(Terminal("t")))))
    val expected = """t-1 ::= ε | _t
                     |_t ::= "t"""".stripMargin
    Print(bfn) should be(expected)
  }
}
