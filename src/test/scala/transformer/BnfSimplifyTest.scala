package transformer

import bnf.{BnfName, BnfRules, Empty, Terminal}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BnfSimplifyTest extends AnyFunSuite with Matchers {

  test("simplify rule with no empty case") {
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"),BnfName("t4"))),
                                   "_0" -> List(List(BnfName("_1"),BnfName("t3"),BnfName("t4"))),
                                   "_1" -> List(List(Terminal("A")))))
    val expected = BnfRules(Map("t-1" -> List(List(Terminal("A"),BnfName("t3"),BnfName("t4"),BnfName("t4")))))
    BnfSimplify(transformed) should be(expected)
  }

  test("translate nested replacement") {
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                   "_0" -> List(List(Empty),List(BnfName("_1"))),
                                   "_1" -> List(List(BnfName("_2"),BnfName("t4"))),
                                   "_2" -> List(List(Empty),List(BnfName("t2"),BnfName("_2")))))
    val expected = BnfRules(Map("t-1" -> List(List(Empty),List(BnfName("_2"),BnfName("t4"))),
                                "_2" -> List(List(Empty),List(BnfName("t2"),BnfName("_2")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("translate nested replacement - 2") {
    val transformed = BnfRules(Map("t-1" -> List(List(Empty),List(BnfName("_0"))),
                                   "_0" -> List(List(BnfName("_1"),BnfName("t4"))),
                                   "_1" -> List(List(Empty),List(BnfName("t2"),BnfName("_1")))))
    val expected = BnfRules(Map("t-1" -> List(List(Empty),List(BnfName("_1"),BnfName("t4"))),
                                "_1" -> List(List(Empty),List(BnfName("t2"),BnfName("_1")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("translate range num value") {
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                    "_0" -> List(List(Empty),List(BnfName("_1"),BnfName("_0"))),
                                    "_1" -> List(List(Empty),List(BnfName("t2"),BnfName("t4")))))
    val expected = BnfRules(Map("t-1" -> List(List(Empty),List(BnfName("_1"),BnfName("t-1"))),
                                 "_1" -> List(List(Empty),List(BnfName("t2"),BnfName("t4")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("simplify rules when composition is simplifiable") {
    val transformed = BnfRules(Map("_subtag" -> List(List(BnfName("_0"))),
                                    "_0" -> List(List(Terminal("A"),Terminal("B"),Terminal("C"))),
                                    "_1" -> List(List(Terminal("-"),BnfName("_subtag"))),
                                    "_2" -> List(List(Empty),List(BnfName("_1"),BnfName("_3")))))
    val expected = BnfRules(Map("_2" -> List(List(Empty),List(Terminal("-"),Terminal("A"),Terminal("B"),Terminal("C"),BnfName("_3")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("simplify rules when composition is not simplifiable") {
    val transformed = BnfRules(Map("subtag" -> List(List(BnfName("_0"))),
                                   "_0" -> List(List(Terminal("A")),List(Terminal("B")),List(Terminal("C"))),
                                   "_1" -> List(List(Terminal("-"),BnfName("subtag"))),
                                   "_2" -> List(List(Empty),List(BnfName("_1"),BnfName("_3")))))
    val expected = BnfRules(Map("subtag" -> List(List(Terminal("A")),List(Terminal("B")),List(Terminal("C"))),
                                "_2" -> List(List(Empty),List(Terminal("-"),BnfName("subtag"),BnfName("_3")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("should not simplify rules when rule name appears in different alts") {
    val transformed = BnfRules(Map("_19" -> List(List(BnfName("_20")),List(BnfName("_20"),BnfName("_19"))),
                                    "_20" -> List(List(Terminal("alphanum")),List(Terminal("B")),List(Terminal("C"))),
                                    "alphanum" -> List(List(BnfName("ALPHA"),BnfName("DIGIT")))))
    val expected = BnfRules(Map("_19" -> List(List(BnfName("_20")),List(BnfName("_20"),BnfName("_19"))),
                                "_20" -> List(List(Terminal("alphanum")),List(Terminal("B")),List(Terminal("C"))),
                                "alphanum" -> List(List(BnfName("ALPHA"),BnfName("DIGIT")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("should simplify alt rule by replacing rule name with multiple alts") {
    val transformed = BnfRules(Map("_0" -> List(List(BnfName("_1")),List(BnfName("_2")),List(BnfName("_3"))),
                                   "_2" -> List(List(Terminal("alphanum")),List(Terminal("B")),List(Terminal("C")))))
    val expected = BnfRules(Map("_0" -> List(List(BnfName("_1")),List(Terminal("alphanum")),List(Terminal("B")),List(Terminal("C")),List(BnfName("_3")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }


}
