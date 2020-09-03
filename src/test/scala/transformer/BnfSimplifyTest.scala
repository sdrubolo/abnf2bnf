package transformer

import bnf.{BnfName, BnfRules, Empty, Terminal}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BnfSimplifyTest extends AnyFunSuite with Matchers {

  test("simplify rule with no empty case") {
    /*
    * t-1 := _0 t4
    * _0  := _1 t3 t4
    * _1  := "A"
    *
    * t-1 := "A" t3 t4 t4
    * */
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"),BnfName("t4"))),
                                   "_0"  -> List(List(BnfName("_1"),BnfName("t3"),BnfName("t4"))),
                                   "_1"  -> List(List(Terminal("A")))))
    val expected = BnfRules(Map("t-1" -> List(List(Terminal("A"),BnfName("t3"),BnfName("t4"),BnfName("t4")))))
    BnfSimplify(transformed) should be(expected)
  }

  test("translate nested replacement") {
    /*
    * t-1 := _0
    * _0  := ε | _1
    * _1  := _2 t4
    * _2  :=  ε | t2 _2
    *
    * t-1 := _ε | _2 t4
    * _2  :=  ε | t2 _2
    * */
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                   "_0"  -> List(List(Empty),List(BnfName("_1"))),
                                   "_1"  -> List(List(BnfName("_2"),BnfName("t4"))),
                                   "_2"  -> List(List(Empty),List(BnfName("t2"),BnfName("_2")))))
    val expected = BnfRules(Map("t-1" -> List(List(Empty),List(BnfName("_2"),BnfName("t4"))),
                                "_2"  -> List(List(Empty),List(BnfName("t2"),BnfName("_2")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("translate nested replacement - 2") {
    /*
    * t-1 := ε | _0
    * _0  := _1 t4
    * _1  := ε | t2 _1
    *
    * t-1 := ε | _1 t4
    * _1  := ε | t2 _1
    * */
    val transformed = BnfRules(Map("t-1" -> List(List(Empty),List(BnfName("_0"))),
                                   "_0"  -> List(List(BnfName("_1"),BnfName("t4"))),
                                   "_1"  -> List(List(Empty),List(BnfName("t2"),BnfName("_1")))))
    val expected = BnfRules(Map("t-1" -> List(List(Empty),List(BnfName("_1"),BnfName("t4"))),
                                "_1"  -> List(List(Empty),List(BnfName("t2"),BnfName("_1")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("translate range num value") {
    /*
    * t-1 := _0
    * _0  := ε | _1 _0
    * _1  := t2 t4
    *
    * t-1 := ε | t2 t4 t-1
    * */
    val transformed = BnfRules(Map("t-1" -> List(List(BnfName("_0"))),
                                    "_0" -> List(List(Empty),List(BnfName("_1"),BnfName("_0"))),
                                    "_1" -> List(List(BnfName("t2"),BnfName("t4")))))
    val expected = BnfRules(Map("t-1" -> List(List(Empty),List(BnfName("t2"),BnfName("t4"),BnfName("t-1")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("simplify rules when composition is simplifiable") {
    /*
    * _4 := _0
    * _0  := "A" "B" "C"
    * _1  := "-" _4
    * _2  := ε | _1 _3
    *
    * _2  := ε | "-" "A" "B" "C" _3
    * */
    val transformed = BnfRules(Map("_4" -> List(List(BnfName("_0"))),
                                   "_0" -> List(List(Terminal("A"),Terminal("B"),Terminal("C"))),
                                   "_1" -> List(List(Terminal("-"),BnfName("_4"))),
                                   "_2" -> List(List(Empty),List(BnfName("_1"),BnfName("_3")))))
    val expected = BnfRules(Map("_2" -> List(List(Empty),List(Terminal("-"),Terminal("A"),Terminal("B"),Terminal("C"),BnfName("_3")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("simplify rules when composition is not simplifiable") {
    /*
     * subtag := _0
     * _0  := "A" | "B" | "C"
     * _1  := "-" subtag
     * _2  := ε | _1 _3
     *
     * subtag := "A" | "B" | "C"
     * _2  := ε | "-" subtag _3
     * */
    val transformed = BnfRules(Map("subtag" -> List(List(BnfName("_0"))),
                                   "_0"     -> List(List(Terminal("A")),List(Terminal("B")),List(Terminal("C"))),
                                   "_1"     -> List(List(Terminal("-"),BnfName("subtag"))),
                                   "_2"     -> List(List(Empty),List(BnfName("_1"),BnfName("_3")))))
    val expected = BnfRules(Map("subtag" -> List(List(Terminal("A")),List(Terminal("B")),List(Terminal("C"))),
                                "_2"     -> List(List(Empty),List(Terminal("-"),BnfName("subtag"),BnfName("_3")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("should simplify rules when rule name appears in different alts") {
    /*
     * _19 := _20 | _20 _19
     * _20  := "A" | "B" | "C"
     *
     * _19 := "A" | "B" | "C" | "A" _19 | "B" _19 | "C" _19
     * */
    val transformed = BnfRules(Map("_19" -> List(List(BnfName("_20")),List(BnfName("_20"),BnfName("_19"))),
                                   "_20" -> List(List(Terminal("A")),List(Terminal("B")),List(Terminal("C")))))
    val expected = BnfRules(Map("_19" -> List(List(Terminal("A")),
                                              List(Terminal("B")),
                                              List(Terminal("C")),
                                              List(Terminal("A"),BnfName("_19")),
                                              List(Terminal("B"),BnfName("_19")),
                                              List(Terminal("C"),BnfName("_19")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }

  test("should simplify alt rule by replacing rule name with multiple alts") {
    /*
     * _0 := _1 | _2 | _3
     * _2  := "A" | "B" | "C"
     *
     * _0 := _1 | "A" | "B" | "C" | _3
     * */
    val transformed = BnfRules(Map("_0" -> List(List(BnfName("_1")),List(BnfName("_2")),List(BnfName("_3"))),
                                   "_2" -> List(List(Terminal("A")),List(Terminal("B")),List(Terminal("C")))))
    val expected = BnfRules(Map("_0" -> List(List(BnfName("_1")),
                                             List(Terminal("A")),
                                             List(Terminal("B")),
                                             List(Terminal("C")),
                                             List(BnfName("_3")))))
    transformer.BnfSimplify(transformed) should be(expected)
  }


}
