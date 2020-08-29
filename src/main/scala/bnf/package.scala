package object bnf {

  type Cons = List[BnfElementAbs]
  type Alternatives = List[Cons]
  type BnfRule = Map[String,Alternatives]

}
