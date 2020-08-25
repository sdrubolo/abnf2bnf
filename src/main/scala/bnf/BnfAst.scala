package bnf

sealed trait BnfAbs
sealed trait BnfElementAbs

object Bnf {

  type Cons = List[BnfElementAbs]
  type Alternatives = List[Cons]
  type BnfRule = Map[String,Alternatives]

}


case class BnfRules(rules:Bnf.BnfRule) extends BnfAbs {

  private def printCons(alt:Bnf.Cons):String = alt match {
    case Nil => ""
    case x::xs => s"${x.toString()} ${printCons(xs)}"
  }

  private def printAlts(alt:Bnf.Alternatives):String = alt match {
    case Nil => ""
    case List(cons) => printCons(cons)
    case x::xs => s"${printCons(x)}| ${printAlts(xs)}"
  }

  override def toString: String = {
    rules.foldLeft("") {
      case (str,(key,value)) => s"$str\n$key ::= ${printAlts(value)}"
    }
  }
}

object Empty extends BnfElementAbs {
  override def toString: String = "ε"
}

case class BnfName(name:String) extends BnfElementAbs {
  override def toString: String = name
}

case class Terminal(name:String) extends BnfElementAbs {

  private def escapeChar(value:Int):String = {
    val escapeMapping = Map(
      "7" -> """\a""",
      "8" -> """\b""",
      "c" -> """\f""",
      "a" -> """\n""",
      "d" -> """\r""",
      "9" -> """\t""",
      "b" -> """\v""",
      "5c" -> """\\""",
      "27" -> """\'""",
      "22" -> """\"""",
    )
    escapeMapping.getOrElse(value.toHexString,s"""${Character.toString(value)}""")
  }

  private def escape(str:String):String = str.foldLeft("") {
    case (acc,char) => acc ++ escapeChar(char)
  }


  override def toString: String = s""""${escape(name)}""""
}

