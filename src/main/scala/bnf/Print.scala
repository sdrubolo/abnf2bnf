package bnf

object Print {

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
      "22" -> """\""""
    )
    escapeMapping.getOrElse(value.toHexString,s"""${Character.toString(value)}""")
  }

  private def escape(str:String):String = str.foldLeft("") {
    case (acc,char) => acc ++ escapeChar(char)
  }

  private def prettyCons(alt:Cons):String = alt match {
    case Nil => ""
    case List(x) => s"${prettyPrint(x)}"
    case x::xs => s"${prettyPrint(x)} ${prettyCons(xs)}"
  }

  private def printAlts(alt:Alternatives):String = alt match {
    case Nil => ""
    case List(cons) => prettyCons(cons)
    case x::xs => s"${prettyCons(x)} | ${printAlts(xs)}"
  }

  private def prettyPrint(element:BnfElementAbs):String = element match {
    case Terminal(name) => s""""${escape(name)}""""
    case BnfName(name) => s"$name"
    case Empty => "ε"
  }

  private def prettyPrint(rules:BnfRules):String = rules match {
    case BnfRules(rules) =>  rules.foldLeft("") {
      case (str,(key,value)) =>
        val newLine = str match {
          case x if x.isEmpty => ""
          case _ => "\n"
        }
        s"$str$newLine${prettyPrint(key)} ::= ${printAlts(value)}"
    }
  }

  def apply(rules:BnfRules): String = prettyPrint(rules)


}
