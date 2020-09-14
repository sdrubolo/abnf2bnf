package bnf


sealed trait BnfAbs
sealed trait BnfElementAbs


case class BnfRules(rules:BnfRule) extends BnfAbs
object Empty extends BnfElementAbs
case class BnfName(name:String) extends BnfElementAbs
case class Terminal(name:String) extends BnfElementAbs




