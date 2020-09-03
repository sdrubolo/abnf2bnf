package transformer

import bnf._

import scala.annotation.tailrec

private[transformer] object BnfSimplify {

  def apply(bnf: BnfAbs): BnfAbs = simplify(bnf)

  private def isInternalRule(name: String) = name.startsWith("_")

  private def rename(name: String, rename: String, alts: Alternatives): Alternatives = alts map {
        cons => cons map {
          case BnfName(n) if name == n => BnfName(rename)
          case any => any
        }
  }

  private def id[A] : A => A = value => value

  private def isRecursive(name:String,alts:Alternatives):Boolean = alts.exists {
    cons => cons.exists {
      case BnfName(n) if name == n => true
      case _ => false
    }
  }

  private def isSingleRule(cons: Cons):Boolean = cons match {
    case _::Nil => true
    case _ => false
  }

  private def findReplacement(ruleName:String, bnfElement:BnfElementAbs, cons: Cons, rules: BnfRule):Option[(String,Alternatives)] = bnfElement match {
    case BnfName(x) if isInternalRule(x) =>
      rules get x match {
        case Some(alts) if isSingleRule(cons) && isRecursive(x,alts) => Some((x,rename(x, ruleName, alts)))
        case Some(alts) if !isRecursive(x,alts) => Some((x,alts))
        case _ => None
      }
    case _ => None
  }

  private def merge(ruleName:String, cons:Cons,rules: BnfRule): (List[Cons=>Cons],Set[String])
    = cons.foldRight(List(id[Cons]),Set[String]()) {
    case (element, (rightVisited, replacedRules)) =>
      val alternatives = findReplacement(ruleName, element, cons, rules) map {
        case (name, alts) =>
          val alternatives = alts map {
            cons => cons.foldRight(id[Cons]) { case (elem, acc) => acc.andThen(elem :: _) }
          }
          (alternatives, Set(name))
      }
      val (leftVisited, replacedRule) = alternatives.getOrElse((List[Cons => Cons](element :: _), Set[String]()))
      val newMap = for {left <- leftVisited
                        right <- rightVisited
                        } yield right.andThen(left)

      (newMap, replacedRule | replacedRules)
  }

  private def mergeCons(ruleName:String,cons: Cons, rules: BnfRule): (Alternatives=>Alternatives,Set[String]) = {
    val (options,replacedRules) = merge(ruleName, cons, rules)
    val alts = options.foldRight(id[Alternatives]) { case (opt,acc) => acc.andThen(opt(List())::_)  }
    (alts,replacedRules)
  }

  private def mergeAlternatives(ruleName:String, alts:Alternatives, rules: BnfRule): (Alternatives,Set[String])
    = {
    val (computedAlts, involvedRules) = alts.foldRight((id[Alternatives], Set[String]())) {
      case (cons, (alts, replaced)) =>
        val (newCons, replacedRuleName) = mergeCons(ruleName, cons, rules)
        (alts.andThen(newCons), replaced | replacedRuleName)
    }
    (computedAlts(List()), involvedRules)
  }

  private def mergeInvolvedRules(name:String,x:Set[String],y:Set[String]):Set[String]
  = if (isInternalRule(name) && x(name) && y.nonEmpty) x else x | y

  @tailrec
  private def simplify(bnf: BnfAbs): BnfAbs = bnf match {
    case BnfRules(rules) =>

      val (newRuleSet,replacedRules):(BnfRule,Set[String]) = rules.foldLeft((rules,Set[String]())) {
        case ((bnf,remove), (head,body)) =>
          val (newBody,replacedRules) = mergeAlternatives(head,body,bnf)
          (bnf + (head -> newBody),mergeInvolvedRules(head,remove,replacedRules))
      }
      val newRules = replacedRules.foldLeft(newRuleSet) {
        case (rules,key) => rules - key
      }
      if(replacedRules.isEmpty) BnfRules(newRuleSet) else simplify(BnfRules(newRules))
  }
}
