package transformer

import bnf._

import scala.annotation.tailrec

private[transformer] object BnfSimplify {

  def apply(bnf: BnfAbs): BnfAbs = simplify(bnf)

  private def rename(name: BnfName, rename: BnfName, alts: Alternatives): Alternatives = alts map {
        cons => cons map {
          case n if name == n => rename
          case any => any
        }
  }

  private def id[A] : A => A = value => value

  private def isRecursive(name:BnfName,alts:Alternatives):Boolean = alts.exists {
    cons => cons.exists {
      case n if name == n => true
      case _ => false
    }
  }

  private def isSingleRule(cons: Cons):Boolean = cons match {
    case _::Nil => true
    case _ => false
  }

  private def isSingleRuleInAlts(name:BnfName,alternatives: Alternatives):Boolean
  = alternatives.filter {
    cons => cons.exists {
      case n if name == n => true
      case _ => false
    }
  }.forall(isSingleRule)

  private def findReplacement(ruleName:BnfName, bnfElement:BnfElementAbs, alternatives: Alternatives, rules: BnfRule):Option[(BnfName,Alternatives)]
    = bnfElement match {
      case n @ BnfName(_) if isInternalRule(n) =>
        rules get n match {
          case Some(alts) if isSingleRuleInAlts(n,alternatives) && isRecursive(n,alts) =>
            Some((n,rename(n, ruleName, alts)))
          case Some(alts) if !isRecursive(n,alts) => Some((n,alts))
          case _ => None
        }
      case _ => None
    }

  private def join:BnfElementAbs=>Cons=>Cons = element => {
    case Nil => List(element)
    case Empty::xs => element::xs
    case any => element match {
      case Empty => any
      case _ => element::any
    }
  }

  private def merge(ruleName:BnfName, cons:Cons, alts: Alternatives, rules: BnfRule): (List[Cons=>Cons],Set[BnfName])
    = cons.foldRight(List(id[Cons]),Set[BnfName]()) {
    case (element, (rightVisited, replacedRules)) =>
      val alternatives = findReplacement(ruleName, element, alts, rules) map {
        case (name, alts) =>
          val alternatives = alts map {
            cons => cons.foldRight(id[Cons]) {
              case (elem, acc) => acc.andThen(join(elem))
            }
          }
          (alternatives, Set(name))
      }
      val (leftVisited, replacedRule) = alternatives.getOrElse((List[Cons => Cons](join(element)), Set[BnfName]()))
      val newMap = for {left <- leftVisited
                        right <- rightVisited
                        } yield right.andThen(left)

      (newMap, replacedRule | replacedRules)
  }

  private def mergeCons(ruleName:BnfName,cons: Cons, alternatives: Alternatives, rules: BnfRule): (Alternatives=>Alternatives,Set[BnfName])
  = {
    val (options,replacedRules) = merge(ruleName, cons, alternatives, rules)
    val alts = options.foldRight(id[Alternatives]) { case (opt,acc) => acc.andThen(opt(List())::_)  }
    (alts,replacedRules)
  }

  private def mergeAlternatives(ruleName:BnfName, alternatives:Alternatives, rules: BnfRule): (Alternatives,Set[BnfName])
    = {
    val (computedAlts, involvedRules) = alternatives.foldRight((id[Alternatives], Set[BnfName]())) {
      case (cons, (alts, replaced)) =>
        val (newCons, replacedRuleName) = mergeCons(ruleName, cons, alternatives, rules)
        (alts.andThen(newCons), replaced | replacedRuleName)
    }
    (computedAlts(List()), involvedRules)
  }

  private def mergeInvolvedRules(name:BnfName,x:Set[BnfName],y:Set[BnfName]):Set[BnfName]
  = if (isInternalRule(name) && x(name) && y.nonEmpty) x else x | y

  @tailrec
  private def simplify(bnf: BnfAbs): BnfAbs = bnf match {
    case BnfRules(rules) =>

      val (newRuleSet,replacedRules):(BnfRule,Set[BnfName]) = rules.foldLeft((rules,Set[BnfName]())) {
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
