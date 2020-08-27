package transformer

import bnf.Bnf.{Alternatives, BnfRule, Cons}
import bnf.{BnfAbs, BnfElementAbs, BnfName, BnfRules}

import scala.annotation.tailrec

object BnfSimplify {

  def apply(bnf: BnfAbs): BnfAbs = simplify(bnf, usage(bnf))

  private def isSingle[A](alt: Seq[A]) = alt match {
    case _ :: Nil => true
    case _ => false
  }

  private def canReplace(replace: Alternatives, alternatives: Alternatives, cons: Cons): Boolean = {
    val isSingleRule = isSingle(replace)
    val isSingleWithMultiple = !isSingle(alternatives) && isSingleRule
    isSingleRule || isSingleWithMultiple || (isSingle(cons) && isSingle(alternatives))
  }

  private def isInternalRule(name: String) = name.startsWith("_")

  @tailrec
  private def terminalRule(name: String, rules: BnfRule): Alternatives = rules get name match {
    case None => List()
    case Some(List(List(BnfName(ruleName)))) => terminalRule(ruleName, rules)
    case Some(alts) => alts
  }

  private def usage(bnf: BnfAbs): Set[String] = bnf match {
    case BnfRules(rules) => rules.foldLeft(Set.empty[String]) {
      case (acc, (ruleName, alternatives)) =>
        alternatives.foldLeft(acc) {
          case (acc, cons) => cons.foldLeft(acc) {
            case (acc, BnfName(name)) if name != ruleName && isInternalRule(name) && canReplace(terminalRule(name, rules), alternatives, cons) =>
              acc + name
            case (acc, _) => acc
          }
        }
    }
  }

  private def rename(name: String, rename: String, cons: Cons): Cons = cons map {
    case BnfName(n) if name == n => BnfName(rename)
    case any => any
  }

  private def merge(key: String, keySubts: String, alts: Alternatives, altsReplace: Alternatives): Alternatives = alts match {
    case List(List(BnfName(name))) if name == keySubts => altsReplace map (rename(name, key, _))
    case _ => altsReplace match {
      case List(cons@_ :: _) => alts map {
        concatenation =>
          val replacedCons = concatenation.foldRight(List[BnfElementAbs]()) {
            case (BnfName(name), acc) if name == keySubts => rename(name, key, cons) ++ acc
            case (any, acc) => any :: acc
          }
          replacedCons
      }

      case _ => alts
    }
  }

  private def simplify(bnf: BnfAbs, bnfUsage: Set[String]): BnfAbs = bnf match {
    case BnfRules(rules) =>
      println(bnfUsage)
      val simplifiedRules = bnfUsage.foldLeft(rules) {
        case (ruleToSimplify, rulesToReplace) =>
          (ruleToSimplify - rulesToReplace) map {
            case (key, alternatives) =>
              val replaceAlts = ruleToSimplify get rulesToReplace
              val newRuleSet = replaceAlts map { alts => merge(key, rulesToReplace, alternatives, alts) }
              (key, newRuleSet.getOrElse(alternatives))
          }
      }
      BnfRules(simplifiedRules)
  }
}
