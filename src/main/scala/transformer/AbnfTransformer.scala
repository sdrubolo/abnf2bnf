package transformer

import abnf._
import bnf.{Alternatives, BnfAbs, BnfName, BnfRules, BnfRule}
import cats.implicits._

import scala.annotation.tailrec

object AbnfTransformer {

  private val coreRules = CoreRules

  private def buildStart(unusedRules:Set[String]) : Map[String,Alternatives] = {
    unusedRules.foldLeft(Map[String,Alternatives]()) {
      case (acc,rule) =>
        val startRuleName = "_start"
        val rules = acc.getOrElse(startRuleName,List())
        acc + (startRuleName -> (List(BnfName(rule))::rules))
    }
  }

  private def internalRules(name:String,rules:BnfRule):Set[String] = rules get name match {
    case None => Set[String]()
    case Some(alts) =>
      alts.foldLeft(Set[String]()) {
      case (acc,cons) => cons.foldLeft(acc) {
        case (acc,BnfName(a)) if isInternalRule(a) => {
          val rightInternalRules = if(a != name) acc | internalRules(a,rules) else Set[String]()
          (acc + a) | rightInternalRules
        }
        case (acc,_) => acc
      }
    }
  }

  private def unusedInternalRules(unusedCoreRules:Set[String],rules:BnfRule):Set[String]
    = unusedCoreRules.foldLeft(Set[String]()) {
      case (acc,coreRule) => acc | internalRules(coreRule,rules)
    }

  private def removeUnusedRules(unusedRules:Set[String],rules:BnfRule):BnfRule
    = unusedRules.foldLeft(rules) {
      case (acc,rule) => acc - rule
    }

  @tailrec
  private def findUsedRules(rules:BnfAbs):BnfAbs = rules match {
    case  BnfRules(ruleSet) =>
      val rightHandRules = ruleSet.foldLeft(Set[String]()) {
        case (acc,(_,foundRules)) =>foundRules.foldLeft(acc) {
          case (acc,cons) => cons.foldLeft(acc) {
            case (acc,BnfName(n)) => acc + n
            case (acc,_) => acc
          }
        }
      }
      val unusedRules = ruleSet.keySet &~ rightHandRules
      val unusedCoreRules = unusedRules & coreRules.keys
      if(unusedCoreRules.nonEmpty) {
        val augmentedUnusedRules = unusedInternalRules(unusedCoreRules,ruleSet)
        val newRuleSet = removeUnusedRules(unusedCoreRules|augmentedUnusedRules,ruleSet)
        findUsedRules(BnfRules(newRuleSet))
      }
      else {
        (rightHandRules &~ ruleSet.keySet &~ coreRules.keys).foreach {
          rule => System.err.println(s"[warning] : Rule $rule is used but never defined")
        }
        val rootRules = ruleSet.keySet &~ rightHandRules
        BnfRules(ruleSet.combine(buildStart(rootRules)))
      }
  }

  def apply(abnf: AbnfAbs): Either[String, BnfAbs] = abnf match {
    case Rules(rules) =>
      Abnf2Bnf(Rules(coreRules.abnfRules ++ rules)) map { bnf => BnfSimplify(bnf)} map findUsedRules
  }

}

















