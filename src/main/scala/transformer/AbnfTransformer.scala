package transformer

import abnf._
import bnf.{BnfAbs, BnfName, BnfRules, BnfRule}
import cats.implicits._

import scala.annotation.tailrec

object AbnfTransformer {

  private val coreRules = CoreRules

  private def buildStart(unusedRules:Set[BnfName]) : BnfRule = {
    val empty : BnfRule = Map()
    unusedRules.foldLeft(empty) {
      case (acc,rule) =>
        val startRuleName = BnfName("_start")
        val rules = acc.getOrElse(startRuleName,List())
        acc + (startRuleName -> (List(rule)::rules))
    }
  }

  private def internalRules(name:BnfName,rules:BnfRule):Set[BnfName] = rules get name match {
    case None => Set[BnfName]()
    case Some(alts) =>
      alts.foldLeft(Set[BnfName]()) {
      case (acc,cons) => cons.foldLeft(acc) {
        case (acc,n @ BnfName(_)) if isInternalRule(n) =>
          val rightInternalRules = if(n != name) acc | internalRules(n,rules) else Set[BnfName]()
          (acc + n) | rightInternalRules
        case (acc,_) => acc
      }
    }
  }

  private def unusedInternalRules(unusedCoreRules:Set[BnfName],rules:BnfRule):Set[BnfName]
    = unusedCoreRules.foldLeft(Set[BnfName]()) {
      case (acc,coreRule) => acc | internalRules(coreRule,rules)
    }

  private def removeUnusedRules(unusedRules:Set[BnfName],rules:BnfRule):BnfRule
    = unusedRules.foldLeft(rules) {
      case (acc,rule) => acc - rule
    }

  @tailrec
  private def findUsedRules(rules:BnfAbs):BnfAbs = rules match {
    case  BnfRules(ruleSet) =>
      val rightHandRules = ruleSet.foldLeft(Set[BnfName]()) {
        case (acc,(key,foundRules)) =>foundRules.foldLeft(acc) {
          case (acc,cons) => cons.foldLeft(acc) {
            case (acc,name @ BnfName(_)) if key != name => acc + name
            case (acc,_) => acc
          }
        }
      }
      val unusedRules = ruleSet.keySet &~ rightHandRules
      val coreKeys = coreRules.keys map BnfName
      val unusedCoreRules = unusedRules & coreKeys
      if(unusedCoreRules.nonEmpty) {
        val augmentedUnusedRules = unusedInternalRules(unusedCoreRules,ruleSet)
        val newRuleSet = removeUnusedRules(unusedCoreRules|augmentedUnusedRules,ruleSet)
        findUsedRules(BnfRules(newRuleSet))
      }
      else {
        (rightHandRules &~ ruleSet.keySet &~ coreKeys).foreach {
          rule => System.err.println(s"[${Console.YELLOW + "warning" + Console.WHITE}] : Rule $rule is used but never defined")
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

















