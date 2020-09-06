package transformer

import abnf._
import bnf.{Alternatives, BnfAbs, BnfName, BnfRules}
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
        val newRuleSet = unusedCoreRules.foldLeft(ruleSet) {
          case (acc,rule) => acc - rule
        }
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
      Abnf2Bnf(Rules(coreRules.abnfRules++rules)) map { bnf => BnfSimplify(bnf)} map findUsedRules
  }

}

















