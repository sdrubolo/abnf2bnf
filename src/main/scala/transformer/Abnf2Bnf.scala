package transformer

import cats.implicits._
import abnf._
import bnf._

private[transformer] object Abnf2Bnf {

  type TranslateError = Either[String, (Int, BnfRule, BnfRule)]

  def apply(abnf: AbnfAbs): Either[String, BnfAbs] = rules(abnf)

  private def addRules(x:BnfRule,y:BnfRule):BnfRule = x -- y.keySet ++ y

  private def rules(rules: AbnfAbs): Either[String, BnfAbs] = rules match {
    case Rules(rules) =>
      val empty: TranslateError = Right(0, Map(), Map())
      val ruleSet = rules.foldLeft(empty) {
        case (acc, element) => acc flatMap {
          case (count, rules, caseInsensitive) =>
            rule(count, element) map { case (c, e, cir) => (c, rules.combine(e), addRules(caseInsensitive,cir)) }
        }
      }
      ruleSet map { case (_, rules, caseInsensitive) => BnfRules(rules.combine(caseInsensitive)) }
  }

  private def rule(counter: Int, rule: RuleAbs): TranslateError =
    rule match {
      case Rule(Name(name), _, elements) => alternatives(counter, BnfName(name), elements)
    }

  private def alternatives(counter: Int, name: BnfName, elements: List[List[ElementRep]]): TranslateError = {
      val empty: TranslateError = Right(counter, Map(), Map())
      elements.foldLeft(empty) {
          case (acc, alternative) => acc flatMap {
          case (c, rule, caseInsensitive) =>
            concatenation(c, name, alternative) map { case (c, e, cir) => (c, rule.combine(e), addRules(caseInsensitive,cir)) }
        }
      }
  }

  private def singleRule(name: BnfName, rule: BnfRule): Cons = rule.getOrElse(name, List(List())).head

  private def concatenation(counter: Int, name: BnfName, elements: List[ElementRep]): TranslateError = {
    val empty: TranslateError = Right(counter, Map(), Map())
      elements.foldLeft(empty) {
        case (acc, element) => acc flatMap {
          case (count, rule, caseInsensitive) => repetition(count, name, element) map {
            case (c, e, cir) => val repetition = singleRule(name, e)
              val cons = singleRule(name, rule)
              (c, rule.combine(e - name) + (name -> List(cons.combine(repetition))), addRules(caseInsensitive,cir))
          }
        }
      }
  }

  private def repetition(count: Int, name: BnfName, elm: ElementRep): TranslateError = elm match {
    case ElementRep(None, elementRule) => element(count, name, elementRule)
    case ElementRep(Some(rep), elementRule) => repeat(count, name, rep, elementRule)
  }

  private def buildRepetitionRange(lower: Int, upper: Int): List[Int] = upper match {
    case -1 => List(lower, -1)
    case _ => if (lower > upper) {
      List()
    } else {
      lower :: buildRepetitionRange(lower + 1, upper)
    }
  }

  private def intToRule(ruleName: BnfName, rule: Cons)(x: Int): Cons = x match {
    case 0 => List(Empty)
    case -1 => rule ++ List(ruleName)
    case x => List.fill(x)(rule).flatten
  }

  private def newEntryRule(from: BnfName, to: BnfName): BnfRule = Map(from -> List(List(to)))

  private def repeat(count: Int, name: BnfName, rep: RepeatAbs, rule: ElementAbs): TranslateError = {
    val newRuleName = BnfName(s"_$count")
    val nextRule = BnfName(s"_${count + 1}")
    element(count + 1, newRuleName, rule) flatMap {

      case (newCount, bnfRule, caseInsensitiveRules) =>
        val newEntry = newEntryRule(name, newRuleName)
        val (min, max) = rep match {
          case DigitRepeat(value) => (value, value)
          case RangeRepeat(min, max) => (min.getOrElse(0), max.getOrElse(-1))
        }
        val toReplicate = bnfRule(newRuleName).head
        val remainingRules = bnfRule - newRuleName
        val rules = buildRepetitionRange(min, max) map intToRule(newRuleName, toReplicate)

        rules match {
          case Nil => Left("Cannot construct empty range")
          case x =>
            val replicatedRule : BnfRule = Map(newRuleName -> x)
            val nestNoEmpty : Option[BnfRule] = noEmptyRule(newRuleName, replicatedRule) flatMap {
              _ => noEmptyRule(nextRule, remainingRules) map { alts => Map(nextRule->alts)}
            }
            val r = nestNoEmpty.getOrElse(remainingRules)
            Right((newCount, r.combine(newEntry.combine(replicatedRule)), caseInsensitiveRules))
        }
    }
  }

  private def element(count: Int, name: BnfName, element: ElementAbs): TranslateError = element match {
    case e: Value => numValue(count, name, e)
    case e: CharVal => charValue(count, name, e)
    case e: Name => ruleName(count, name, e)
    case e: Group => group(count, name, e)
    case e: Opt => options(count, name, e)
    case _: ProseValue => Left("Prose value not supported yet")
  }

  private def group(count: Int, name: BnfName, group: Group): TranslateError = group match {
    case Group(elements) =>
      val newRuleName = BnfName(s"_$count")
      alternatives(count + 1, newRuleName, elements) map {
        case (newCount, groupRule, caseInsensitiveRules) =>
          val newEntry = newEntryRule(name, newRuleName)
          (newCount, groupRule.combine(newEntry), caseInsensitiveRules)
      }
  }

  private def noEmptyRule(ruleName: BnfName, optional: BnfRule): Option[Alternatives] = optional get ruleName match {
    case Some(List(Empty) :: xs) => Some(xs)
    case _ => None
  }

  private def options(count: Int, name: BnfName, opts: Opt): TranslateError = opts match {
    case Opt(elements) => val newRuleName = BnfName(s"_$count")
      val optCount = count + 1
      alternatives(optCount, newRuleName, elements) map {
        case (newCount, optionalRule, caseInsensitiveRules) =>
          val emptyRuleSet : BnfRule = Map()
          val emptyRule : BnfRule = (noEmptyRule(BnfName(s"_$optCount"), optionalRule) map {
              _ => emptyRuleSet
            }).getOrElse(Map(newRuleName -> List(List(Empty))))
          (newCount, emptyRule.combine(optionalRule).combine(newEntryRule(name, newRuleName)), caseInsensitiveRules)
      }
  }

  private def ruleName(count: Int, name: BnfName, rule: Name): TranslateError = rule match {
    case Name(ruleName) => Right((count, Map(name -> List(List(BnfName(ruleName)))), Map()))
  }

  private def joinTerminals(cons:Cons):Cons
    = cons.foldRight(List[BnfElementAbs]()) {
      case (element,acc) => element match {
        case Terminal(value) => acc match {
          case Terminal(v)::xs =>Terminal(s"$value$v")::xs
          case any => element::any
        }
        case any => any::acc
      }
    }

  private def charValue(count: Int, name: BnfName, value: CharVal): TranslateError = value match {
    case CharVal(value) =>
      val empty : BnfRule = Map()
      val (alt,newRules,caseInsensitive) = value.foldRight((List[BnfElementAbs](),empty,empty)) {
        case (char,(acc,newRules,caseInsensitive)) => if (isCharCaseInsensitive(char)) {
          val name = BnfName(s"_${Character.toUpperCase(char)}_")
          val newCaseInsensitive = Map(name -> List(List(Terminal(s"${Character.toLowerCase(char)}")),List(Terminal(s"${Character.toUpperCase(char)}"))))
          (name::acc,newRules, addRules(caseInsensitive,newCaseInsensitive))
        }
        else {
          (Terminal(s"${Character.toString(char)}")::acc, newRules, caseInsensitive)
        }
      }
      Right((count, newRules + (name -> (List(alt) map joinTerminals)), caseInsensitive))
  }

  private def isCharCaseInsensitive(char: Char):Boolean = char.toInt match {
    case x => (x >= 65 && x <= 90) || (x >= 97 && x <= 122)
  }

  private def numValue(count: Int, name: BnfName, value: Value): TranslateError = value match {
    case Value(value, repeat) =>
      val initial = Terminal(charToString(value))
      repeat match {
        case None => simpleNumValue(count,name, initial, List())
        case Some(Left(range)) => rangeNumValue(count, name, initial, value, range)
        case Some(Right(seq)) => simpleNumValue(count,name, initial, seq)
      }
  }

  private def charToString(value:Int):String = Character.toString(value)

  private def simpleNumValue(count:Int, name:BnfName, initial:BnfElementAbs, value: Seq[Int]): TranslateError
    = {
      val cons = initial :: (value.toList map (v => Terminal(charToString(v))))
      Right((count,Map(name -> (List(cons) map joinTerminals)), Map()))
    }

  private def rangeNumValue(count:Int, name:BnfName, initial:BnfElementAbs, start: Int, end: Int): TranslateError
    = if (start > end) {
      Left("Cannot construct empty range")
    } else {
      val terminals = for (i <- start + 1 to end) yield List(Terminal(charToString(i)))
      val newRuleName = BnfName(s"_$count")
      val alts = (List(initial) :: terminals.toList) map joinTerminals
      Right((count+1,Map(newRuleName -> alts).combine(Map(name -> List(List(newRuleName)))), Map()))
    }

}
