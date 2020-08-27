package transformer

import cats.implicits._
import abnf._
import bnf.Bnf.{Alternatives, BnfRule, Cons}
import bnf._

object Abnf2Bnf {

  type TranslateError = Either[String, (Int, BnfRule)]

  def apply(abnf: AbnfAbs): Either[String, BnfAbs] = rules(abnf)

  def rules(rules: AbnfAbs): Either[String, BnfAbs] = rules match {
    case Rules(rules) =>
      val empty: Either[String, (Int, BnfRule)] = Right(0, Map())
      val ruleSet = rules.foldLeft(empty) {
        case (acc, element) => acc flatMap {
          case (count, rules) => rule(count, element) map { case (c, e) => (c, rules.combine(e)) }
        }
      }
      ruleSet map { case (_, rules) => BnfRules(rules) }
  }

  def rule(counter: Int, rule: RuleAbs): TranslateError =
    rule match {
      case Rule(Name(name), _, elements) => alternatives(counter, name, elements)
    }

  def alternatives(counter: Int, name: String, elements: List[List[ElementRep]]): TranslateError = {
      val empty: Either[String, (Int, BnfRule)] = Right(counter, Map())
      elements.foldLeft(empty) {
          case (acc, alternative) => acc flatMap {
          case (c, rule) => concatenation(c, name, alternative) map { case (c, e) => (c, rule.combine(e)) }
        }
      }
  }

  private def singleRule(name: String, rule: BnfRule): Cons = rule.getOrElse(name, List(List())).head

  def concatenation(counter: Int, name: String, elements: List[ElementRep]): TranslateError = {
    val empty: Either[String, (Int, BnfRule)] = Right(counter, Map())
      elements.foldLeft(empty) {
        case (acc, element) => acc flatMap {
          case (count, rule) => repetition(count, name, element) map {
            case (c, e) => val repetition = singleRule(name, e)
              val cons = singleRule(name, rule)
              (c, rule.combine(e - name) + (name -> List(cons.combine(repetition))))
          }
        }
      }
  }

  def repetition(count: Int, name: String, elm: ElementRep): TranslateError = elm match {
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

  private def intToRule(ruleName: String, rule: Cons)(x: Int): Cons = x match {
    case 0 => List(Empty)
    case -1 => rule ++ List(BnfName(ruleName))
    case x => List.fill(x)(rule).flatten
  }

  private def newEntryRule(from: String, to: String): BnfRule = Map(from -> List(List(BnfName(to))))

  private def repeat(count: Int, name: String, rep: RepeatAbs, rule: ElementAbs): TranslateError = {
    val newRuleName = s"_$count"
    val nextRule = s"_${count + 1}"
    element(count + 1, newRuleName, rule) flatMap {

      case (newCount, bnfRule) =>
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
            Right((newCount, r.combine(newEntry.combine(replicatedRule))))
        }
    }
  }

  def element(count: Int, name: String, element: ElementAbs): TranslateError = element match {
    case e: Value => numValue(count, name, e)
    case e: CharVal => charValue(count, name, e)
    case e: Name => ruleName(count, name, e)
    case e: Group => group(count, name, e)
    case e: Opt => options(count, name, e)
    case _: ProseValue => Left("Prose value not supported yet")
  }

  def group(count: Int, name: String, group: Group): TranslateError = group match {
    case Group(elements) => val newRuleName = s"_$count"
      alternatives(count + 1, newRuleName, elements) map {
        case (newCount, groupRule) =>
          val newEntry = newEntryRule(name, newRuleName)
          (newCount, groupRule.combine(newEntry))
      }
  }

  private def noEmptyRule(ruleName: String, optional: BnfRule): Option[Alternatives] = optional get ruleName match {
    case Some(List(Empty) :: xs) => Some(xs)
    case _ => None
  }

  def options(count: Int, name: String, opts: Opt): TranslateError = opts match {
    case Opt(elements) => val newRuleName = s"_$count"
      val optCount = count + 1
      alternatives(optCount, newRuleName, elements) map {
        case (newCount, optionalRule) =>
          val emptyRuleSet : BnfRule = Map()
          val emptyRule : BnfRule = (noEmptyRule(s"_$optCount", optionalRule) map {
              _ => emptyRuleSet
            }).getOrElse(Map(newRuleName -> List(List(Empty))))
          (newCount, emptyRule.combine(optionalRule).combine(newEntryRule(name, newRuleName)))
      }
  }

  def ruleName(count: Int, name: String, rule: Name): TranslateError = rule match {
    case Name(ruleName) => Right((count, Map(name -> List(List(BnfName(ruleName))))))
  }

  def charValue(count: Int, name: String, value: CharVal): TranslateError = value match {
    case CharVal(value) => Right((count, Map(name -> List(List(Terminal(value))))))
  }

  def numValue(count: Int, name: String, value: Value): TranslateError = value match {
    case Value(value, repeat) =>
      val initial = Terminal(charToString(value))
      repeat match {
        case None => simpleNumValue(count,name, initial, List())
        case Some(Left(range)) => rangeNumValue(count, name, initial, value, range)
        case Some(Right(seq)) => simpleNumValue(count,name, initial, seq)
      }
  }

  private def charToString(value:Int):String = Character.toString(value)

  private def simpleNumValue(count:Int, name:String, initial:BnfElementAbs, value: Seq[Int]): TranslateError
  = { val cons = initial :: (value.toList map (v => Terminal(charToString(v))))
      Right((count,Map(name -> List(cons))))
  }

  private def rangeNumValue(count:Int, name:String, initial:BnfElementAbs, start: Int, end: Int): TranslateError
  = if (start > end) {
    Left("Cannot construct empty range")
  } else {
    val terminals = for (i <- start + 1 to end) yield List(Terminal(charToString(i)))
    val newRuleName = s"_$count"
    val alts = Map(newRuleName -> (List(initial) :: terminals.toList))
    Right((count+1,alts.combine(Map(name -> List(List(BnfName(newRuleName)))))))
  }

}
