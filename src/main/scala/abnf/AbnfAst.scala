package abnf

  object AssignmentDef extends Enumeration {
    type DefinedAs = Value
    val Assignment, Incremental = Value
  }

  object Base extends Enumeration {
    protected case class Val(base: Int) extends super.Val

    import scala.language.implicitConversions
    implicit def valueToBase(x: Value): Val = x.asInstanceOf[Val]

    val Binary = Val(2)
    val Decimal = Val(10)
    val Hex = Val(16)
  }

  sealed trait ToBase[A,B] {
    def convert(value:A, base:B) : A
  }

  import abnf.AssignmentDef._

  sealed trait AbnfAbs
  sealed trait ElementAbs
  sealed trait RuleAbs
  sealed trait RepeatAbs
  sealed trait ValueAbs

  case class Rules(rules:Seq[RuleAbs]) extends AbnfAbs
  case class Rule(name:Name, assignmentDef: DefinedAs, elements:List[List[ElementRep]]) extends RuleAbs

  case class ElementRep(repeat:Option[RepeatAbs], element:ElementAbs)

  case class Group(elements:List[List[ElementRep]]) extends ElementAbs
  case class Opt(elements:List[List[ElementRep]]) extends ElementAbs
  case class Name(name:String) extends ElementAbs
  case class CharVal(value:String) extends ElementAbs
  case class ProseValue(value:String) extends ElementAbs
  case class Value(value:Int, rep:Option[Either[Int,Seq[Int]]]) extends ElementAbs

  case class DigitRepeat(value:Int) extends RepeatAbs
  case class RangeRepeat(start:Option[Int],end:Option[Int]) extends RepeatAbs