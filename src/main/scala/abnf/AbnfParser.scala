package abnf

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}

case class AbnfParserError(location: Location,msg: String) extends AbnfCompilationError
case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}

class AbnfTokenReader(tokens: Seq[AbnfToken]) extends Reader[AbnfToken] {
  override def first: AbnfToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = if(!atEnd) { first.pos } else NoPosition
  override def rest: Reader[AbnfToken] = new AbnfTokenReader(tokens.tail)
}

object AbnfParser extends Parsers {
  override type Elem = AbnfToken

  private object AbnfParserAux {

    def rules : Parser[AbnfAbs] = (((emptyRule*) ~> rule <~ (emptyRule*))+) ^^ {
      rules => Rules(rules)
      }

    def emptyRule : Parser[Object] =  (whiteSpace*) ~> c_nl
    def rule : Parser[RuleAbs] = (ruleName ~ defined_as ~ alternation <~ c_nl) ^^ {
        case name ~ definedAs ~ body => Rule(name,definedAs,body)
      }

    def alternative : Parser[List[Serializable] ~ Elem ~ List[Serializable]] = (whiteSpace*) ~ VCHAR("/") ~ (whiteSpace*)

    def alternation: Parser[List[List[ElementRep]]] = (concatenation ~ ((alternative ~> concatenation)*) ~ (whiteSpace*)) ^^ {
      case rep ~ reps ~ _ => rep :: reps
    }

    def concatenation : Parser[List[ElementRep]] = (repetition ~ (((whiteSpace+) ~> repetition)*)) ^^ {
      case rep ~ reps => rep :: reps
    }

    private def mapOptionToInt(value:Option[String]):Option[Int] = value map (_.toInt)

    def repetition : Parser[ElementRep] = ((repeat?) ~ element) ^^ { case repeat ~ element => ElementRep(repeat,element) }
    def repeat : Parser[RepeatAbs] = (digits ||| (opt(digits) ~ VCHAR("*") ~ opt(digits))) ^^ {
        case (d:Option[_]) ~ _ ~ (v:Option[_]) =>
          val start = d.asInstanceOf[Option[String]]
          val end = v.asInstanceOf[Option[String]]
          RangeRepeat(mapOptionToInt(start),mapOptionToInt(end))
        case d : String => DigitRepeat(d.toInt)
    }

    def element  : Parser[ElementAbs] = ruleName ||| group ||| optional ||| charVal ||| numValue ||| prose
    def ruleName : Parser[Name]       = (alpha ~ ((digits | alpha | ruleNameDash)*)) ^^ { case name ~ names => Name((name::names) reduce (_+_)) }
    def group    : Parser[ElementAbs] = (VCHAR("(") ~ (whiteSpace*)) ~> alternation <~ ((whiteSpace*) ~ VCHAR(")")) ^^ { elements => Group(elements) }
    def optional : Parser[ElementAbs] = (VCHAR("[") ~ (whiteSpace*)) ~> alternation <~ ((whiteSpace*) ~ VCHAR("]")) ^^ { elements => Opt(elements) }
    def prose    : Parser[ElementAbs] = accept("prose", { case PROSE(any) => ProseValue(any) })
    def assignment : Parser[AssignmentDef.DefinedAs] = accept("assignment", { case ASSIGNMENT(value) => value })
    def digits     : Parser[String]        = (digit+) ^^ ( digit => digit.reduce (_+_) )
    def digit      : Parser[String]        = accept("digit", { case VCHAR(value) if """[0-9]""".r matches value => value })
    def new_line   : Parser[Elem]          = accept("new line", { case c @ CRLF() => c })
    def charVal    : Parser[ElementAbs]    = accept("quoted string", { case QUOTEDSTRING(value) => CharVal(value) })
    def alpha      : Parser[String]        = accept("alpha", { case VCHAR(v) if """[A-Za-z]""".r matches v => v })
    def numValue   : Parser[ElementAbs]
      = VCHAR("%") ~>
        (value("b",bit,Base.Binary) | value("d",digit,Base.Decimal) | value("x",hex,Base.Hex))

    private def value(initial:String,parser:Parser[String],v:Base.Value)
      = VCHAR(initial) ~> (parser+) ~ ((cons(parser) | range(parser))?) ^^ { case tokens ~ body => buildValue (v,tokens,body) }

    def bit        : Parser[String]        =  accept("bit", { case VCHAR(d) if """[01]""".r matches d => d })
    def hex        : Parser[String]        =  accept("hex", { case VCHAR(v) if """[A-Fa-f0-9]""".r matches v => v })

    private def cons(token:Parser[String]) = ((VCHAR(".") ~> (token+))+) ^^ { values => values map (_ reduce (_+_))}
    private def range(token:Parser[String]) = VCHAR("-") ~> (token+) ^^ { (values:Seq[String]) => values reduce (_+_) }

    private def valueToBase(ty:Base.Value,value:String):Int = Integer.parseInt(value, ty.base)

    private def buildValue(ty:Base.Value, tokens:Seq[String], body:Option[Serializable]) = {
        val baseConverter = valueToBase(ty,_)
        val start = baseConverter(tokens reduce (_+_))
        val rep = body match {
          case Some(cons:Seq[String]) => Some(Right(cons map baseConverter))
          case Some(cons:String) => Some(Left(baseConverter(cons)))
          case _ => None
        }
      Value(start, rep)
    }

    private def space      = accept("space", { case WSP(value) => value })
    private def defined_as = (whiteSpace*) ~> assignment <~ (whiteSpace*)
    private def whiteSpace = space | (c_nl ~ space)
    private def c_nl       = comment | new_line
    private def comment    = COMMENT() ~ new_line


    def ruleNameDash : Parser[String] = { accept("dash", { case VCHAR("-") => "-" }) }

  }

  @tailrec
  private def parse(reader: Reader[AbnfToken]) : Either[AbnfParserError, AbnfAbs] = AbnfParserAux.rules(reader) match {
    case NoSuccess(msg, next) => Left(AbnfParserError(Location(next.pos.line, next.pos.column),msg))
    case Success(result, next) => if (next.atEnd) { Right(result) } else { parse(next) }
  }

  def apply(tokens: Seq[AbnfToken]): Either[AbnfParserError, AbnfAbs] = parse(new AbnfTokenReader(tokens))
}
