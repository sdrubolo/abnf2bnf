package abnf

import abnf.AssignmentDef._

import scala.language.postfixOps
import scala.util.parsing.combinator._
import scala.util.parsing.input.StreamReader

trait AbnfCompilationError
case class AbnfLexerError(location: Location,msg: String) extends AbnfCompilationError


object AbnfLex extends RegexParsers {

  override val skipWhitespace = false

  def wsp                   : Parser[AbnfToken] = positioned { """[\x09\x20]""".r ^^ WSP }
  def crlf                  : Parser[AbnfToken] = positioned { """[\x0D\x0A]""".r ^^ { _ => CRLF() } }
  def assignment            : Parser[AbnfToken] = positioned { """=""".r ^^ { _ => ASSIGNMENT(Assignment) } }
  def incrementalAssignment : Parser[AbnfToken] = positioned { """=/""".r ^^ { _ => ASSIGNMENT(Incremental) } }
  def quotedString          : Parser[AbnfToken] = positioned { ("\"" ~> """([\x20-\x21]|[\x23-\x7E])*""".r <~ "\"") map QUOTEDSTRING }
  def prose                 : Parser[AbnfToken] = positioned { ("<" ~> """([\x20-\x3D]|[\x3F-\x7E])*""".r <~ ">") map PROSE }
  def char                  : Parser[AbnfToken] = positioned { """[\x21-\x7E]""".r ^^ VCHAR }
  def comment               : Parser[AbnfToken] = positioned { """;[\x20\t\x21-\x7E]*""".r ^^ { _ => COMMENT() } }

  def tokens: Parser[List[AbnfToken]] = {
    phrase(rep1(wsp ||| crlf ||| char ||| quotedString ||| prose ||| incrementalAssignment ||| assignment ||| comment))
  }

  def apply(code: StreamReader): Either[AbnfLexerError, List[AbnfToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(AbnfLexerError(Location(next.pos.line, next.pos.column),msg))
      case Success(result, _) => Right(result)
    }
  }

  def apply(code: String): Either[AbnfLexerError, List[AbnfToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(AbnfLexerError(Location(next.pos.line, next.pos.column),msg))
      case Success(result, _) => Right(result)
    }
  }

}