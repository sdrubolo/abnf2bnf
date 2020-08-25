package abnf

import abnf.AssignmentDef._

import scala.util.parsing.input.Positional

sealed trait AbnfToken extends Positional

case class CRLF() extends AbnfToken
case class COMMENT() extends AbnfToken

case class ASSIGNMENT(space:DefinedAs) extends AbnfToken
case class WSP(space:String) extends AbnfToken
case class RULENAME(name:String) extends AbnfToken
case class QUOTEDSTRING(txt:String) extends AbnfToken
case class VCHAR(char:String) extends AbnfToken
case class PROSE(str:String) extends AbnfToken
