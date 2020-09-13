import bnf.BnfName

package object transformer {

  private val InternalRule = """(_[0-9]+)""".r

  private [transformer] def isInternalRule(name: BnfName):Boolean = name match {
    case BnfName(n) => n match {
      case InternalRule(_) => true
      case _ => false
    }
  }

}
