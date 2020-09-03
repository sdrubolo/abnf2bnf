package transformer

import abnf.AbnfAbs
import bnf.BnfAbs

object AbnfTransformer {

  def apply(abnf: AbnfAbs): Either[String, BnfAbs] = Abnf2Bnf(abnf) map { bnf => BnfSimplify(bnf)}

}
