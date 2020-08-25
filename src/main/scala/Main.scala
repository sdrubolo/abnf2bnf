import transformer.Abnf2Bnf
import abnf.AbnfParser
import abnf.AbnfLex
import scala.io.Source
import java.io.File
import java.io.PrintWriter
import bnf.BnfSimplify
import scala.util.parsing.input.StreamReader

object Main {

  def main(args: Array[String]): Unit = {
    if(args.length == 1) {
      val fileName = args(0)
      val reader = StreamReader( Source.fromFile(fileName,"UTF-8").reader() )
      val bnf = (AbnfLex(reader) flatMap { tokens => AbnfParser(tokens) }) flatMap { abnf => Abnf2Bnf(abnf) }
      bnf map (BnfSimplify(_)) match {
        case Left(e) => println(e)
        case Right(e) =>
          val writer = new PrintWriter(new File(s"$fileName.bnf"))
          writer.write(e.toString())
          writer.close()
      }
    }
    else {
      println("Error: missing file name")
    }
  }

}