import transformer.AbnfTransformer
import abnf.AbnfParser
import abnf.AbnfLex

import scala.io.Source
import java.io.File
import java.io.PrintWriter

import bnf.{BnfRules, Print}

import scala.util.parsing.input.StreamReader

object Main {

  def main(args: Array[String]): Unit = {
    if(args.length == 2) {
      val inFile = args(0)
      val outFile = args(1)
      val reader = StreamReader( Source.fromFile(inFile,"UTF-8").reader() )
      val bnf = (AbnfLex(reader) flatMap { tokens => AbnfParser(tokens) }) flatMap { abnf => AbnfTransformer(abnf) }
      bnf match {
        case Left(e) => System.err.println(s"[${Console.RED + "error" + Console.WHITE}] : $e")
        case Right(e @ BnfRules(_)) =>
          val writer = new PrintWriter(new File(outFile))
          writer.write(Print(e))
          writer.close()
      }
    }
    else {
      System.err.println(s"[${Console.RED + "error" + Console.WHITE}] : missing parameters")
    }
  }

}