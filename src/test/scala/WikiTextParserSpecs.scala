package akka.wiki2rest

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Reader, CharSequenceReader}

class WikiTextParserSpec extends WordSpec with ShouldMatchers {
  val parser = new WikiTextParser

  implicit def str2reader(str: String): Reader[Char] = new CharSequenceReader(str)

  "parsing headers" should {
    "succeed when simple" in {
      parser(parser.heading, "=I'm a Heading=") should equal(Heading(1, List(Text("I'm a Heading"))))
      parser(parser.heading, "==I'm a Heading==") should equal(Heading(2, List(Text("I'm a Heading"))))
    }

    "succeed when followed by text" in {
      parser(parser.document, "=Heading=\nother text\n") should equal(List(Heading(1, List(Text("Heading"))), LF, Para(List(Text("other text"), LF))))
    }
  }

  "parsing Link" should {
    "succeed when simple" in {
      parser(parser.link, "[[http://example.org|Example Link]]") should equal(Link("http://example.org", Some("Example Link")))
    }
    "succed when inline" in {
      parser(parser.para, "This is a test of [[http://example.org|Example Link]] is a test") should equal(Para(List(Text("This is a test of "), Link("http://example.org", Some("Example Link")), Text(" is a test"))))
    }
  }

  "parsing a document" should {
    "succeed with futures-scala" in {
      val reader = new java.io.BufferedReader(new java.io.InputStreamReader(getClass.getClassLoader.getResourceAsStream("futures-scala")))
      val original = Stream.continually(Option(reader.readLine)).takeWhile(_.isDefined).flatten.mkString("\n")
      reader.close
      val parsed = parser(parser.document, original)
      Doc.wiki(parsed) should equal(original)
    }
    /*"succeed with futures-scala to reSt" in {
      val reader = new java.io.BufferedReader(new java.io.InputStreamReader(getClass.getClassLoader.getResourceAsStream("futures-scala")))
      val original = Stream.continually(Option(reader.readLine)).takeWhile(_.isDefined).flatten.mkString("\n")
      reader.close
      val parsed = parser(parser.document, original)
      println(Doc.reSt(parsed))
      true should equal(true)
    }*/
  }
}
