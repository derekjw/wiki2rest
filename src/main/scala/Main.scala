package akka.wiki2rest

import scala.util.parsing.combinator.Parsers
import java.io.{File, FileReader, BufferedReader, FileWriter, BufferedWriter}
import scala.util.parsing.input.CharSequenceReader

object Main extends App {

  val parser = new WikiTextParser

  args foreach { arg =>
    val reader = new BufferedReader(new FileReader(new File(arg)))
    val doc = Doc.reSt(parser(parser.document, new CharSequenceReader(Stream.continually(Option(reader.readLine)).takeWhile(_.isDefined).flatten.mkString("\n"))))
    reader.close
    val writer = new BufferedWriter(new FileWriter(new File(arg+".rst")))
    writer.write(doc,0,doc.length)
    writer.close
  }
}

class WikiTextParser extends Parsers {
  type Elem = Char

  def document = rep(lf | h5 | h4 | h3 | h2 | h1 | code | para)

  def h1 = heading(1) ^^ { x => Heading(1, x) }
  def h2 = heading(2) ^^ { x => Heading(2, x) }
  def h3 = heading(3) ^^ { x => Heading(3, x) }
  def h4 = heading(4) ^^ { x => Heading(4, x) }
  def h5 = heading(5) ^^ { x => Heading(5, x) }

  def heading(level: Int) = repN(level, '=') ~> rep1(not("=") ~> chr) <~ repN(level, '=')

  def para = rep1(not(code | h5 | h4 | h3 | h2 | h1 | (lf ~ lf)) ~> (text | link | target | tag | lf)) ^^ { Para(_) }

  def text = rep1(not("[[") ~> chr) ^^ { x => Text(x) }

  def target = block('#' ~> rep1(not("]]") ~> chr)) ^^ { x => Target(x) }

  def tag = block(rep1(not("]]") ~> chr)) ^^ { x => Tag(x) }

  def link = block(rep1(not("]]" | '|') ~> chr) ~ '|' ~ rep1(not("]]") ~> chr)) ^^ {
    case uri ~ '|' ~ text => Link(uri, text)
  }

  def code = block("code format=\"scala\"") ~ lf ~> rep1(text | lf) <~ block("code") ~ lf ^^ { x => Code(x.map(Doc.text).mkString) }

  def block[T](p: Parser[T]) = '[' ~ '[' ~> p <~ ']' ~ ']'

  def chr = elem("chr", c => !c.isControl)

  def lf = '\n' ^^^ LF

  def apply[T](parser: Parser[T], in: Input): T = parser(in) match {
    case Success(r, n) => r
    case NoSuccess(msg, n) =>
      val pos = n.pos
      val msg = """Failed to parse line %d:
      |%s
      |""".stripMargin.format(pos.line, pos.longString)
      throw new RuntimeException(msg)
  }

  implicit def charseq2string(in: Seq[Char]): String = in.mkString

  implicit def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      var i = 0
      var j = offset
      while (i < s.length && j < source.length && s.charAt(i) == source.charAt(j)) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(source.subSequence(offset, j).toString, in.drop(j - offset))
      else
        Failure("`"+s+"' expected but `"+in.first+"' found", in)
    }
  }
}

object Doc {
  def text(in: Doc): String = in match {
    case Para(c) => text(c)
    case Text(s) => s
    case LF => "\n"
    case Code(s) => s
    case Heading(l,s) => s
    case Link(u,s) => s
    case Tag(s) => s
    case Target(s) => "#"+s
  }

  def text(in: Seq[Doc]): String = ("" /: in)(_ + text(_))

  def wiki(in: Doc): String = in match {
    case Para(c) => wiki(c)
    case Text(s) => s
    case LF => "\n"
    case Code(s) => "[[code format=\"scala\"]]\n"+s+"[[code]]\n"
    case Heading(l,s) => ("="*l)+s+("="*l)
    case Link(u,s) => "[["+u+"|"+s+"]]"
    case Tag(s) => "[["+s+"]]"
    case Target(s) => "[[#"+s+"]]"
  }

  def wiki(in: Seq[Doc]): String = ("" /: in)(_ + wiki(_))

  def reSt(in: Doc): String = in match {
    case Para(c) => reSt(c)
    case Text(s) => s
    case LF => "\n"
    case Code(s) => "::\n"+s.lines.map(l => "  "+l+"\n").mkString+"\n"
    case Heading(l,s) =>
      val hchr = l match {
        case 1 => "="
        case 2 => "-"
        case 3 => "*"
        case 4 => "^"
        case 5 => "#"
      }
      s+"\n"+(hchr*s.length)+"\n"
    case Link(u,s) => "`"+s+" <"+u+">`_"
    case Tag(s) => "[["+s+"]]"
    case Target(s) => "_`"+s+"`"
  }

  def reSt(in: Seq[Doc]): String = ("" /: in)(_ + reSt(_))
}

sealed trait Doc

case class Para(contents: List[Doc]) extends Doc
case class Text(text: String) extends Doc
case object LF extends Doc
case class Code(text: String) extends Doc
case class Heading(level: Int, text: String) extends Doc
case class Link(uri: String, text: String) extends Doc
case class Tag(text: String) extends Doc
case class Target(text: String) extends Doc
