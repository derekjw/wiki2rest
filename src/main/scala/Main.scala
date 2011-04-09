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

  def document = rep(block)

  def block: Parser[Block] = lf | heading | code | para

  def heading = (rep1('=') ^^ (_.length)) >> { l =>
    rep1(inline(("="*l))) <~ repN(l, '=') ^^ { x => Heading(l, x) }
  }

  def code = ("[[code" ~> opt(" format=\"" ~> text("\"") <~ "\"") <~ "]]") ~ (lf ~> rep1(text("[[code]]" | "\n") | lf) <~ "[[code]]" ~ lf) ^^ {
    case l ~ t => Code(l.map(_.text), t.map(Doc.text).mkString)
  }

  def para = rep1(not(code | heading | (lf ~ lf)) ~> multiline) ^^ { Para(_) }

  def inline(limit: Parser[Any]): Parser[Inline] =  target | link | html | emphtext | boldtext | codetext | text(limit | link | target | html | emphtext | boldtext | codetext)

  def multiline: Parser[MultiLine] = inline("\n") | lf

  def emphtext = "//" ~> rep1(not("//") ~> chr) <~ "//" ^^ { x => EmphText(x) }

  def boldtext = "**" ~> rep1(not("**") ~> chr) <~ "**" ^^ { x => BoldText(x) }

  def codetext = "{{" ~> rep1(not("}}") ~> chr) <~ "}}" ^^ { x => CodeText(x) }

  def text(limit: Parser[Any]) = rep1(not(limit) ~> chr) ^^ { x => Text(x) }

  def html = ("</" | "<") ~ ("span" | "div") ~ opt(text(">")) ~ ">" ^^ {
    case l ~ e ~ Some(Text(t)) ~ r => HTML(l+e+t+r)
    case l ~ e ~ None ~ r => HTML(l+e+r)
  }

  def target = "[[#" ~> rep1(not("]]") ~> chr) <~ "]]" ^^ { x => Target(x) }

  def link = "[[" ~> rep1(not("]]" | '|') ~> chr) ~ opt('|' ~> rep1(not("]]") ~> chr)) <~ "]]" ^^ {
    case uri ~ Some(text) => Link(uri, Some(text))
    case uri ~ None => Link(uri, None)
  }

  def chr = elem("chr", c => (!c.isControl || c == '\u0009'))

  def lf = '\n' ^^^ LF

  def apply[T](parser: Parser[T], in: Input): T = phrase(parser)(in) match {
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
    case Code(l,s) => s
    case Heading(l,c) => text(c)
    case Text(s) => s
    case EmphText(s) => s
    case BoldText(s) => s
    case CodeText(s) => s
    case LF => "\n"
    case Link(u,s) => u
    case Target(s) => "#"+s
    case HTML(s) => ""
  }

  def text(in: Seq[Doc]): String = ("" /: in)(_ + text(_))

  def wiki(in: Doc): String = in match {
    case Para(c) => wiki(c)
    case Code(Some(l), s) => "[[code format=\""+l+"\"]]\n"+s+"[[code]]\n"
    case Code(None, s) => "[[code]]\n"+s+"[[code]]\n"
    case Heading(l,c) => ("="*l)+wiki(c)+("="*l)
    case Text(s) => s
    case EmphText(s) => "//"+s+"//"
    case BoldText(s) => "**"+s+"**"
    case CodeText(s) => "{{"+s+"}}"
    case LF => "\n"
    case Link(u,Some(s)) => "[["+u+"|"+s+"]]"
    case Link(u,None) => "[["+u+"]]"
    case Target(s) => "[[#"+s+"]]"
    case HTML(s) => s
  }

  def wiki(in: Seq[Doc]): String = ("" /: in)(_ + wiki(_))

  def reSt(in: Doc): String = in match {
    case Para(c) => reSt(c)+"\n"
    case Code(Some(l), s) => ".. code-block:: "+reStLang(l)+"\n\n"+s.lines.map(x => "  "+x+"\n").mkString+"\n"
    case Code(None, s) => "::\n\n"+s.lines.map(x => "  "+x+"\n").mkString+"\n"
    case Heading(l,c) =>
      val hchr = l match {
        case 1 => "="
        case 2 => "-"
        case 3 => "^"
        case 4 => "*"
        case 5 => "\""
      }
      val s = reSt(c)
      s+"\n"+(hchr*s.length)+"\n\n"
    case Text(s) => s
    case EmphText(s) => "*"+s+"*"
    case BoldText(s) => "**"+s+"**"
    case CodeText(s) => "``"+s+"``"
    case LF => "\n"
    case Link("toc", None) => ""
    case Link(u,Some(s)) => "`"+s+" <"+u+">`_"
    case Link(u,None) => "`<"+u+">`_"
    case Target(s) => "" //"_`"+s+"`"
    case HTML(s) => ""
  }

  def reSt(in: Seq[Doc]): String = {
    def cleanLF(s: String): String = {
      val cleaned = s.replaceAll(" \n", "\n").replaceAll("\n\n\n", "\n\n")
      if (cleaned == s) s
      else cleanLF(cleaned)
    }
    cleanLF(("" /: in)(_ + reSt(_)))
  }

  def reStLang(in: String) = in match {
    case "java5" => "java"
    case s => s
  }
}

sealed trait Doc
sealed trait Block extends Doc
sealed trait MultiLine extends Doc
sealed trait Inline extends MultiLine

case class Para(contents: List[MultiLine]) extends Block
case class Text(text: String) extends Inline
case class EmphText(text: String) extends Inline
case class BoldText(text: String) extends Inline
case class CodeText(text: String) extends Inline
case class BList(contents: List[ListItem]) extends Block
case class NList(contents: List[ListItem]) extends Block
case class ListItem(contents: List[Inline]) extends Block
case object LF extends MultiLine with Block
case class Code(lang: Option[String], text: String) extends Block
case class Heading(level: Int, contents: List[Inline]) extends Block
case class Link(uri: String, text: Option[String]) extends Inline
case class Target(text: String) extends Inline
case class HTML(html: String) extends Inline
