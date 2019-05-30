package hurril.spliph
package parser

import scala.util.parsing.combinator._
import cats.implicits._
import data.markup._
import hurril.spliph.codec.SampleData


object Xml extends JavaTokenParsers {
  def qname = qualifiedName | nullName

  def qualifiedName = ident ~ (":" ~> ident) ^^ {
    case prefix ~ name => Qname.qualified(prefix, name)
  }

  def nullName = ident ^^ Qname.make

  def document = xmlDeclaration ~ element ^^ {
    case xmlDecl ~ root =>
      // This is mildly crappy
      Document.make(
        xmlDecl.get("encoding") getOrElse "utf-8",
        xmlDecl.get("standalone") exists "yes".==,
        root
      )
  }

  def xmlDeclaration = "<?xml" ~> (declaration.+ <~ "?>") ^^ {
    case decls => decls.toMap
  }

  def declaration = declarationKeyword ~ ("=" ~> stringLiteral) ^^ {
    case key ~ value => key -> value
  }

  def declarationKeyword = "version" | "standalone" | "encoding"

  def node = content | markup

  def attributes = attribute.*

  def attribute = qname ~ ("=" ~> stringLiteral) ^^ {
    case name ~ value => 
      name -> value.drop(1).dropRight(1)
  }

  def content = text ^^ Node.makeContent

  def markup = element ^^ Node.makeMarkup

  def text = "[^<>]+".r

  def element: Parser[Element.T] = emptyElement | nonEmptyElement

  def emptyElement = "<" ~> (qname ~ attributes <~ "/>") ^^ {
    case name ~ attributes => 
      Element.Empty(name, attributes)
  }

  def openElement  = "<" ~> (qname ~ attributes <~ ">") ^^ {
    case name ~ attributes => 
      Element.NonEmpty(name, attributes, List.empty)
  }

  def closeElement(name: Qname.T) =
    "</" ~ name.show ~ ">"

  def nonEmptyElement: Parser[Element.NonEmpty] = for {
    open <- openElement
    cs   <- children
    _    <- closeElement(open.name)
  } yield open.withChildren(cs)

  def children: Parser[List[Node.T]] = node.*
}

object RunXml extends App with SampleData {
  val xml = 
  """|<?xml version="1.0"?>
     |<html>
     |  <head>
     |    <title>Hi, mom</title>
     |  </head>
     |  <body onload="window.close();">
     |    <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
     |  </body>
     |</html>
  """.stripMargin

  val xml2 =
  """|<?xml version="1.0"?>
     |<html>
     |  <foo:head>
     |    <title>Hello, world</title>
     |  </foo:head>
     |  <body onload="alert('hi, mom');">
     |  </body>
     |</html>
  """.stripMargin

  Xml.parseAll(Xml.document, breakfastMenu)
     .map(doc => Node.fold(doc.root, List.empty[Node.T])(_ :: _))
     .map { nodes =>
        nodes.reverse.map {
          case Node.Content(text) => 
            text.mkString("/", "", "/")
          case Node.Markup(element) =>
            element.name.show.mkString("<", "", ">")
      }.show
    }
    .map(x => println(x))
}