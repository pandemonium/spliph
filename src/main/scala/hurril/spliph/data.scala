package hurril.spliph
package data

import cats._,
       cats.implicits._


package object markup {
  // This thing might just as well be a linked list of siblings
  // encoded in each Node.
  type Nodes      = List[Node.T]
  type Attributes = List[Element.Attribute]

  implicit val showNodes: Show[Nodes] = Show { ns =>
    ns.map(_.show).mkString(" ")
  }

  object Qname {
    sealed trait T
    case class Null(name: String)
      extends T
    case class Qualified(prefix: String,
                           name: String)
      extends T

    def make(name: String): T = 
      Null(name)

    def qualified(prefix: String, name: String): T = 
      Qualified(prefix, name)

    implicit val showQname: Show[T] = Show {
      case Null(name)              => name
      case Qualified(prefix, name) => s"$prefix:$name"
    }
  }

  object Node {
    object Type {
      sealed trait T
      case object Text 
        extends T
      case object Element 
        extends T
    }

    sealed trait T
      extends Signature

    sealed trait Signature {
      def nodeType: Type.T
    }

    sealed abstract class Template(val nodeType: Type.T) {
      self: Signature =>
    }

    case class Content(text: String)
      extends Template(Type.Text)
         with T

    case class Markup(element: Element.T)
      extends Template(Type.Element)
         with T

    def makeContent(text: String): T      = Content(text)
    def makeMarkup(element: Element.T): T = Markup(element)
  
    // Is this a fold left or right?
    def fold[A](tree: T, z: A)(f: (T, A) => A): A = tree match {
      case t @ Content(_) => 
        f(t, z)
      case e @ Markup(Element.Empty(_, _)) =>
        f(e, z)
      case e @ Markup(Element.NonEmpty(_, _, children)) =>
        children.foldLeft(f(e, z))((b, a) => fold(a, b)(f))
    }

    implicit val showNode: Show[T] = Show {
      case Content(text)   => text
      case Markup(element) => element.show
    }
  }

  object Element {
    type Attribute = (Qname.T, String)

    sealed trait T
      extends Signature
    case class Empty(name: Qname.T, 
               attributes: Attributes)
      extends T
    case class NonEmpty(name: Qname.T, 
                  attributes: Attributes,
                  childNodes: Nodes)
      extends T
         with WithChildren

    sealed trait WithChildren { self: NonEmpty =>
      def withChildren(children: Nodes): NonEmpty =
        copy(childNodes = children)
    }

    sealed trait Signature {
      def name: Qname.T
      def attributes: Attributes
    }

    def children(el: T): List[T] = el match {
      case ChildNodes(_) => 
//        children
        List.empty
    }

    object ChildNodes {
      def unapply(t: T): Option[Nodes] = t match {
        case Empty(_, _)        => List.empty.some
        case NonEmpty(_, _, cs) => cs.some
      }
    }

    object TextContent {
      def unapply(t: T): Option[List[String]] = t match {
        case ChildNodes(cs) => cs.collect {
          case Node.Content(text) => text
        }.some
      }
    }

    def makeEmpty(name: Qname.T, 
            attributes: Attribute*): T = 
      Empty(name, attributes.toList)

    def make(name: Qname.T, 
       attributes: Attribute*): T = 
      NonEmpty(name, attributes.toList, List.empty)

    // First transform into list of atoms surrounded
    // by <> for showing.
    implicit val showElement: Show[T] = Show {
      case Empty(name, attributes) =>
        s"<${name.show} ${attributes.show} />"
      case NonEmpty(name, attributes, children) =>
        s"""<${name.show} ${attributes.show}>${children.show}</${name.show}>"""
    }

    implicit val showAttribute: Show[Attribute] = Show {
      case (name, value) => s"""${name.show}="$value""""
    }

    implicit val showAttributes: Show[Attributes] = Show { as =>
      as.map(_.show).mkString(" ")
    }
  }

  object Document {
    sealed trait T
      extends Signature

    case class Xml(encoding: String, 
                 standalone: Boolean, // Replace with two different constructors?
                       root: Node.Markup)
      extends T

    sealed trait Signature {
      def root: Node.T
    }

    def make(encoding: String, 
           standalone: Boolean,
                 root: Element.T): T =
      Xml(encoding, standalone, Node.Markup(root))

    implicit val showDocument: Show[T] = Show {
      case Xml(enc, s, root) =>
        val standalone = if (s) "yes" else "no"
        s"""|<?xml version="1.0" encoding="$enc" standalone="$standalone"?>
            |${(root: Node.T).show}
        """.stripMargin
    }
  }
}