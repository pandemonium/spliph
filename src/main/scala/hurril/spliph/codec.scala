package hurril.spliph
package codec

import cats.data._,
       cats.implicits._
import data.markup._

// Don't I need a Cursor API that can be positioned on a node
// in a way that also permits jumping across siblings.
object Decoder {
  type Result[A] = Either[Error.T, A]
  type T[A]      = Kleisli[Result, Element.T, A]
  type U[A]      = Kleisli[Result, String, A]

  object Error {
    sealed trait T
    case class ExpectedChild(of: Element.T,
                          named: Qname.T)
      extends T

    def expectedChild(of: Element.T,
                   named: Qname.T): T =
      ExpectedChild(of, named)
  }

  def textOfFirstChildNamed(name: Qname.T): T[String] = instance { parent =>
    Element.firstChildNamed(parent)(name)
           .map(Element.text)
           .toRight(Error.expectedChild(parent, name))
  }

  def unable[A](error: Error.T): T[A]              = Kleisli(_ => error.asLeft)
  def able[A](value: A): T[A]                      = Kleisli(_ => value.asRight)
  def instance[A](f: Element.T => Result[A]): T[A] = Kleisli(f)
}

trait SampleData {
  val breakfastMenu = """
    |<?xml version="1.0" encoding="UTF-8"?>
    |<breakfast_menu>
    |<food>
    |    <name>Belgian Waffles</name>
    |    <price>$5.95</price>
    |    <description>
    |  Two of our famous Belgian Waffles with plenty of real maple syrup
    |  </description>
    |    <calories>650</calories>
    |</food>
    |<food>
    |    <name>Strawberry Belgian Waffles</name>
    |    <price>$7.95</price>
    |    <description>
    |    Light Belgian waffles covered with strawberries and whipped cream
    |    </description>
    |    <calories>900</calories>
    |</food>
    |<food>
    |    <name>Berry-Berry Belgian Waffles</name>
    |    <price>$8.95</price>
    |    <description>
    |    Belgian waffles covered with assorted fresh berries and whipped cream
    |    </description>
    |    <calories>900</calories>
    |</food>
    |<food>
    |    <name>French Toast</name>
    |    <price>$4.50</price>
    |    <description>
    |    Thick slices made from our homemade sourdough bread
    |    </description>
    |    <calories>600</calories>
    |</food>
    |<food>
    |    <name>Homestyle Breakfast</name>
    |    <price>$6.95</price>
    |    <description>
    |    Two eggs, bacon or sausage, toast, and our ever-popular hash browns
    |    </description>
    |    <calories>950</calories>
    |</food>
    |</breakfast_menu>
  """.stripMargin
}

object RunDecode extends App {
  object BreakfastModel {
    case class BreakfastMenu(foods: List[Food])
    case class Food(name: String,
                   price: String,
             description: String,
                calories: Int)

    implicit val decodeFood: Decoder.T[Food] = for {
      name        <- Decoder.textOfFirstChildNamed(Qname.make("name"))
      price       <- Decoder.textOfFirstChildNamed(Qname.make("price"))
      description <- Decoder.textOfFirstChildNamed(Qname.make("description"))
      calories    <- Decoder.textOfFirstChildNamed(Qname.make("calories"))
    } yield ???
  }
}