package sandbox

import cats.Eq

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit printable: functor.Printable[A]): String =
    printable.format(value)

  def print[A](value: A)(implicit printable: functor.Printable[A]): Unit =
    println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit printable: functor.Printable[A]): String = printable.format(a)
    def print(implicit printable: functor.Printable[A]): Unit = println(printable.format(a))
  }
}

final case class Cat(name: String, age: Int, color: String)

object PrintableInstances {
  implicit val stringPrintable = new functor.Printable[String] {
    override def format(value: String): String = "s." + value
  }

  implicit val intPrintable = new functor.Printable[Int] {
    override def format(value: Int): String = "i." + value.toString
  }

  implicit val catPrintable = new functor.Printable[Cat] {
    override def format(value: Cat): String =
      s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}

object Part1_ex1 extends App {
  import PrintableInstances._
  import Printable._
  import PrintableSyntax._

  print("some string")
  Cat("blink", 5, "gray").print
}

object Part2_ex2 extends App {
  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val catShow: Show[Cat] = Show.show(c => {
    val name = c.name.show
    val age = c.age.show
    val color = c.color.show
    s"$name is a $age year-old $color cat."
  })

  println(Cat("blink", 5, "gray").show)
}

object Part1_ex3 extends App {
  import cats.syntax.eq._
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.option._

  implicit val catEq: Eq[Cat] = Eq.instance { (c1, c2) =>
    c1.color === c2.color && c1.age === c2.age && c1.name == c2.name
  }

  val c1 = Cat("blink", 5, "gray")
  val c2 = Cat("blink", 5, "gray")

  println(c1 === c2)

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println("equal: " + (cat1 === cat2))
  println("not equal: " + (cat1 =!= cat2))

  println("equal: " + (optionCat1 === optionCat2))
  println("not equal: " + (optionCat1 =!= optionCat2))
}
