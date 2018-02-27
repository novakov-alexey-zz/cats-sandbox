package sandbox.functor

sealed trait Tree[+A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Functors extends App {

  import Tree._
  import cats.Functor
  import cats.syntax.functor._

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
    }
  }

  val f = (i: Int) => i + 1
  val res = branch(leaf(1), branch(leaf(2), leaf(3))).map(f)
  println(res)
}

trait Printable[A] { self =>
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    (value: B) => self.format(func(value))
}

final case class Box[A](value: A)

object Functors_ex2 extends  App {
  implicit val stringPrintable: Printable[String] =
    (value: String) => "\"" + value + "\""

  implicit val booleanPrintable: Printable[Boolean] =
    (value: Boolean) => if (value) "yes" else "no"

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    (b: Box[A]) => p.format(b.value)

  def format[A](v: A)(implicit p: Printable[A]): String = p.format(v)

  println(format("hello"))
  println(format(true))
  println(format(Box("box1")))
  println(format(Box("hello world")))
  //format(Box(123))
}