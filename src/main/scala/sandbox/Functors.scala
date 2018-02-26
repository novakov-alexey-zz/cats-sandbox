package sandbox

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
