package sandbox.cases.crdt

import cats.Monoid
import cats.instances.int._
import cats.instances.map._
import cats.syntax.semigroup._

final case class GCounter(counters: Map[String, Int])  {
  def increment(machine: String, amount: Int) = GCounter(counters |+| Map(machine -> amount) )

  def merge(that: GCounter): GCounter = GCounter(counters |+| that.counters)

  def total: Int = counters.values.sum
}

trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(a1: A, a2: A): A

  def empty: A
}

object Main extends App {
  val counter1 = GCounter(Map("c" -> 0, "b" -> 1))
  val newCounter = counter1.increment("b", 2)
  println(newCounter)
  println(newCounter.total)
  val counter2 = GCounter(Map("c" -> 1, "b" -> 1))
  val mergedCounter = counter1.merge(counter2)
  println(mergedCounter)

  implicit val intSemiLattice = new BoundedSemiLattice[Int] {
    override def combine(a1: Int, a2: Int): Int = a1.max(a2)

    override def empty: Int = 0
  }

  implicit def setSemiLattice[A](implicit monoid: Monoid[A]) = new BoundedSemiLattice[Set[A]] {
    override def combine(a1: Set[A], a2: Set[A]): Set[A] =
      a1.union(a2)

    override def empty: Set[A] = Set()
  }
}