package sandbox.cases.crdt

import cats.Monoid
import cats.instances.list._
import cats.instances.map._
import cats.syntax.foldable._
import cats.syntax.semigroup._ // for combineAll

//final case class GCounter[A](counters: Map[String, A])  {
//  def increment(machine: String, amount: A)(implicit monoid: Monoid[A]): GCounter[A] = {
//    val value = amount |+| counters.getOrElse(machine, monoid.empty)
//    GCounter(counters + (machine -> value) )
//  }
//
//  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] = {
//    GCounter(counters |+| that.counters)
//  }
//
//  def total(implicit monoid: Monoid[A]): A = counters.values.toList.combineAll
//}

trait KeyValueStore[F[_,_]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)
  def values[K, V](f: F[K, V]): List[V]
}

object Main extends App {
  implicit val intSemiLattice = new BoundedSemiLattice[Int] {
    override def combine(a1: Int, a2: Int): Int = a1.max(a2)

    override def empty: Int = 0
  }

  implicit def setSemiLattice[A](implicit monoid: Monoid[A]) = new BoundedSemiLattice[Set[A]] {
    override def combine(a1: Set[A], a2: Set[A]): Set[A] =
      a1.union(a2)

    override def empty: Set[A] = Set()
  }

  implicit def mapInst[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] = {
      val total = f.getOrElse(k, m.empty) |+| v
      f + (k -> total)
    }

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
      f1 |+| f2


    override def total(f: Map[K, V])(implicit m: Monoid[V]): V = f.values.toList.combineAll
  }

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)
  val counter = GCounter[Map, String, Int]
  val merged = counter.merge(g1, g2)
  val total  = counter.total(merged)
  println(merged)
  println("total = " + total)
}