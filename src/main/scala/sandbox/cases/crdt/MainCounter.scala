package sandbox.cases.crdt

import cats.Monoid
import cats.instances.list._
import cats.instances.map._
import cats.syntax.foldable._
import cats.syntax.semigroup._ // for combineAll

trait KeyValueStore[F[_, _]] {
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

  implicit def setSemiLattice[A] = new BoundedSemiLattice[Set[A]] {
    override def combine(a1: Set[A], a2: Set[A]): Set[A] =
      a1.union(a2)

    override def empty: Set[A] = Set()
  }

  implicit def storeMapInst: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)
           (implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(key)

    def getOrElse(key: K, default: V)
                 (implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }

  implicit def gcounterInstance[F[_, _], K, V]
  (implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]) =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)
                   (implicit m: Monoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }

      def merge(f1: F[K, V], f2: F[K, V])
               (implicit b: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2

      def total(f: F[K, V])(implicit m: Monoid[V]): V =
        f.values.combineAll
    }


  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)
  val counter = GCounter[Map, String, Int]
  val merged = counter.merge(g1, g2)
  val total = counter.total(merged)
  println(merged)
  println("total = " + total)
}