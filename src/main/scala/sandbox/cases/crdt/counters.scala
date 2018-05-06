package sandbox.cases.crdt

import cats.Monoid

trait GCounter[F[_,_],K, V] {
  def increment(f: F[K, V])(k: K, v: V)
               (implicit m: Monoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])
           (implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])
           (implicit m: Monoid[V]): V
}
object GCounter {
  def apply[F[_,_], K, V]
  (implicit counter: GCounter[F, K, V]) =
    counter
}


trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(a1: A, a2: A): A

  def empty: A
}

