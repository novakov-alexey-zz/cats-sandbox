package sandbox.semigroupal

import cats.syntax.functor._ // for map
import cats.syntax.flatMap._

object Semigroupal extends App{
  import cats.Monad

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    x.flatMap(a => y.map(b => (a, b)))
  }

}
