package sandbox

object MonoidApp extends App {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] =
      monoid
  }

  val booleanOrMonoid = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  assert(booleanOrMonoid.combine(booleanOrMonoid.empty, y = true))
  assert(booleanOrMonoid.combine(x = false, y = true))
}

object MonoidEx2 extends App {
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.double._
  import cats.instances.option._
  import cats.syntax.semigroup._
  import cats.syntax.option._

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid = new Monoid[Order] {
    override def empty: Order = Order(Monoid[Double].empty, Monoid[Double].empty)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  println(add(List(1, 2, 3)))
  println(add(List(Some(1), 2.some, 3.some, None)).getOrElse(0))
  println(add(List(Order(10, 2), Order(5, 4))))
}
