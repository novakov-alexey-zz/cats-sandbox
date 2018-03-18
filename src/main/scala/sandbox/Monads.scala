package sandbox

import cats.Eval
import cats.data.{Reader, Writer}

import scala.language.higherKinds

class Monads {
  type Id[A] = A

  val idMonad = new Monad[Id] {
    override def pure[A](a: A): Id[A] = a

    override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

    override def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
  }
}

trait Monad[F[_]] {

  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(a => pure(func(a)))
}

object Monands extends App {

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def foldRightEval(as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
      case head :: tail =>
        Eval.defer(foldRightEval(tail, acc)(fn).map(b => fn(head, b)))
      case Nil =>
        Eval.now(acc)
    }

    foldRightEval(as, acc)(fn).value
  }

  def foldRight2[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
    case head :: tail =>
      fn(head, foldRight(tail, acc)(fn))
    case Nil =>
      acc
  }

  //  println(foldRight(List(4, 4, 5, 1), 1)((a, b) => {println(a * b); a * b}))
}

object Writers {

  import cats.instances.vector._
  import cats.syntax.applicative._
  import cats.syntax.writer._ // for Monoid

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  val result = Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(6))
  )), 5.seconds)

  result.foreach(println)
}

object Readers extends App {

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId)
      .flatMap(name => name.map(n => checkPassword(n, password))
        .getOrElse(Reader(_ => false)))

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )
  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))
}