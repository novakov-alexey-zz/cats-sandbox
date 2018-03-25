package sandbox

import cats.Eval
import cats.data.{Reader, StateT, Writer}
import sandbox.functor.{Branch, Leaf, Tree}

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

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
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))
}

object States extends App {

  import cats.data.State
  import cats.syntax.applicative._ // for pure

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    State(stack => {
      Try(sym.toInt) match {
        case Success(v) => (v :: stack, v)
        case Failure(_) =>
          val r = calculate(sym, stack)
          val newSTack = stack.dropRight(2)
          (r :: newSTack, r)
      }
    })

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState])((s, str) => s.flatMap(_ => evalOne(str)))

  private def calculate(sym: String, stack: List[Int]) = {
    stack match {
      case a :: b :: _ =>
        sym match {
          case "+" => a + b
          case "-" => a - b
          case "/" => a / b
          case "*" => a * b
          case op => throw new IllegalArgumentException(s"$op is unknown operator")
        }
      case _ => sys.error("Fail! Calculation requires at least two operands")
    }
  }

  def evalInput(s: String): Int = {
    evalAll(s.split(" ").toList).runA(Nil).value
  }

  val program: StateT[Eval, List[Int], Int] = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  //  println(program.runA(Nil).value)
  val program2 = evalAll(List("1", "2", "+", "3", "*"))
  //  println(program2.runA(Nil).value)

  val program3 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  //println(program3.runA(Nil).value)
  println(evalInput("1 2 + 3 4 + *"))
}

object CustomMonad extends App {
  import Tree._
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._

  implicit val treeM = new cats.Monad[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(l, r) => Tree.branch(flatMap(l)(f), flatMap(r)(f))
        case Leaf(a) => f(a)
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      f(a) match {
        case Leaf(Left(a1)) => tailRecM(a1)(f)
        case Leaf(Right(v)) => Leaf(v)
        case Branch(lt, rt) => Branch(
          flatMap(lt) {
            case Left(l) => tailRecM(l)(f)
            case Right(l) => pure(l)
          },
          flatMap(rt) {
            case Left(r) => tailRecM(r)(f)
            case Right(r) => pure(r)
          }
        )
      }
    }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }

  val tree: Tree[Int] = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c
}