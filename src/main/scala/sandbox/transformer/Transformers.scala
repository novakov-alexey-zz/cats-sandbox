package sandbox.transformer

import cats.Monad
import cats.data.EitherT
import cats.instances.future._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.higherKinds

class Transformers {
  // Hypothetical example. This won't actually compile:
  def compose[M1[_] : Monad, M2[_] : Monad] = {

    type Composed[A] = M1[M2[A]]

    new Monad[Composed] {

      def pure[A](a: A): Composed[A] =
        a.pure[M2].pure[M1]

      def flatMap[A, B](fa: Composed[A])
                       (f: A => Composed[B]): Composed[B] = ???

      // Problem! How do we write flatMap
      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }
}

object Transformers extends App {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels: Map[String, Int] = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(i) => EitherT.right(Future.successful[Int](i))
      case None => EitherT.left(Future.successful[String](s"unknown $autobot"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    r1 <- getPowerLevel(ally1)
    r2 <- getPowerLevel(ally2)
  } yield r1 + r2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value
    Await.result(stack, 1.second) match {
      case Left(msg) => s"error: $msg"
      case Right(true) => "can do special move"
      case Right(false) => "can not do special move"
    }
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  // res28: String = Jazz and Bumblebee need a recharge.
  println(tacticalReport("Bumblebee", "Hot Rod"))
  // res29: String = Bumblebee and Hot Rod are ready to roll out!
  println(tacticalReport("Jazz", "Ironhide"))
}
