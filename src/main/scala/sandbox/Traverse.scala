package sandbox

import cats.Applicative
import cats.data.Validated
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.instances.option._
import cats.data.Validated
import cats.instances.list._ // for Monoid

import scala.language.higherKinds // for Applicative


object Traverse extends App {

  def listTraverse[F[_] : Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accum, item) => (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

//  println(listSequence(List(Vector(1, 2), Vector(3, 4))))
//  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))


  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

//  println(process(List(2, 4, 6)))
//  println(process(List(1, 2, 3)))

  type ErrorsOr[A] = Validated[List[String], A]

  def process2(inputs: List[Int]): ErrorsOr[List[Int]] = listTraverse(inputs) { n =>
    if(n % 2 == 0) {
      Validated.valid(n)
    } else {
      Validated.invalid(List(s"$n is not even"))
    }
  }

  println(process2(List(2, 4, 6)))
  println(process2(List(1, 2, 3)))
}
