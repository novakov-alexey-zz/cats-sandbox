package sandbox.cases.validation

import cats.instances.list._
import cats.syntax.either._
import cats.Semigroup
import cats.data.Validated
import cats.syntax.semigroup._ // for |+|

sealed trait Check[E, A, B] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] =
    Map(this, func)

  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap(this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = {
    AndThen(this, that)
  }
}

final case class Map[E, A, B, C](
                                  check: Check[E, A, B],
                                  func: B => C) extends Check[E, A, C] {
  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(in).map(func)
}

final case class FlatMap[E, A, B, C](
                                      check: Check[E, A, B],
                                      func: B => Check[E, A, C]) extends Check[E, A, C] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(a).withEither(_.flatMap(b => func(b)(a).toEither))
}

final case class AndThen[E, A, B, C](
                                      check1: Check[E, A, B],
                                      check2: Check[E, B, C]) extends Check[E, A, C] {
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check1(a).withEither(_.flatMap(b => check2(b).toEither))
}


final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] =
    func(a)

  def and(that: CheckF[E, A])
         (implicit s: Semigroup[E]): CheckF[E, A] =
    CheckF { a =>
      (this (a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(a)) => e.asLeft
        case (Right(a), Left(e)) => e.asLeft
        case (Right(a1), Right(a2)) => a.asRight

      }
    }
}

object DataValidationApp extends App {
  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if(v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if(v < -2) v.asRight
      else List("Must be < -2").asLeft
    }
  val check: CheckF[List[String], Int] =
    a and b

  println(check(5))
  println(check(0))
}
