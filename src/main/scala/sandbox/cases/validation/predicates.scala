package sandbox.cases.validation

import cats.data.Validated.{Invalid, Valid}
import cats.Semigroup
import cats.data.Validated
import cats.syntax.semigroup._
import cats.syntax.apply._
import sandbox.cases.validation.Predicate._     // for mapN
import cats.data.{Kleisli, NonEmptyList, Validated}

sealed trait Predicate[E, A] {
  type Errors = NonEmptyList[String]
  type Result[A1] = Either[Errors, A1]
  type Check[A1, B] = Kleisli[Result, A1, B]

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) =>
      func(a)
    case And(left, right) =>
      (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right) =>
      left(a) match {
        case Valid(a1)   => Valid(a)
        case Invalid(e1) =>
          right(a) match {
            case Valid(a2)   => Valid(a)
            case Invalid(e2) => Invalid(e1 |+| e2)
          } }
  }

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def run()(implicit s: Semigroup[E]): A => Either[E, A] = a => this(a).toEither

  def check[B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  def checkPred(pred: Predicate[Errors, A]): Check[A, A] = Kleisli[Result, A, A](pred.run())

}

object Predicate {

  final case class Pure[E, A](pred: Predicate[E, A]) extends Predicate[E, A]

  final case class And[E, A](
                              left: Predicate[E, A],
                              right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](
                             left: Predicate[E, A],
                             right: Predicate[E, A]) extends Predicate[E, A]
}