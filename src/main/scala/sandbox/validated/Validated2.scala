package sandbox.validated

import cats.data.Validated
import cats.syntax.either._
import cats.instances.list._
import cats.syntax.apply._   // for mapN

case class User(name: String, age: Int)

object Validated2 extends App {
  def readName(data: Map[String, String]): Either[List[String], String] = {
    getValue(data)("name").flatMap(nonBlank)
  }

  def readAge(data: Map[String, String]): Either[List[String], Int] = {
    getValue(data)("age").flatMap(nonBlank).flatMap(parseInt).flatMap(nonNegative)
  }

  def parseInt(s: String): Either[List[String], Int] =
    Either.catchOnly[NumberFormatException](s.toInt)
      .leftMap(_ => List("must be an non-negative integer"))

  def nonBlank(s: String): Either[List[String], String] = {
    Right[List[String], String](s).ensure(List(s"cannot be blank"))(_.nonEmpty)
  }

  def nonNegative(n: Int): Either[List[String], Int] = {
    Right[List[String], Int](n).ensure(List(s"cannot be blank"))(_ >= 0)
  }

  def getValue(data: Map[String, String])(name: String): Either[List[String], String] = {
    data.get(name)
      .toRight(List(s"$name is not specified"))
  }

  def readUser(data: Map[String, String]): Validated[List[String], User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN { case (s, i) => User(s, i)}
}
