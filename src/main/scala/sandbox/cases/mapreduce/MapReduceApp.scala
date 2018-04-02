package sandbox.cases.mapreduce

import cats.Monoid
import cats.instances.int._
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object MapReduceApp extends App {
  val cores = 4

  def foldMap[A, B: Monoid](v: Vector[A])(f: A => B): B = {
    v.foldLeft(Monoid[B].empty)(_ |+| f(_))
  }

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val bs = values.grouped(values.length / cores).map(b => Future(foldMap(b)(func)))
    Future.sequence(bs).map { it =>
      foldMap(it.toVector)(identity)
    }
  }

  //  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  //  println(foldMap(Vector(1, 2, 3))(identity))
  //  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

  val result = parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))
}
