package sandbox.cases.testingasync

import cats.{Applicative, Id}
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.concurrent.Future
import scala.language.higherKinds

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class RealUptimeService[F[_] : Applicative](client: UptimeClient[Future]) {
  def getTotalUptime(hostnames: List[String]): Future[Int] = {
    hostnames.traverse(client.getUptime).map(_.sum)
  }
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(hostname: String): Int = {
    hosts.getOrElse(hostname, 0)
  }
}

class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {

  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

object Tests extends App {

  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()
}