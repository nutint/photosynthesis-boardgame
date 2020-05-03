package com.nat.photosynthesis.experiment

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Scheduler}
import akka.util.Timeout
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Try

class AkkaStreamSpec
  extends AnyWordSpecLike
    with BeforeAndAfterAll
    with Matchers {

  val testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "Echo" must {
    import AkkaStreamSpec._
    "pong if pinger ping" in {
      val pinger = testKit.spawn(Echo(), "ping")
      val probe = testKit.createTestProbe[Echo.Pong]()
      pinger ! Echo.Ping("Hello", probe.ref)
      probe.expectMessage(Echo.Pong("Hello"))
    }

    "be able to stop actors under test" in {
      val probe = testKit.createTestProbe[Echo.Pong]()

      val pinger1 = testKit.spawn(Echo(), "pinger")
      pinger1 ! Echo.Ping("Hello", probe.ref)
      probe.expectMessage(Echo.Pong("Hello"))
      testKit.stop(pinger1)

      val pinger2 = testKit.spawn(Echo(), "pinger")
      pinger2 ! Echo.Ping("Hello", probe.ref)
      probe.expectMessage(Echo.Pong("Hello"))
      testKit.stop(pinger2, 10 seconds)
    }
  }
}

object AkkaStreamSpec {
  object Echo {
    case class Ping(message: String, response: ActorRef[Pong])
    case class Pong(message: String)

    def apply(): Behavior[Ping] = Behaviors.receiveMessage {
      case Ping(msg, replyTo) =>
        replyTo ! Pong(msg)
        Behaviors.same
    }
  }

  object MockedBehavior {
    case class Message(i: Int, replyTo: ActorRef[Try[Int]])

    class Producer(publisher: ActorRef[Message])(implicit scheduler: Scheduler) {
      def produce(messages: Int)(implicit timeout: Timeout): Unit =
        (0 until messages).foreach(publish)
      def publish(i: Int)(implicit timeout: Timeout): Future[Try[Int]] =
        publisher.ask(ref => Message(i, ref))
    }
  }
}
