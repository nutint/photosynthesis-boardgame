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
import scala.util.{Success, Try}

class AkkaStreamSpec
  extends AnyWordSpecLike
    with BeforeAndAfterAll
    with Matchers {

  val testKit = ActorTestKit()
  import testKit._

  override def afterAll(): Unit = shutdownTestKit()

  "Echo" must {
    import AkkaStreamSpec._
    "pong if pinger ping" in {
      val pinger = spawn(Echo(), "ping")
      val probe = createTestProbe[Echo.Pong]()
      pinger ! Echo.Ping("Hello", probe.ref)
      probe.expectMessage(Echo.Pong("Hello"))
    }

    "be able to stop actors under test" in {
      val probe = createTestProbe[Echo.Pong]()

      val pinger1 = spawn(Echo(), "pinger")
      pinger1 ! Echo.Ping("Hello", probe.ref)
      probe.expectMessage(Echo.Pong("Hello"))
      stop(pinger1)

      val pinger2 = spawn(Echo(), "pinger")
      pinger2 ! Echo.Ping("Hello", probe.ref)
      probe.expectMessage(Echo.Pong("Hello"))
      stop(pinger2, 10 seconds)
    }
  }

  "MockedBehavior" must {
    import AkkaStreamSpec.MockedBehavior._
    "support observing mocked behavior" in {
      val mockedBehavior = Behaviors.receiveMessage[Message] { msg =>
        msg.replyTo ! Success(msg.i)
        Behaviors.same
      }
      val probe = createTestProbe[Message]()
      val mockedPublisher = spawn(Behaviors.monitor(probe.ref, mockedBehavior))

      val producer = new Producer(mockedPublisher)
      val messages = 3
      producer.produce(messages)

      for ( i <- 0 until messages) {
        val msg = probe.expectMessageType[Message]
        msg.i shouldBe i
      }
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
