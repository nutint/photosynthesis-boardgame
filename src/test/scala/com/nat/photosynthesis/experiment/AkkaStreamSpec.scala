package com.nat.photosynthesis.experiment

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class AkkaStreamSpec
  extends TestKit(ActorSystem("AkkaStreamSpec"))
    with AnyWordSpecLike
    with Matchers
    with ImplicitSender
    with BeforeAndAfterAll {

  override def afterAll(): Unit =
    TestKit.shutdownActorSystem(system)

  "An echo actor" must {
    "send back message unchanged" in {
      val echo = system.actorOf(TestActors.echoActorProps)
      echo ! "Hello world"
      expectMsg("Hello world")
    }
  }
}
