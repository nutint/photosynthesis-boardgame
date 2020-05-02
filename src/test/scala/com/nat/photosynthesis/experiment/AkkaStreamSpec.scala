package com.nat.photosynthesis.experiment

import scala.concurrent.duration._
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.testkit.typed.scaladsl.ManualTime
import akka.actor.testkit.typed.scaladsl.TestProbe
import akka.actor.typed.scaladsl.Behaviors
import org.scalatest.wordspec.AnyWordSpecLike

class AkkaStreamSpec
  extends ScalaTestWithActorTestKit(ManualTime.config) with AnyWordSpecLike {

  val manualTime: ManualTime = ManualTime()

  "A timer" must {
    "schedule non-repeated ticks" in {
      case object Tick
      case object Tock

      val probe = TestProbe[Tock.type]()
      val behavior = Behaviors.withTimers[Tick.type] { timer =>
        timer.startSingleTimer("T", Tick, 10.millis)
        Behaviors.receiveMessage { _ =>
          probe.ref ! Tock
          Behaviors.same
        }
      }

      spawn(behavior)

      manualTime.expectNoMessageFor(9.millis, probe)

      manualTime.timePasses(2.millis)
      probe.expectMessage(Tock)

      manualTime.expectNoMessageFor(10.seconds, probe)
    }
  }
}
