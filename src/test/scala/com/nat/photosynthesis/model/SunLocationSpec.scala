package com.nat.photosynthesis.model

import org.scalatest.{FreeSpec, Matchers}

class SunLocationSpec extends FreeSpec with Matchers {

  "next" in {
    SunLocation0.next shouldBe SunLocation1
    SunLocation1.next shouldBe SunLocation2
    SunLocation2.next shouldBe SunLocation3
    SunLocation3.next shouldBe SunLocation4
    SunLocation4.next shouldBe SunLocation5
    SunLocation5.next shouldBe SunLocation0
  }

}
