package com.nat.photosynthesis.service.model

import org.scalatest.{FreeSpec, Matchers}

class PlayerSpec extends FreeSpec with Matchers {

  "initBoard" - {
    "should work correctly" in {
      val john = Player("John", Green)
      john
        .initBoard shouldBe PlayerBoard(
          player = john,
          Nil,
          0,
          List(
            List(Seed(Green), Seed(Green)),
            List(SmallTree(Green), SmallTree(Green)),
            List(MediumTree(Green))
          ).flatten,
          PlantStore(plantType = Green)
      )
    }
  }
}
