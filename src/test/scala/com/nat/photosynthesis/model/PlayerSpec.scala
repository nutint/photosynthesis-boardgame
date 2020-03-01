package com.nat.photosynthesis.model

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
            List(SmallTree(Green), SmallTree(Green), SmallTree(Green), SmallTree(Green)),
            List(MediumTree(Green))
          ).flatten,
          PlantStore(
            plantType = Green,
            seedStore = List(Priced(1), Priced(1), Priced(2), Priced(2)),
            smallTreeStore = List(Priced(2), Priced(2), Priced(3), Priced(3)),
            mediumTreeStore = List(Priced(3), Priced(3), Priced(4)),
            largeTreeStore = List(Priced(4), Priced(5))
          )
      )
    }
  }
}
