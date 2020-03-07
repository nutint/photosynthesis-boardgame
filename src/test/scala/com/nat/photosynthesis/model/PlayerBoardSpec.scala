package com.nat.photosynthesis.model

import com.nat.photosynthesis.model.PlantItem.Resource
import org.scalatest.{FreeSpec, Matchers}

class PlayerBoardSpec extends FreeSpec with Matchers {

  "withdrawResource" - {
    "should return new board if the available resource is enough" in {
      val originalPlayerBoard = Player("John", Green).initBoard
      originalPlayerBoard
        .withdrawResource(Resource(SmallTree(Green), 0)) shouldBe Right(
          originalPlayerBoard.copy(sun = originalPlayerBoard.sun, stock = originalPlayerBoard.stock.diff(List(SmallTree(Green))))
      )
    }
    "should return error if not enough sun" in {
      val originalPlayerBoard = Player("John", Green).initBoard
      originalPlayerBoard
        .withdrawResource(Resource(SmallTree(Green), 2)) shouldBe Left("Not enough sun")
    }
    "should return error if not enough tree" in {
      val originalPlayerBoard = Player("John", Green).initBoard.copy(stock = Nil)
      originalPlayerBoard
        .withdrawResource(Resource(SmallTree(Green), 0)) shouldBe Left("Not enough tree/seed in stock")
    }
  }

  "putBack" - {
    "should ok if there is available space" in {
      Player("John", Green).initBoard
        .withdrawResource(Resource(SmallTree(Green), 0))
        .map(_.depositResource(SmallTree(Green))) shouldBe Player("John", Green).initBoard
    }
  }

  "buy" - {
    "should fail if there is not enough sun" in {
      Player("John", Green).initBoard
        .buy(Seed(Green)) shouldBe Left("Not enough sun")
    }

    "should fail if there is no remaining item in the store" in {
      val johnBoard = Player("John", Green).initBoard.copy(sun = 9)
      val emptySeedStore = johnBoard.store
        .take(Seed(Green))
        .flatMap(_.take(Seed(Green)))
        .flatMap(_.take(Seed(Green)))
        .flatMap(_.take(Seed(Green))).toOption.get
      johnBoard.copy(store = emptySeedStore)
        .buy(Seed(Green)) shouldBe Left("Not available")
    }

    "should success if there is enough sun" in {
      val johnBoard = Player("John", Green).initBoard.copy(sun = 9)
      val johnStore = johnBoard.store
      val price = johnStore.getPrice(Seed(Green)).toOption.get
      johnBoard
        .buy(Seed(Green)) shouldBe Right(johnBoard.copy(
          store = johnStore.take(Seed(Green)).toOption.get,
          sun = johnBoard.sun - price)
        )
    }
  }
}
