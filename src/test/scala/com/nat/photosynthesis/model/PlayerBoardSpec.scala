package com.nat.photosynthesis.model

import com.nat.photosynthesis.model.Plant.Resource
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
    "should be full if there is no available space" in {
      Player("John", Green).initBoard
        .putBack(SmallTree(Green)) shouldBe Left("Already full")
    }

    "should be able to put back if there is available space" in {
      val availableSpacePlantStore = PlantStore(Green).copy(smallTreeStore = StoreSpace(1, 2, 3, 4).copy(currItem = 1))
      val putBackSpacePlantStore = availableSpacePlantStore.putBack(SmallTree(Green))
      val playerBoard = Player("John", Green).initBoard.copy(store = availableSpacePlantStore)

      playerBoard
        .putBack(SmallTree(Green)) shouldBe Right(playerBoard.copy(store = putBackSpacePlantStore.toOption.get))
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
        .buy(Seed(Green)) shouldBe Left("Out of stock")
    }

    "should deducted sun, deduct deduct store space, and add to the stock if there is enough sun" in {
      val johnBoard = Player("John", Green).initBoard.copy(sun = 9)
      val johnStore = johnBoard.store
      val price = johnStore.getPrice(Seed(Green)).toOption.get
      johnBoard
        .buy(Seed(Green)) shouldBe Right(johnBoard.copy(
          store = johnStore.take(Seed(Green)).toOption.get,
          sun = johnBoard.sun - price,
          stock = johnBoard.stock :+ Seed(Green)
        ))
    }
  }
}
