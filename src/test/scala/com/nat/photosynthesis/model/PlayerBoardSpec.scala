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
}
