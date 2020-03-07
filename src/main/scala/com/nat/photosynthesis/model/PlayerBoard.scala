package com.nat.photosynthesis.model

import com.nat.photosynthesis.model.PlantItem.Resource

case class PlayerBoard(
  player: Player,
  tokens: List[Token],
  sun: Int,
  stock: List[PlantItem],
  store: PlantStore
) {
  def withdrawResource(resource: Resource): Either[String, PlayerBoard] = {
    (sun < resource.cost, !stock.contains(resource.seedAble)) match {
      case (true, _) => Left("Not enough sun")
      case (_, true) => Left("Not enough tree/seed in stock")
      case _ =>
        Right(copy(
          sun = sun - resource.cost,
          stock = stock diff List(resource.seedAble)
        ))
    }
  }

  def depositResource(plantItem: PlantItem): Either[String, PlayerBoard] =
    store.putBack(plantItem).map(s => copy(store = s))

  def buy(plantItem: PlantItem): Either[String, PlayerBoard] =
    store
      .getPrice(plantItem)
      .flatMap(price => if(price > sun) Left("Not enough sun") else Right(sun - price))
      .flatMap(remainingSun => store.take(plantItem).map(s => copy(store = s, sun = remainingSun)))
}

object PlayerBoard {
  def apply(player: Player): PlayerBoard = player.initBoard
}
