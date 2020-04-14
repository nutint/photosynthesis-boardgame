package com.nat.photosynthesis.service.model

import com.nat.photosynthesis.service.model.Plant.Resource

case class PlayerBoard(
  player: Player,
  tokens: List[ScoringToken],
  lightPoints: Int,
  stock: List[Plant],
  store: PlantStore
) {
  def withdrawResource(resource: Resource): Either[String, PlayerBoard] = {
    (lightPoints < resource.cost, !stock.contains(resource.seedAble)) match {
      case (true, _) => Left("Not enough sun")
      case (_, true) => Left("Not enough tree/seed in stock")
      case _ =>
        Right(copy(
          lightPoints = lightPoints - resource.cost,
          stock = stock diff List(resource.seedAble)
        ))
    }
  }

  def putBack(plantItem: Plant): Either[String, PlayerBoard] =
    store.putBack(plantItem).map(s => copy(store = s))

  def buy(plantItem: Plant): Either[String, PlayerBoard] =
    store
      .getPrice(plantItem)
      .flatMap(price => if(price > lightPoints) Left("Not enough sun") else Right(lightPoints - price))
      .flatMap(remainingLightPoints => store.take(plantItem).map(s => copy(store = s, lightPoints = remainingLightPoints, stock = stock :+ plantItem)))
}

object PlayerBoard {
  def apply(player: Player): PlayerBoard = player.initBoard
}
