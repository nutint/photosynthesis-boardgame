package com.nat.photosynthesis.service.model

case class Player(
  name: String,
  plantType: PlantType
) {
  def initBoard: PlayerBoard = PlayerBoard(
    player = copy(),
    tokens = Nil,
    sun = 0,
    stock = List(
      List(Seed(plantType), Seed(plantType)),
      List(SmallTree(plantType), SmallTree(plantType)),
      List(MediumTree(plantType))
    ).flatten,
    store = PlantStore(plantType = plantType)
  )
}
