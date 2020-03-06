package com.nat.photosynthesis.model

case class Player(
  name: String,
  plantType: PlantType
) {
  def initBoard: PlayerBoard = PlayerBoard(
    player = copy(),
    Nil,
    0,
    List(
      List(Seed(plantType), Seed(plantType)),
      List(SmallTree(plantType), SmallTree(plantType)),
      List(MediumTree(plantType))
    ).flatten,
    PlantStore(plantType = plantType)
  )
}
