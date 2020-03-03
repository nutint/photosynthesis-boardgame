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
    PlantStore(
      plantType = plantType,
      seedStore = List(Priced(1), Priced(1), Priced(2), Priced(2)),
      smallTreeStore = List(Priced(2), Priced(2), Priced(3), Priced(3)),
      mediumTreeStore = List(Priced(3), Priced(3), Priced(4)),
      largeTreeStore = List(Priced(4), Priced(5))
    )
  )
}
