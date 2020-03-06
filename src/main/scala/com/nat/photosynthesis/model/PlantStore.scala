package com.nat.photosynthesis.model

case class PlantStore(
  plantType: PlantType,
  seedStore: List[Priced[Seed]],
  smallTreeStore: List[Priced[SmallTree]],
  mediumTreeStore: List[Priced[MediumTree]],
  largeTreeStore: List[Priced[LargeTree]],
)

object PlantStore {

  def apply(plantType: PlantType): PlantStore = PlantStore(
    plantType = plantType,
    seedStore = List(1, 1, 2, 2).map(Priced[Seed]),
    smallTreeStore = List(2, 2, 3, 3).map(Priced[SmallTree]),
    mediumTreeStore = List(3, 3, 4, 4).map(Priced[MediumTree]),
    largeTreeStore = List(4, 5).map(Priced[LargeTree])
  )

}
