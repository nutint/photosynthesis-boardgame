package com.nat.photosynthesis.model

case class PlantStore(
  plantType: PlantType,
  seedStore: StoreSpace[Seed],
  smallTreeStore: StoreSpace[SmallTree],
  mediumTreeStore: StoreSpace[MediumTree],
  largeTreeStore: StoreSpace[LargeTree]
)

object PlantStore {

  def apply(plantType: PlantType): PlantStore = PlantStore(
    plantType = Green,
    seedStore = StoreSpace[Seed](List(1, 1, 2, 2)),
    smallTreeStore = StoreSpace[SmallTree](List(2, 2, 3, 3)),
    mediumTreeStore = StoreSpace[MediumTree](List(3, 3, 4, 4)),
    largeTreeStore = StoreSpace[LargeTree](List(4, 5))
  )

}
