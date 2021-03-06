package com.nat.photosynthesis.service.model

case class PlantStore(
  plantType: PlantType,
  seedStore: StoreSpace[Seed],
  smallTreeStore: StoreSpace[SmallTree],
  mediumTreeStore: StoreSpace[MediumTree],
  largeTreeStore: StoreSpace[LargeTree]
) {
  def getPrice(plantItem: Plant): Either[String, Int] =
    plantItem match {
      case _: Seed => seedStore.currentPrice
      case _: SmallTree => smallTreeStore.currentPrice
      case _: MediumTree => mediumTreeStore.currentPrice
      case _: LargeTree => largeTreeStore.currentPrice
      case _ => Left("Invalid Plant Type")
    }

  def take(plantItem: Plant): Either[String, PlantStore] =
    plantItem match {
      case _: Seed => seedStore.take.map(ss => copy(seedStore = ss))
      case _: SmallTree => smallTreeStore.take.map(ss => copy(smallTreeStore = ss))
      case _: MediumTree => mediumTreeStore.take.map(ss => copy(mediumTreeStore = ss))
      case _: LargeTree => largeTreeStore.take.map(ss => copy(largeTreeStore = ss))
      case _ => Left("Invalid Plant Type")
    }

  def putBack(plantItem: Plant):Either[String, PlantStore] =
    plantItem match {
      case _: Seed => seedStore.putBack.map(ss => copy(seedStore = ss))
      case _: SmallTree => smallTreeStore.putBack.map(ss => copy(smallTreeStore = ss))
      case _: MediumTree => mediumTreeStore.putBack.map(ss => copy(mediumTreeStore = ss))
      case _: LargeTree => largeTreeStore.putBack.map(ss => copy(largeTreeStore = ss))
      case _ => Left("Invalid Plant Type")
    }
}

object PlantStore {

  def apply(plantType: PlantType): PlantStore = PlantStore(
    plantType = Green,
    seedStore = StoreSpace[Seed](List(1, 1, 2, 2)),
    smallTreeStore = StoreSpace[SmallTree](List(2, 2, 3, 3)),
    mediumTreeStore = StoreSpace[MediumTree](List(3, 3, 4, 4)),
    largeTreeStore = StoreSpace[LargeTree](List(4, 5))
  )

}
