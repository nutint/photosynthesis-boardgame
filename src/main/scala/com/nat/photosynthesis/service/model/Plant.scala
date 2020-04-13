package com.nat.photosynthesis.service.model

import Plant._

sealed trait Plant {
  def plantType: PlantType
  def score: Int
  def height: Int
}

sealed trait SeedAble extends Plant {
  def seed: CoolingDownPlant
  def seedCost: Resource = Resource(Seed(plantType), 1)
}
sealed trait GrowAble extends Plant {
  def grow: CoolingDownPlant
  def growCost: Int
  def growResource: Resource
}
sealed trait CoolingDownPlant extends Plant {
  def reset: ReadyPlant
}
sealed trait ReadyPlant extends Plant {
}

case class Seed(plantType: PlantType) extends ReadyPlant with GrowAble {
  override def score: Int = 0
  override def height: Int = 0
  override def grow: CoolingDownPlant = CoolingDownSmallTree(plantType)
  override def growCost: Int = 1
  override def growResource: Resource = Resource(SmallTree(plantType), 1)
}

case class CoolingDownSmallTree(plantType: PlantType) extends CoolingDownPlant {
  override def score: Int = 1
  override def height: Int = 1
  override def reset: ReadyPlant = SmallTree(plantType)
}
case class SmallTree(plantType: PlantType) extends ReadyPlant with GrowAble with SeedAble {
  override def score: Int = 1
  override def height: Int = 1
  override def grow: CoolingDownPlant = CoolingDownMediumTree(plantType)
  override def growCost: Int = 2
  override def seed: CoolingDownPlant = CoolingDownSmallTree(plantType)
  override def growResource: Resource = Resource(MediumTree(plantType), 2)
}

case class CoolingDownMediumTree(plantType: PlantType) extends CoolingDownPlant {
  override def score: Int = 2
  override def height: Int = 2
  override def reset: ReadyPlant = MediumTree(plantType)
}
case class MediumTree(plantType: PlantType) extends ReadyPlant with GrowAble with SeedAble {
  override def score: Int = 2
  override def height: Int = 2
  override def grow: CoolingDownPlant = CoolingDownLargeTree(plantType)
  override def growCost: Int = 3
  override def seed: CoolingDownPlant = CoolingDownMediumTree(plantType)
  override def growResource: Resource = Resource(LargeTree(plantType), 3)
}

case class CoolingDownLargeTree(plantType: PlantType) extends CoolingDownPlant {
  override def score: Int = 3
  override def height: Int = 3
  override def reset: ReadyPlant = LargeTree(plantType)
}
case class LargeTree(plantType: PlantType) extends ReadyPlant with SeedAble {
  override def score: Int = 3
  override def height: Int = 3
  override def seed: CoolingDownPlant = CoolingDownLargeTree(plantType)
  def shopCost: Int = 4
}

object Plant {
  case class Resource(seedAble: Plant, cost: Int)
}
