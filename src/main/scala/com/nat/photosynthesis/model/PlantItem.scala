package com.nat.photosynthesis.model

sealed trait PlantItem {
  def plantType: PlantType
  def score: Int
  def height: Int
}

sealed trait SeedAble extends PlantItem {
  def seed: CooledDownPlantItem
}
sealed trait GrowAble extends PlantItem {
  def grow: CooledDownPlantItem
}
sealed trait CooledDownPlantItem extends PlantItem {
  def reset: ReadyPlantItem
}
sealed trait ReadyPlantItem extends PlantItem {
}

case class Seed(plantType: PlantType) extends ReadyPlantItem with GrowAble {
  override def score: Int = 0
  override def height: Int = 0
  override def grow: CooledDownPlantItem = CooledDownSmallTree(plantType)
}

case class CooledDownSmallTree(plantType: PlantType) extends CooledDownPlantItem {
  override def score: Int = 1
  override def height: Int = 1
  override def reset: ReadyPlantItem = SmallTree(plantType)
}
case class SmallTree(plantType: PlantType) extends ReadyPlantItem with GrowAble with SeedAble {
  override def score: Int = 1
  override def height: Int = 1
  override def grow: CooledDownPlantItem = CooledDownMediumTree(plantType)
  override def seed: CooledDownPlantItem = CooledDownSmallTree(plantType)
}

case class CooledDownMediumTree(plantType: PlantType) extends CooledDownPlantItem {
  override def score: Int = 2
  override def height: Int = 2
  override def reset: ReadyPlantItem = MediumTree(plantType)
}
case class MediumTree(plantType: PlantType) extends ReadyPlantItem with GrowAble with SeedAble {
  override def score: Int = 2
  override def height: Int = 2
  override def grow: CooledDownPlantItem = CooledDownLargeTree(plantType)
  override def seed: CooledDownPlantItem = CooledDownMediumTree(plantType)
}

case class CooledDownLargeTree(plantType: PlantType) extends CooledDownPlantItem {
  override def score: Int = 3
  override def height: Int = 3
  override def reset: ReadyPlantItem = LargeTree(plantType)
}
case class LargeTree(plantType: PlantType) extends ReadyPlantItem with SeedAble {
  override def score: Int = 3
  override def height: Int = 3
  override def seed: CooledDownPlantItem = CooledDownLargeTree(plantType)
}
