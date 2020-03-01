package com.nat.photosynthesis.model

sealed trait PlantItem {
  def plantType: PlantType
  def score: Int
}
case class Seed(plantType: PlantType) extends PlantItem {
  override def score: Int = 0
}
case class SmallTree(plantType: PlantType) extends PlantItem {
  override def score: Int = 1
}
case class MediumTree(plantType: PlantType) extends PlantItem {
  override def score: Int = 2
}
case class LargeTree(plantType: PlantType) extends PlantItem {
  override def score: Int = 3
}
