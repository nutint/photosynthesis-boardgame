package com.nat.photosynthesis.model

sealed trait PlantItem {
  def plantType: PlantType
  def score: Int
  def height: Int
}
case class Seed(plantType: PlantType) extends PlantItem {
  override def score: Int = 0
  override def height: Int = 0
}
case class SmallTree(plantType: PlantType) extends PlantItem {
  override def score: Int = 1
  override def height: Int = 1
}
case class MediumTree(plantType: PlantType) extends PlantItem {
  override def score: Int = 2
  override def height: Int = 2
}
case class LargeTree(plantType: PlantType) extends PlantItem {
  override def score: Int = 3
  override def height: Int = 3
}
