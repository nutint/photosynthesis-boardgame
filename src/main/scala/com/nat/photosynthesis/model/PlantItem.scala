package com.nat.photosynthesis.model

sealed trait PlantItem
case class Seed(plantType: PlantType) extends PlantItem
case class SmallTree(plantType: PlantType) extends PlantItem
case class MediumTree(plantType: PlantType) extends PlantItem
case class LargeTree(plantType: PlantType) extends PlantItem
