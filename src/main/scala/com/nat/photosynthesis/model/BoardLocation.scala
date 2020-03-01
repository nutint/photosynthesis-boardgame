package com.nat.photosynthesis.model

case class BoardLocation(x: Int, y: Int, z: Int) {
  def toForestBlock(plantItem: PlantItem) = ForestBlock(this, plantItem)
}
