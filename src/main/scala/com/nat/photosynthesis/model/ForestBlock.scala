package com.nat.photosynthesis.model

case class ForestBlock(
  boardLocation: BoardLocation,
  plantItem: PlantItem
) {
  def calculateScore(sunLocation: SunLocation, forestBlocks: List[ForestBlock]): Int = {
    if(forestBlocks.length == 1 && forestBlocks.contains(this)) plantItem.score
    else 0
  }
}
