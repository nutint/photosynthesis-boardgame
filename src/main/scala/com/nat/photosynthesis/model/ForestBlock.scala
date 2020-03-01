package com.nat.photosynthesis.model

case class ForestBlock(
  boardLocation: BoardLocation,
  plantItem: PlantItem
) {
  def calculateScore(sunLocation: SunLocation, forestBlocks: List[ForestBlock]): Int = {
    if(forestBlocks.length == 1 && forestBlocks.contains(this)) plantItem.score
    else 0
  }

//  def calculateScore2(sunLocation0: SunLocation, forestBlocks: List[ForestBlock]): Int = {
//    case SunLocation0 => forestBlocks.filter { forestBlocks =>
//      val BoardLocation(x, y, z) = forestBlocks.boardLocation
//      val BoardLocation(myX, myY, myZ) = boardLocation
//
//      val aheadPosDiff = -1
//      val diffY = myY - y
//      val diffZ = myZ - z
//      if(diffY == diffZ && diffZ > 0) {
//
//      }
//    }
//  }

}
