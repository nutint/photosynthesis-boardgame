package com.nat.photosynthesis.model

case class ForestBlock(
  boardLocation: BoardLocation,
  plantItem: PlantItem
) {
  def calculateScore(sunLocation: SunLocation, forestBlocks: List[ForestBlock]): Int = {
    if(forestBlocks.length == 1 && forestBlocks.contains(this)) plantItem.score
    else 0
  }

  def calculateScoreV2(sunLocation: SunLocation, forestBlocks: List[ForestBlock]): Int = {
    0
  }

  def isUnderShadowOf(rhs: ForestBlock, sunLocation: SunLocation): Either[String, Boolean] = {
    if(boardLocation == rhs.boardLocation) Left("Same Location")
    else {
      val isSameLine = boardLocation.isSameLine(rhs.boardLocation, sunLocation)
      if(!isSameLine) Right(false)
      else {
        val distance = boardLocation.getDistance(rhs.boardLocation, sunLocation)
        distance match {
          case Rear(x) => {
            val shadowCovered = x <= rhs.plantItem.height
            val shorterOrEqual = plantItem.height <= rhs.plantItem.height
            Right(shadowCovered && shorterOrEqual)
          }
          case _ => Right(false)
        }
      }
    }
  }

}

object ForestBlock {
  def apply(x: Int, y: Int, z: Int, plantItem: PlantItem): ForestBlock = ForestBlock(BoardLocation(x, y, z), plantItem)
}
