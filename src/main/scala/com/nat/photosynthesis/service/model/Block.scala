package com.nat.photosynthesis.service.model

case class Block(
  location: Location,
  plantItem: Plant
) {
  def calculateScore(sunLocation: SunLocation, blocks: List[Block]): Int = {
    if (
      blocks
        .filter(_.location.isSameLine(this.location, sunLocation))
        .exists(block => isUnderShadowOf(block, sunLocation) == Right(true))
    ) 0 else plantItem.score
  }

  def isUnderShadowOf(rhs: Block, sunLocation: SunLocation): Either[String, Boolean] = {
    if(location == rhs.location) Left("Same Location")
    else {
      val isSameLine = location.isSameLine(rhs.location, sunLocation)
      if(!isSameLine) Right(false)
      else {
        val distance = location.getDistance(rhs.location, sunLocation)
        distance match {
          case Rear(x) =>
            val shadowCovered = x <= rhs.plantItem.height
            val shorterOrEqual = plantItem.height <= rhs.plantItem.height
            Right(shadowCovered && shorterOrEqual)
          case _ => Right(false)
        }
      }
    }
  }

  def isOwnedBy(player: Player): Boolean = plantItem.plantType == player.plantType
}

object Block {
  def apply(x: Int, y: Int, z: Int, plantItem: Plant): Block = Block(Location(x, y, z), plantItem)
}
