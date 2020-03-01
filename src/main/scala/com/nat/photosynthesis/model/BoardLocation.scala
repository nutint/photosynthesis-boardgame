package com.nat.photosynthesis.model

case class BoardLocation(x: Int, y: Int, z: Int) {
  def toForestBlock(plantItem: PlantItem) = ForestBlock(this, plantItem)
  def isEdgeLocation: Boolean = getBoardLocationTier == BoardLocationTier1
  def getBoardLocationTier: BoardLocationTier = {
    List(x, y, z).foldLeft(0) { (acc, elem) => acc + (elem * elem)} match {
      case 18 | 14 => BoardLocationTier1
      case 8 | 6 => BoardLocationTier2
      case 2 => BoardLocationTier3
      case 0 => BoardLocationTier4
      case _ => assert(false); BoardLocationTier1
    }
  }
  def isBehind(rsh: BoardLocation, sunLocation: SunLocation): Boolean = {
    sunLocation match {
      case SunLocation0 =>
        val BoardLocation(_, rhsY, _) = rsh
        rhsY > y && isSameLine(rsh, sunLocation)
      case SunLocation3 =>
        val BoardLocation(_, rhsY, _) = rsh
        rhsY < y && isSameLine(rsh, sunLocation)
      case _ => false
    }
  }

  def isSameLine(rhs: BoardLocation, sunLocation: SunLocation): Boolean = {
    sunLocation match {
      case SunLocation0 =>
        val BoardLocation(_, myY, myZ) = this
        val BoardLocation(_, rhsY, rhsZ) = rhs
        rhsY - myY == rhsZ - myZ
      case SunLocation3 => isSameLine(rhs, SunLocation0)
      case _ => false
    }
  }
}
