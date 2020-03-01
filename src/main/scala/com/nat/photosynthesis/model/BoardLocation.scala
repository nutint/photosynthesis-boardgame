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
    val BoardLocation(rhsX, rhsY, rhsZ) = rsh
    (sunLocation match {
      case SunLocation0 => rhsY > y
      case SunLocation3 => rhsY < y
      case SunLocation1 => rhsX > x
      case SunLocation4 => rhsX < x
      case SunLocation2 => rhsZ > z
      case SunLocation5 => rhsZ < z
    }) && isSameLine(rsh, sunLocation)
  }

  def isSameLine(rhs: BoardLocation, sunLocation: SunLocation): Boolean = {
    val BoardLocation(rhsX, rhsY, rhsZ) = rhs
    sunLocation match {
      case SunLocation0 | SunLocation3 => rhsX == x
      case SunLocation1 | SunLocation4 => rhsY == y
      case SunLocation2 | SunLocation5 => rhsZ == z
    }
  }
}
