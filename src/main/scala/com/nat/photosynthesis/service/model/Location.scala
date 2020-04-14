package com.nat.photosynthesis.service.model

case class Location(x: Int, y: Int, z: Int) {
  def toForestBlock(plantItem: Plant): Block = Block(this, plantItem)
  def isExternalEdge: Boolean = getBoardLocationTier == LocationTier1$
  def getBoardLocationTier: LocationTier = {
    List(x, y, z).foldLeft(0) { (acc, elem) => acc + (elem * elem)} match {
      case 18 | 14 => LocationTier1$
      case 8 | 6 => LocationTier2$
      case 2 => LocationTier3$
      case 0 => LocationTier4$
    }
  }

  def isBehind(rsh: Location, sunLocation: SunLocation): Boolean = {
    val Location(rhsX, rhsY, rhsZ) = rsh
    (sunLocation match {
      case SunLocation0 => rhsY > y
      case SunLocation3 => rhsY < y
      case SunLocation1 => rhsX > x
      case SunLocation4 => rhsX < x
      case SunLocation2 => rhsZ > z
      case SunLocation5 => rhsZ < z
    }) && isSameLine(rsh, sunLocation)
  }

  def isSameLine(rhs: Location, sunLocation: SunLocation): Boolean = {
    val Location(rhsX, rhsY, rhsZ) = rhs
    sunLocation match {
      case SunLocation0 | SunLocation3 => rhsX == x
      case SunLocation1 | SunLocation4 => rhsY == y
      case SunLocation2 | SunLocation5 => rhsZ == z
    }
  }

  def inRadius(rhs: Location, range: Int): Boolean =
    List(SunLocation0, SunLocation1, SunLocation2)
      .exists(sl => getDistance(rhs, sl) match {
        case Front(r) if r <= range => true
        case Rear(r) if r <= range => true
        case _ => false
      })

  def getDistance(boardLocation: Location, sunLocation: SunLocation): Displacement = {
    if(boardLocation == this) Same
    else if(!isSameLine(boardLocation, sunLocation)) DifferentLine
    else {
      val Location(rhsX, rhsY, rhsZ) = boardLocation
      sunLocation match {
        case SunLocation0 => toDistance(y - rhsY)
        case SunLocation3 => toDistance(rhsY - y)
        case SunLocation1 => toDistance(z - rhsZ)
        case SunLocation4 => toDistance(rhsZ - z)
        case SunLocation2 => toDistance(rhsX - x)
        case SunLocation5 => toDistance(x - rhsX)
      }
    }
  }

  private def toDistance(distance: Int): SameLineDisplacement =
    distance match {
      case d if d > 0 => Front(d)
      case d if d < 0 => Rear(-d)
      case _ => Same
    }
}

object Location {
  def apply(tup: (Int, Int, Int)): Location = Location(tup._1, tup._2, tup._3)
}
