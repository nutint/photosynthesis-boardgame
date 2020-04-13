package com.nat.photosynthesis.service.model

/**
 * According to the our use cases, to be able to categorize
 */
sealed trait Displacement
sealed trait SameLineDisplacement extends Displacement
case class Front(d: Int) extends SameLineDisplacement
case class Rear(d: Int) extends SameLineDisplacement
case object Same extends SameLineDisplacement
case object DifferentLine extends Displacement
