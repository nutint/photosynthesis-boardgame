package com.nat.photosynthesis.model

sealed trait Distance
sealed trait SameLineDistance extends Distance
case class Front(d: Int) extends SameLineDistance
case class Rear(d: Int) extends SameLineDistance
case object Same extends SameLineDistance
case object DifferentLine extends Distance
