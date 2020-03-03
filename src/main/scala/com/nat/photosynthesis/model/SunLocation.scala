package com.nat.photosynthesis.model

sealed trait SunLocation {
  def next: SunLocation
}
case object SunLocation0 extends SunLocation {
  override def next: SunLocation = SunLocation1
}
case object SunLocation1 extends SunLocation {
  override def next: SunLocation = SunLocation2
}
case object SunLocation2 extends SunLocation {
  override def next: SunLocation = SunLocation3
}
case object SunLocation3 extends SunLocation {
  override def next: SunLocation = SunLocation4
}
case object SunLocation4 extends SunLocation {
  override def next: SunLocation = SunLocation5
}
case object SunLocation5 extends SunLocation {
  override def next: SunLocation = SunLocation0
}
