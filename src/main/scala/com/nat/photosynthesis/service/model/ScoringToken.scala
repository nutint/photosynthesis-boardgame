package com.nat.photosynthesis.service.model

sealed trait ScoringToken {
  def score: Int
}
case class ScoringTokenTierOne(score: Int) extends ScoringToken
case class ScoringTokenTierTwo(score: Int) extends ScoringToken
case class ScoringTokenTierThree(score: Int) extends ScoringToken
case class ScoringTokenTierFour(score: Int) extends ScoringToken
