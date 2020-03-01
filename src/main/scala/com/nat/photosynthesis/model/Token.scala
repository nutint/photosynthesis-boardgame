package com.nat.photosynthesis.model

sealed trait Token {
  def score: Int
}
case class TokenTierOne(score: Int) extends Token
case class TokenTierTwo(score: Int) extends Token
case class TokenTierThree(score: Int) extends Token
case class TokenTierFour(score: Int) extends Token
