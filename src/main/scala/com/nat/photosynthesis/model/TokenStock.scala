package com.nat.photosynthesis.model

case class TokenStock(
  tier1: List[ScoringTokenTierOne],
  tier2: List[ScoringTokenTierTwo],
  tier3: List[ScoringTokenTierThree],
  tier4: List[ScoringTokenTierFour]
)

case object TokenStock
{
  def apply(): TokenStock = TokenStock(Nil, Nil, Nil, Nil)
}
