package com.nat.photosynthesis.service.model

case class ScoringTokenStacks(
  tier1: List[ScoringTokenTierOne],
  tier2: List[ScoringTokenTierTwo],
  tier3: List[ScoringTokenTierThree],
  tier4: List[ScoringTokenTierFour]
)

case object ScoringTokenStacks
{
  def apply(): ScoringTokenStacks = ScoringTokenStacks(Nil, Nil, Nil, Nil)
}
