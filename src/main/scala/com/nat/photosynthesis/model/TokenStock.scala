package com.nat.photosynthesis.model

case class TokenStock(
  tier1: List[TokenTierOne],
  tier2: List[TokenTierTwo],
  tier3: List[TokenTierThree],
  tier4: List[TokenTierFour]
)

case object TokenStock
{
  def apply(): TokenStock = TokenStock(Nil, Nil, Nil, Nil)
}
