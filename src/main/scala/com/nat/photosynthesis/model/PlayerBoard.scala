package com.nat.photosynthesis.model

case class PlayerBoard(
  player: Player,
  tokens: List[Token],
  sun: Int,
  stock: List[PlantItem],
  store: PlantStore
)
