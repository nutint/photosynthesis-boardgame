package com.nat.photosynthesis.model

case class PlayerBoard(
  player: Player,
  tokens: List[Token],
  sun: Int,
  stock: List[PlantItem],
  store: PlantStore
)

object PlayerBoard {
  def apply(player: Player): PlayerBoard = player.initBoard
}