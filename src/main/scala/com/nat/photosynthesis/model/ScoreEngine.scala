package com.nat.photosynthesis.model

object ScoreEngine {
  def calculatePlayerScore(player: Player, forestBlocks: List[Block], sunLocation: SunLocation): Int = {
    val playersPlant = forestBlocks.filter(_.plantItem.plantType == player.plantType)
    playersPlant.foldLeft(0) { (totalScore, currentPlayerPlant) =>
      totalScore + currentPlayerPlant.calculateScore(sunLocation, forestBlocks)
    }
  }
}
