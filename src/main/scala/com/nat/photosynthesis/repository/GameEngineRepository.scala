package com.nat.photosynthesis.repository

import com.nat.photosynthesis.service.model.engine.GameEngine

trait GameEngineRepository {
  def getGameById(gameId: String): Either[String, GameEngine]
}
