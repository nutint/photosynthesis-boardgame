package com.nat.photosynthesis.service

sealed trait GameEngineResult
case class GameAdded(roomId: String) extends GameEngineResult
case object PlayerAdded extends GameEngineResult
case class AddPlayerFailed(reason: String) extends GameEngineResult
