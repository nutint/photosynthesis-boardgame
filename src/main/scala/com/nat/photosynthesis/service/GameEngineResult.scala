package com.nat.photosynthesis.service

sealed trait GameEngineResult
case class GameAdded(roomId: String) extends GameEngineResult
