package com.nat.photosynthesis.service

import com.nat.photosynthesis.service.model._

sealed trait GameEngineCommand
case object AddGame extends GameEngineCommand
case class AddPlayer(roomId: String, playerName: String, plantType: PlantType) extends GameEngineCommand
