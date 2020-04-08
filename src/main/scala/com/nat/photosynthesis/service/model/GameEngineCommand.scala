package com.nat.photosynthesis.service.model

sealed trait GameEngineCommand
case object AddGame extends GameEngineCommand
case class AddPlayer(player: Player) extends GameEngineCommand
case object StartGame extends GameEngineCommand