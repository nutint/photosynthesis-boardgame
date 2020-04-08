package com.nat.photosynthesis.service

sealed trait GameEngineCommand
case object AddGame extends GameEngineCommand
