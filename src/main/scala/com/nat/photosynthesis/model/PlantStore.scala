package com.nat.photosynthesis.model

case class PlantStore(
  plantType: PlantType,
  seedStore: List[Priced[Seed]],
  smallTreeStore: List[Priced[MediumTree]],
  mediumTreeStore: List[Priced[MediumTree]],
  largeTreeStore: List[Priced[MediumTree]],
)
