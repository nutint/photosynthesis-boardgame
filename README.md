# Photosynthesis Board Game
This is backend service for providing the logic of photosynthesis board game, it includes various game states 
including game registration, game play, scoring system, and end game mechanism

# Game Rules
please visit [here](https://ilo307.com/public/pdf/BO-PHOTO-002_RULES.PDF)

# Prerequisites
- Scala version 2.12.7 or above
- java version 1.8.xx
- [Scalatest](https://www.scalatest.org/) please read setup guidelines [here](https://www.scalatest.org/install) all you need to do in your local environment is to add global scala test resolver in `~/.sbt/0.13/global.sbt`
  ```scala
  resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"
  ```


# Installation
- git clone
- Install Intellij and Scala plugin from Intellij marketplace
- Restart Intellij
- Open Intellij and new project with existing sources
  - Import project with sbt
  - setup Import project  
    - download sbt sources.
    - use sbt shell for imports
    - use sbt shell for builds
    - select project JDK as Java 1.8.xxx (Scala run on top java jvm)
- validate installation by running unit test suite.

# Note
- Intellij auto format shortcut: option + cmd + l

