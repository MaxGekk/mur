package mur

import org.scalatest.{FreeSpec, Matchers}

class MainTests extends FreeSpec with Matchers {
  "Main example" - {
    "calculate Pi" in {
      Main.getPi shouldBe "pi = 3.143588659585789"
    }
  }
}
