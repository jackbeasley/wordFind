package com.jackbeasley.wordFind.test

import org.scalatest._
import com.jackbeasley.wordFind.WordGrid
import scala.io.Source

class WordGridSpec extends UnitSpec {
  "A WordGrid" should "Instantiate from a String and dimensions" in {
    val wg = new WordGrid("abcdefghi", 3, 3)
    val testStr = "abc\ndef\nghi"
    wg.toString should be (testStr)
  }

  it should "Read in the test file correctly" in {
    val file = Source.fromURL(getClass.getResource("/three.txt"))
    val wg = WordGrid.gridFromFile(file)
    val testStr = "rbc\neat\ntst"
    wg.toString should be (testStr)
  }
}

