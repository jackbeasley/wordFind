package com.jackbeasley.wordFind.test

import com.jackbeasley.wordFind.WordGrid
import com.jackbeasley.wordFind.Word
import scala.io.Source

class WordGridSpec extends UnitSpec {
  
  "A WordGrid" should "instantiate from a String and dimensions" in {
    val wg = new WordGrid("abcdefghi", 3, 3)
    val testStr = "abc\ndef\nghi"
    wg.toString should be (testStr)
  }

  it should "read in the test file correctly" in {
    val file = Source.fromURL(getClass.getResource("/three.txt"))
    val wg = WordGrid.gridFromFile(file)
    val testStr = "rbc\neat\ntst"
    wg.toString should be (testStr)

    // Use 14x14 test grid
    val file2 = Source.fromURL(getClass.getResource("/fourteen.txt"))
    val wg2 = WordGrid.gridFromFile(file2)
  }
  
  it should "correctly scan a word stem" in {
    // Use 14x14 test grid
    val file = Source.fromURL(getClass.getResource("/fourteen.txt"))
    val wg = WordGrid.gridFromFile(file)

    val test1 = new Word("jo", Array((6,0), (7,0))) // journals
    val test2 = new Word("bo", Array((6,4), (7,5))) // books

    val result1 = wg.scanWordStem(test1)
    val result2 = wg.scanWordStem(test2)

    val expected1 = new Word("journals",
      Array((6,0),(7,0),(8,0),(9,0),(10,0),(11,0),(12,0),(13,0)))
    val expected2 = new Word("books", Array((6,4),(7,5),(8,6),(9,7),(10,8)))

    result1 should be (expected1)
    result2 should be (expected2)

  }

  it should "check if a loc is in bounds" in {
     // Use 14x14 test grid
    val file = Source.fromURL(getClass.getResource("/fourteen.txt"))
    val wg = WordGrid.gridFromFile(file)

    wg.inBounds((-1,-1)) should be (false)
    wg.inBounds((-1,0)) should be (false)
    wg.inBounds((3,4)) should be (true)
    wg.inBounds((15,0)) should be (false)
  }

  it should "find all the words" in {
    // Use 14x14 test grid
    val file = Source.fromURL(getClass.getResource("/fourteen.txt"))
    val wg = WordGrid.gridFromFile(file)

    println(wg.getWords.mkString)

    true should be (false)
  }
   
}

