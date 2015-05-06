package com.jackbeasley.wordFind

import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    // Use 3x3 test grid
    val file = Source.fromURL(getClass.getResource("/three.txt"))
    val wg = WordGrid.gridFromFile(file)

    println(wg.getWords.mkString)
  }
}
