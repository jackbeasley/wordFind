package com.jackbeasley.wordFind

import scala.io.Source

object Dictionary {

  def getEnglish():Set[String] = {
    val ENGLISH_FILENAME = "/english.txt"
    val file = Source.fromURL(getClass().getResource(ENGLISH_FILENAME))
    val dictSet:Set[String] = file.getLines.toSet
    return dictSet
  }
}
