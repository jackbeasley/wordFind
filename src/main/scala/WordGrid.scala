package com.jackbeasley.wordFind

import scala.io.Source

class WordGrid(
  data:String, rows:Int, cols:Int
) {
  // Instanciates 2D array of chars from string data
  val grid : Array[Array[Char]] = {
    var arr = Array.ofDim[Char](rows, cols)
      var ct = 0
      for(r <- 0 to rows - 1){
        for(c <- 0 to cols - 1){
          arr(r)(c) = data.charAt(ct)
          ct = ct + 1
        }
      }
      arr
  }

  // Creates a simple string grid pattern
  override def toString : String = {
    var str = ""
    grid.foreach(str += _.mkString + "\n")
    str = str.substring(0, str.length - 1)
    return str
  }
}

object WordGrid{

  /*
   * Reads in grid data from a text file. First two
   * lines hold dimmensions and the rest is grid data
   */
  def gridFromFile(src:Source):WordGrid = {
    val str = src.mkString.replaceAll("""[\n\r]""", "")
    val rows = str.substring(0,1).toInt
    val cols = str.substring(1,2).toInt
    val grid = str.substring(2, str.length)
    return new WordGrid(grid, rows, cols)
  }
}
