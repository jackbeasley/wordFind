package com.jackbeasley.wordFind

import scala.io.Source
import scala.util.control.Breaks._

class WordGrid(
  data:String, rows:Int, cols:Int
) {
  // TODO: Implement non-english dictionary support
  val dict:Set[String] = Dictionary.getEnglish()

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

  /*
   * Searches for, finds and returns all the words in the grid
   */
  def getWords:Array[Word] = {
    var words:Array[Word] =  Array()

    for(r <- 1 to rows - 2){
      for(c <- 1 to cols -2){
        val newWords = scanOffset((r,c), scanWordStem)
        words ++ newWords
      }
    }

    return words
  }

  /*
   * Given a set of coordinates, this finds all possible 2 letter combinations
   * and call a fcn that follows up on checking the word stem
   */
  def scanOffset(coord:(Int,Int), fcn:Word => Word):Array[Word] = {
    var words:Array[Word] = Array[Word]()
    def checkWord(i:Int, j:Int) = {
      if(!(i == 0 && j == 0)){
          val coordArr = Array((coord._1,coord._2),
            (coord._1 + i, coord._2 + j))
          val str = {
            "" + grid(coordArr(0)._1)(coordArr(0)._2) +
            grid(coordArr(1)._1)(coordArr(1)._2)
          }
          //println(coordArr.mkString)
        val finalWord = fcn(new Word(str, coordArr))
        if(finalWord != null){
          words :+ finalWord
        }
          
        }
    }
    for(i <- -1 to 1){
      for(j <- -1 to 1){
        checkWord(i, j)
      }
    }
    return words
  }

  /*
   * Scans the stem of a word by folloowing its offset and returns the longest word
   * if it is found, otherwise it returns null
   */
  def scanWordStem(stem:Word):Word = {
    def getMaxWord:Word = {
      val offset:(Int,Int) = (stem.getCoordinates(1)._1 - stem.getCoordinates(0)._1,
      stem.getCoordinates(1)._2 - stem.getCoordinates(0)._2)
    
      val initLoc:(Int,Int) = stem.getCoordinates(1)

      // Create moveable loc
      var loc = initLoc
    
      // maxWord will build up until it includes all the possible letters
      var maxWord:Word = stem

      // Apply offset to loc
      loc = (loc._1 + offset._1, loc._2 + offset._2)

      while(inBounds(loc)){
        // Grid is (y,x) not (x,y)
        val nextLetter = new Word("" + grid(loc._2)(loc._1), Array(loc))
        maxWord = maxWord ++ nextLetter

        // Apply offset to loc
        loc = (loc._1 + offset._1, loc._2 + offset._2)
      }
      return maxWord
    }

    val maxWord = getMaxWord

    for(word <- maxWord.iterator){
      //TODO: Remove magic number
      if(dict.contains(word.getWord) && word.getWord.length > 2){
        return word
      }
    }

    // No word found
    return null
  }

  def inBounds(loc:(Int,Int)):Boolean = {
    return (loc._1 < cols && loc._1 >= 0) && (loc._2 < rows && loc._2 >= 0)
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
    val lines = src.getLines.toList
    val rows = lines(0).toInt
    val cols = lines(1).toInt
    val grid = lines.slice(2,lines.length).mkString.replaceAll("""[\n\r]""", "")
    src.close()
    return new WordGrid(grid, rows, cols)
  }
}
