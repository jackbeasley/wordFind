package com.jackbeasley.wordFind

class Word(
  word:String,
  coordinates:Array[(Int,Int)]
) extends Iterable[Word]{
  // Define outer class for iterator nested subclass
  outer =>
  /*
   * Gets the string of the word
   */
  def getWord:String = word

  /*
   * Gets an array of the coordinates
   */
  def getCoordinates:Array[(Int,Int)] = coordinates

  def ++(wrd:Word):Word = {
    val str = word + wrd.getWord
    val arr = coordinates ++ wrd.getCoordinates
    return new Word(str, arr)
  }

  /*
   * Analogous to substring, returns the part of the word
   * start is inclusive, end is exclusive
   */
  def subword(start:Int, end:Int):Word =
    new Word(word.substring(start, end), coordinates.slice(start, end))

  override def iterator:Iterator[Word] =  new Iterator[Word] {
    // length of word to be returned
    var leng = outer.word.length

    def hasNext = leng > 0

    def next:Word = {
      val nextWord = outer.subword(0, leng)
      leng -= 1
      return nextWord
    }
  }

  override def equals(obj:Any):Boolean = {
    obj match {
      case w: Word => this.getWord == w.getWord &&
        this.getCoordinates.sameElements(w.getCoordinates)
      case _ => false
    }
  }

  override def toString:String = {
    var str = ""
    for(i <- 0 to word.length - 1){
      str += word.charAt(i) + " " + coordinates(i) + "\n"
    }
    // Remove last newline
    return str.substring(0,str.length()-1)
  }
}
