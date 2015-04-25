package com.jackbeasley.wordFind.test

import com.jackbeasley.wordFind.Word

class WordSpec extends UnitSpec{
  "A Word" should "print a breakdown of the coordinates of letters" in {
    val wrd = new Word("hello", Array((1,1), (2,2), (3,3), (4,4), (5,5)))
    wrd.toString should be ("h (1,1)\ne (2,2)\nl (3,3)\nl (4,4)\no (5,5)")
  }
  it should "correctly evaluate equality" in {
    val wrd1 = new Word("hello", Array((1,1), (2,2), (3,3), (4,4), (5,5)))
    val wrd2 = new Word("hello", Array((1,1), (2,2), (3,3), (4,4), (5,5)))
    val wrd3 = new Word("hello", Array((1,1), (2,1), (5,3), (4,2), (5,5)))
    val wrd4 = new Word("heeeo", Array((1,1), (2,2), (3,3), (4,4), (5,5)))
    wrd1.equals(wrd2) should be (true)
    wrd2.equals(wrd1) should be (true)
    wrd2.equals(wrd3) should be (false)
    wrd3.equals(wrd2) should be (false)
    wrd2.equals(wrd4) should be (false)
    wrd4.equals(wrd2) should be (false)
    wrd3.equals(wrd4) should be (false)
    wrd4.equals(wrd3) should be (false)
  }

  it should "concatinate with ++" in {
    val wrd1 = new Word("he", Array((1,1), (2,2)))
    val wrd2 = new Word("llo", Array((3,3), (4,4), (5,5)))
    val wrd3 = new Word("hello", Array((1,1), (2,2), (3,3), (4,4), (5,5)))

    wrd3.equals(wrd1 ++ wrd2) should be (true)
  }

  it should "create subwords as a string makes substrings" in {
    val wrd1 = new Word("he", Array((1,1), (2,2)))
    val wrd2 = new Word("llo", Array((3,3), (4,4), (5,5)))
    val wrd3 = new Word("hello", Array((1,1), (2,2), (3,3), (4,4), (5,5)))

    wrd3.subword(0,2).equals(wrd1) should be (true)
    wrd3.subword(2,5).equals(wrd2) should be (true)
   
  }

  it should "iterate through the possible words in decreasing order" in {
    val wrd = new Word("hello", Array((1,1), (2,2), (3,3), (4,4), (5,5)))
    val testWords = Array("hello", "hell", "hel", "he", "h")

    var index = 0
    for(word <- wrd.iterator){
      word.getWord should be (testWords(index))
      index += 1
    }
  }
}
