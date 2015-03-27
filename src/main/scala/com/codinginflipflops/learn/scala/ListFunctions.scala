package com.codinginflipflops.learn.scala

object ListFunctions {
  def last(ls: List[Int]): Int = {
    if (ls.tail == Nil) ls.head
    else last(ls.tail)
  }

  def init(ls: List[Int]): List[Int] = {
    def aux(sourceList: List[Int], growingList: List[Int]): List[Int] = {
      if (sourceList.tail == Nil) growingList
      else aux(sourceList.tail, growingList ++ List(sourceList.head))
    }
    aux(ls, Nil)
  }

  def take(ls: List[Int], numberOfElements: Int): List[Int] = {
    def aux(sourceList: List[Int], growingList: List[Int]): List[Int] = {
      if (growingList.length == numberOfElements) growingList
      else aux(sourceList.tail, growingList ++ List(sourceList.head))
    }
    aux(ls, Nil)
  }

  def drop(ls: List[Int], numberOfElements: Int): List[Int] = {
    def aux(shrinkingList: List[Int], elementsDropedSoFar: Int): List[Int] = {
      if(elementsDropedSoFar == numberOfElements) shrinkingList
      else aux(shrinkingList.tail, elementsDropedSoFar + 1)
    }
    aux(ls, 0)
  }
}
