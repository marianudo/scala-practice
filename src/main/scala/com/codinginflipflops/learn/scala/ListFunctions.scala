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
    if(ls.isEmpty) return Nil
    if(ls.length < numberOfElements) ls
    else aux(ls, Nil)
  }

  def drop(ls: List[Int], numberOfElements: Int): List[Int] = {
    if(numberOfElements == 0) ls
    else drop(ls.tail, numberOfElements - 1)
  }
}
