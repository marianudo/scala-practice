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
    if (ls.isEmpty) return Nil
    if (ls.length < numberOfElements) ls
    else aux(ls, Nil)
  }

  def drop(ls: List[Int], numberOfElements: Int): List[Int] = {
    if (numberOfElements == 0) ls
    else drop(ls.tail, numberOfElements - 1)
  }

  def zip(l1: List[Int], l2: List[Int]): List[(Int, Int)] = {
    //    if(l1.isEmpty || l2.isEmpty) Nil
    //    else (l1.head, l2.head) :: zip(l1.tail, l2.tail)
    def aux(l1: List[Int], l2: List[Int], output: List[(Int, Int)]): List[(Int, Int)] = {
      if(l1.isEmpty || l2.isEmpty) output
      else aux(l1.tail, l2.tail, output ::: List((l1.head, l2.head)))
    }
    aux(l1, l2, Nil)
  }

  def reverse(ls: List[Int]): List[Int] = {
    def aux(sourceList: List[Int], outputList: List[Int]): List[Int] = {
      if (sourceList.isEmpty) outputList
      else aux(take(sourceList, sourceList.length - 1), outputList ::: List(last(sourceList)))
    }
    aux(ls, Nil)
  }

  def map(ls: List[Int], f: Int => Int): List[Int] = {
    if (ls.isEmpty) Nil
    else f(ls.head) :: map(ls.tail, f)
  }
}
