
package com.lubin.study

object CollectionTest extends App {
  var list = List(1,2,3,5)
  def printEle(x: Any) = print(" " + x)
  
  list.foreach(printEle)
  println()
  
  list.map( _ + 1).foreach(printEle)
  println()
  
  list.flatMap(x => List(x + 1)).foreach(printEle)
  println()
  
  list.scan(0)((x, y)=>x+y).foreach(printEle)
  println()
  
  val foldValue = list.fold(100)((x,y)=>x + y)
  println(foldValue)
  
  val foldLeftValue = list.foldLeft(100)((x,y)=>x + y)
  println(foldLeftValue)
  
  
  val foldRightValue = list.foldRight(1000)((x,y)=>x + y)
  println(foldRightValue)
  
  println(list.find( _ == 1).getOrElse(-1))
  
  import scala.reflect.ClassTag
  class TestArray[T : ClassTag]{
    val array = new Array[T](10)
    
  }
  
}



