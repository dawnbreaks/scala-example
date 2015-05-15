
package com.lubin.study

object OtherTypeBoundTest extends App {
  
  
  /*
A =:= B A must be equals B
A <:< B A must be subtype of B
A <%< B A must be view as B     //deprecated symbol?  "(implicit evidence: A <%< B )" equals to "(implicit func: A > B )" 
   */
  
  
  //A =:= B A must be equals B
  class Container1[A](value: A) {
    def addIt(implicit evidence: A =:= Int ) = 100 + value
  }
  object Container1 {
    def apply[A](x: A) = new Container1(x)
  }
  println("Container1=" +Container1(100).addIt)
  
  // A <:< B A must be subtype of B  
  abstract class Animal(number: Int){
    def nu = number
  }
  case class Dog(number: Int) extends Animal(number)
  
  class Container2[A](value: A) {
    def addIt(implicit evidence: A <:< Animal) = 200 + value.nu
  }
  object Container2 {
    def apply[A](x: A) = new Container2(x)
  }
  println("Container2=" + Container2(Dog(200)).addIt)
  
  
  //A <%< B A must be view as B     //deprecated symbol?
  sealed abstract class <%<[From <% To, To] 
  private[this] final val singleton_=:= = new <%<[Any, Any] {}
  object <%< {
     implicit def tpEquals[A <% B, B]: <%<[A, B] = {
       println("tpEquals") 
       singleton_=:=.asInstanceOf[<%<[A, B]]
     }
  }
  class Container3[A](value: A) {
    //implicit evidence is redundant....
    def addIt(implicit func: A => Int, evidence: <%<[A, Int] ) = 300 + value
  }
  object Container3 {
    def apply[A](x: A) = new Container3(x)
  }
  
  implicit def dog2Int(dog: Dog): Int = {
    println("dog2Int")
    dog.number
  }
  def printIntForDog[T <% Int](t: T) = printInt(t)
  def printInt(v: Int) =println(v)
  printInt(Dog(100))
  
  println("Container3=" + Container3(Dog(300)).addIt)
  
}



