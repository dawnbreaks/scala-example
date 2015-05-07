
package com.lubin.study

import scala.Ordered.orderingToOrdered
import scala.math.Ordering.StringOrdering

object OrderingAndOrderedTest extends App {
  class Fraction(n: Int, d: Int) {
    def value: Double = n.asInstanceOf[Double] / d.asInstanceOf[Double]
    override def toString(): String = n + "/" + d
  }
  object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d);
  }
  implicit def fractionOrderedFunc: Fraction => Ordered[Fraction] = { f =>
    new Ordered[Fraction] {
      override def compare(that: Fraction): Int = 
        if(f.value < that.value) -1
        else if(f.value == that.value) 0
        else 1
    }
  }
 
   /* implicit*/ val fractionOrderin2 = Ordering.by[Fraction, Double](f => f.value)
   
  implicit val fractionOrdering = new Ordering[Fraction]{
    override def compare(x: Fraction, y: Fraction): Int = 
      if(x.value < y.value) -1
      else if(x.value == y.value) 0
      else 1
  }
  
 /*
  * "implicit order: T => Ordered[T]" tell the compiler to find an implicit Function[T, Ordered[T]] in current context.
  *  Meant that T could be implicitly converse to Ordered[T]
  */
 def smaller1[T](a: T, b: T)(implicit order: T => Ordered[T]) = if (a < b) a else b
 /*
  * view bound, meant that T could be implicitly converse to Ordered[T]. 
  * Compiler will try to find an implicit Function[T, Ordered[T]] in current context.
  */
 def smaller2[T <% Ordered[T]](a: T, b: T) = if (a < b) a else b
 
 
 
 //context bound
 def smaller3[T : Ordering](a: T, b: T) = if (implicitly[Ordering[T]].compare(a, b) < 0) a else b
 /*
  * Strongly recommend this implementation: 
  * if your provide an Ordering[T] instance, you can simply import the func of scala.Ordered.orderingToOrdered, and then 
  * you have the ability to implicitly convert T to Ordered[T], so you can compare a and b by  statement "a < b"
  */
 
 //context bound and type implicitly conversion (T --> Ordered[T])
 def smaller4[T : Ordering](a: T, b: T) = if (a < b) a else b
 
 
  val a = Fraction(1, 7)
  val b = Fraction(2, 9)
  println("a=" + a)
  println("b=" + b)
  println("smaller1 method: The smaller one=" + smaller1[Fraction](a, b))
  println("smaller2 method: The smaller one=" + smaller2[Fraction](a, b))
  println("smaller3 method: The smaller one=" + smaller3[Fraction](a, b))
  println("smaller4 method: The smaller one=" + smaller4[Fraction](a, b))
  
  
  /*
   * Basic primitive type ordering. Please refer to scala.math.Ordering.StringOrdering.
   */
  val s1 = "Hello"
  val s2 = "Hello"
  val s3 = "Goodbye"
  val s4: String = null
  val s5 = "H" + "ello"

  if (s1 > s2) println("s1 == s2, good")
  if (s1 == s3) println("s1 == s3, bad")
  if (s1 == s4) println("s1 == s4, bad")
  if (s1 == s5) println("s1 == s5, good")
  
  
}



