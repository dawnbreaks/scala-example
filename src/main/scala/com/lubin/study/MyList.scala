package com.lubin.study;

import scala.annotation.tailrec

sealed trait MyList[+A]

abstract class MyListOpt[+A](myList: MyList[A]){
  def head(): A
  def tail(): MyList[A]
  def setHead[B >: A](head: B): MyList[B]
  
  def map[B](f: A=> B): MyList[B]
  def foreach(f: A=>Unit): Unit
}


private case class MyOneEleList[A](headEle: A) extends MyList[A]
private case class MyPairList[A](headEle: A, tailList: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] = {
    if(as.isEmpty) throw new UnsupportedOperationException 
    as.length match {
      case 1 =>  MyOneEleList(as.head)
      case n if n > 1=> MyPairList(as.head, apply(as.tail:_*))
    }
  }
  
  implicit def  myList2MyListOpt[A](myList: MyList[A]): MyListOpt[A] = new MyListOpt[A](myList){
    override def head(): A = myList match {
      case MyPairList(headEle, tailList) => headEle
      case MyOneEleList(headEle) => headEle
    }
    
    override def tail(): MyList[A] = myList match {
      case MyPairList(headEle, tailList) => tailList
      case MyOneEleList(headEle) => myList
    }
    
    override def setHead[B >: A](head: B): MyList[B] = myList match {
      case MyPairList(headEle, tailList) => MyPairList(head, tailList)
      case MyOneEleList(headEle) => MyOneEleList(head)
    }
    
    def map[B](f: A=> B): MyList[B] = myList match {
      case MyPairList(headEle, tailList) => MyPairList(f(headEle), tailList.map(f))
      case MyOneEleList(headEle) => MyOneEleList(f(headEle))
    }
    
    
    def foreach(f: A=>Unit): Unit = myList match {
      case MyPairList(headEle, tailList) => f(headEle); tailList.foreach(f)
      case MyOneEleList(headEle) => f(headEle)
    }
  }
}

object Main extends App{
  val list = MyList(1,3,5)
  println(list)
  println(list.head)
  println(list.tail)
  println(list.setHead(100).head)
  list.map(_*2).foreach{ a => print(a); print(" ")}
}


//object List {
//  def apply[A](as: A*): List[A] = {
//    if(as.isEmpty) Nil else Cons(as.head, apply(as.tail:_*))
//  }
//
//  /** Exercise 3.2 */
//  def tail[A](xs: List[A]): List[A] = xs match {
//    case Nil => throw new UnsupportedOperationException
//    case Cons(_, ys) => ys
//  }
//
//  /** Exercise 3.3 */
//  def setHead[A](xs: List[A], head: A): List[A] = xs match {
//    case Nil => throw new UnsupportedOperationException
//    case Cons(_, ys) => Cons(head, ys)
//  }
//
//  /** Exercise 3.4 */
//  @tailrec def drop[A](xs: List[A], n: Int): List[A] = xs match {
//    case Nil => Nil
//    case Cons(_, ys) if n > 0 => drop(ys, n - 1)
//    case ys: Cons[A] => ys
//  }
//
//  /** Exercise 3.5 */
//  @tailrec def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
//    case Nil => Nil
//    case Cons(y, ys) if f(y) => dropWhile(ys, f)
//    case ys: Cons[A] => ys
//  }
//
//  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
//    case Nil => z
//    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//  }
//
//  /** Exercise 3.10 */
//  @tailrec def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
//    case Nil => z
//    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
//  }
//
//  /** Exercise 3.11 */
//  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
//
//  /** Exercise 3.11 */
//  def product(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
//
//  /** Exercise 3.11 */
//  def length(as: List[_]): Int = foldLeft(as, 0)((x, _) => x + 1)
//
//  /** Exercise 3.12 */
//  def reverse[A](as: List[A]): List[A] = foldLeft[A, List[A]](as, Nil) { (xs, x) => Cons(x, xs) }
//}



