package com.lubin.study;

import scala.annotation.tailrec
object MyList2Test extends App {

  sealed trait MyList[+A]

  abstract class MyListOpt[+A](myList: MyList[A]) {
    def head(): A
    def tail(): MyList[A]
    def setHead[B >: A](head: B): MyList[B]

    def map[B](f: A => B): MyList[B]

    def foreach(f: A => Unit): Unit
    def quickForeach(f: A => Unit): Unit

    def drop(n: Int): MyList[A]
    def dropWhile(f: A => Boolean): MyList[A]

    def foldLeft[B](z: B)(f: (A, B) => B): B
    def foldRight[B](z: B)(f: (A, B) => B): B

    def stringRepr(): String
  }

  private case object MyNilList extends MyList[Nothing]
  private case class MyPairList[A](headEle: A, tailList: MyList[A]) extends MyList[A]

  object MyList {
    def apply[A](as: A*): MyList[A] = {
      as.length match {
        case 0 => MyNilList
        case n if n > 0 => MyPairList(as.head, apply(as.tail: _*))
      }
    }

    //  @tailrec
    def quickMap[A, B](myList: MyList[A], f: A => B): MyList[B] = myList match {
      case MyPairList(headEle, tailList) => MyPairList(f(headEle), quickMap(tailList, f))
      case MyNilList => MyNilList
    }

    @tailrec
    def quickForeach[A](myList: MyList[A], f: A => Unit): Unit = myList match {
      case MyPairList(headEle, tailList) =>
        f(headEle); quickForeach(tailList, f)
      case MyNilList => Unit
    }

    //TODO  use @tailrec annotation to optimize performance of methods like map/drop/foreach/foldLeft... 
    implicit def myList2MyListOpt[A](myList: MyList[A]): MyListOpt[A] = new MyListOpt[A](myList) {
      override def head(): A = myList match {
        case MyPairList(headEle, tailList) => headEle
        case MyNilList => throw new RuntimeException("Bad operation on empty List")
      }

      override def tail(): MyList[A] = myList match {
        case MyPairList(headEle, tailList) => tailList
        case MyNilList => throw new RuntimeException("Bad operation on empty List")
      }

      override def setHead[B >: A](head: B): MyList[B] = myList match {
        case MyPairList(headEle, tailList) => MyPairList(head, tailList)
        case MyNilList => MyPairList(head, MyNilList)
      }

      //impossible to use @tailrec
      override def map[B](f: A => B): MyList[B] = myList match {
        case MyPairList(headEle, tailList) => MyPairList(f(headEle), tailList.map(f))
        case MyNilList => MyNilList
      }

      override def foreach(f: A => Unit): Unit = myList match {
        case MyPairList(headEle, tailList) =>
          f(headEle); tailList.foreach(f)
        case MyNilList => Unit
      }
      override def quickForeach(f: A => Unit): Unit = MyList.quickForeach(myList, f)

      //TODO use @tailrec
      override def drop(n: Int): MyList[A] = {
        if (n < 0) throw new RuntimeException(s"Invalid parameters: n=$n")
        else if (n == 0) myList
        else {
          myList match {
            case MyPairList(headEle, tailList) => tailList.drop(n - 1)
            case MyNilList => MyNilList
          }
        }
      }

      //impossible to use @tailrec
      override def dropWhile(f: A => Boolean): MyList[A] = myList match {
        case MyPairList(headEle, tailList) =>
          if (f(headEle)) tailList.dropWhile(f)
          else MyPairList(headEle, tailList.dropWhile(f))
        case MyNilList => MyNilList
      }

      //TODO use @tailrec
      override def foldLeft[B](z: B)(f: (A, B) => B): B = myList match {
        case MyPairList(headEle, tailList) =>
          val accu = f(headEle, z)
          tailList.foldLeft(accu)(f)
        case MyNilList => z
      }

      //impossible to use @tailrec
      override def foldRight[B](z: B)(f: (A, B) => B): B = myList match {
        case MyPairList(headEle, tailList) =>
          val accu = tailList.foldRight(z)(f)
          f(headEle, accu)
        case MyNilList => z
      }
      //TODO use @tailrec
      override def stringRepr(): String = myList match {
        case MyNilList => ""
        case MyPairList(headEle, MyNilList) => headEle.toString
        case MyPairList(headEle, tailList: MyPairList[A]) => headEle + " " + tailList.stringRepr()
      }
    }
  }

  def printList[A](msg: String, list: MyList[A]) {
    println(s"$msg:  " + list.stringRepr())
  }

  val list = MyList(1, 3, 5, 7, 9, 11, 13, 15)
  printList("list", list)
  println("list.head = " + list.head)
  printList("list.tail", list.tail)

  printList("list.setHead(100)", list.setHead(100))
  printList("list.map(_ * 2)", list.map(_ * 2))

  printList("list.drop(4)", list.drop(4))

  printList("list.dropWhile(_ > 7)", list.dropWhile(_ > 7))

  println("list.foldLeft(100) =  " + list.foldLeft(100)((x, y) => x + y))
  println("list.foldRight(200) = " + list.foldRight(200)((x, y) => x + y))
  print("Test annotaion of @tailrec : ")
  list.quickForeach { x => print(x); print(" ") }
}

