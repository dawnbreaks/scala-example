
package com.lubin.study

object Twice {
	def apply(x: Int): Int = x * 2
	def unapply(z: Int): Option[Int] = if (z % 2 == 0) Some(z / 2) else None
}

object ExtractorTest extends App {
	private def doMatch(num: Int): Unit = {
		num match {
			case Twice(n) => Console.println(n) // prints 21
			case _ => println("un-matched")
		}
	}

	private def doMatch2(num: Int) = {
		num match {
			case Twice(n) => Console.println(n) // prints 21
			case _ => println("un-matched"); 
		}
	}
	
	var num :Int = 1
	
	doMatch(Twice(31))
	doMatch(Twice(30))
	doMatch(31)
	doMatch(30)
}



