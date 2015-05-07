
package com.lubin.study

import scala.reflect.ClassTag
object ClassTagTest extends App {
  //ClassTag example
  def tabulate[T: ClassTag](len: Int, f: Int => T) = {
    var xs = new Array[T](len)
    for (i <- 0 until len) xs(i) = f(i)
    xs
  }

  var xs = tabulate[String](11, num => {
    num + ""
  })
  xs.map { x => println(x) }
}



