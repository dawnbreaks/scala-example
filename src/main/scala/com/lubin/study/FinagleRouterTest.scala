
package com.lubin.study

import com.twitter.finagle.http.path.Path
import com.twitter.finagle.http.path._
import scala.reflect.ClassTag

object FinalgleRouterTest extends App {
  val path = Path("/user/1");
  val Root / "user" / "accountId" / Integer(id) = Path("user") / "accountId" / "100"
  println(id)

  val Path("user") / Integer(id2) = Path("user") / "200"
  println(id2)

  val /(/(/(Root, "user"), "accountId"), Integer(id3)) = Path("user") / "accountId" / "300"
  println(id3)

  val var400 = /(/(/(Root, "user"), "accountId"), "400")

  println(var400)

}



