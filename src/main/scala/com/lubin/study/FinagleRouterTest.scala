
package com.lubin.study

import com.twitter.finagle.http.path.Path
import com.twitter.finagle.http.path._
import scala.reflect.ClassTag


object FinalgleRouterTest extends App {
	var path = Path("/user/1");
  	var Root / "user" / "accountId" / Integer(id) = Path("user") / "accountId" / "100"
  	println(id)
  	
  	var Path("user") / Integer(id2) = Path("user") / "200"
  	println(id2)
  	
  	var /(/(/(Root, "user"), "accountId"), Integer(id3)) = Path("user") / "accountId" / "300"
  	println(id3)
  	
  	var var400 = /(/(/(Root, "user"), "accountId"), "400")
  	
  	var var5:Int = 3
  	println(var400)
    
    
}



