package com.lubin.finagle;
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.path._
import com.twitter.finagle.http.service.RoutingService
import com.twitter.finagle.Service
import com.twitter.finagle.http._
import com.twitter.util.Future
import java.net.InetSocketAddress
import scala.util.parsing.json.JSONObject

object FinableExample extends App {

  def userService(id: Int) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val rep = Response(Version.Http11, Status.Ok)
      // scala.util.parsing.json.JSONObject
      val o = JSONObject(Map("id" -> id, "name" -> "lubin wang"))
      rep.setContentTypeJson()
      rep.setContentString(o.toString())
      Future(rep)
    }
  }

  def echoService(message: String) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val rep = Response(req.getProtocolVersion(), Status.Ok)
      rep.setContentTypeJson()
      rep.setContentString("[{}]")
      Future(rep)
    }
  }

  val blackHole = new Service[Request, Response] {
    def apply(request: Request): Future[Response] = {
      val resp = request.method match {
        case Method.Get => Response(Version.Http11, Status.Ok)
        case _          => Response(Version.Http11, Status.NotFound)
      }

      Future(resp)
    }
  }

  val router = RoutingService.byPathObject[Request] {
    case Root / "user" / Integer(id) => userService(id)
    case Root / "echo"/ message => echoService(message)
    case _ => blackHole
  }

//  val server = ServerBuilder()
//    .codec(RichHttp[Request](Http()))
//    .bindTo(new InetSocketAddress(8080))
//    .name("router")
//    .build(router)
  
    var path = Path("/user/1");
  	var Root / "user" / "accountId" / Integer(id) = Path("user") / "accountId" / "100"
  	println(id)
  	
  	var Path("user") / Integer(id2) = Path("user") / "200"
  	println(id2)
  	
  	var /(/(/(Root, "user"), "accountId"), Integer(id3)) = Path("user") / "accountId" / "300"
  	println(id3)
}
