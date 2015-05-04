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
      
      val o = JSONObject(Map("id" -> id, "name" -> "lubin wang"))
      rep.setContentTypeJson()
      rep.setContentString(o.toString())
      Future(rep)
    }
  }

  def echoService(message: String) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val rep = Response(req.getProtocolVersion(), Status.Ok)
      rep.setContentType("text/plain", "UTF-8")
      rep.setContentString(message)
      Future(rep)
    }
  }
  
   var index = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val rep = Response(req.getProtocolVersion(), Status.Ok)
      rep.setContentType("text/html", "UTF-8")
      rep.setContentString("Hello twitter finagle!<br/><a href='/user/1'>first user</a><br/><a href='/echo/hellolubin'>echo test</a><br/>")
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
    case Root => index
    case Root / "user" / Integer(id) => userService(id)
    case Root / "echo"/ message => echoService(message)
    case _ => blackHole
  }
  
  var port = 80
  val server = ServerBuilder()
          .codec(RichHttp[Request](Http()))
          .bindTo(new InetSocketAddress(80))
          .name("FinagleTest")
          .build(router)

  println(s"Http server ready for connections on port $port")
}
