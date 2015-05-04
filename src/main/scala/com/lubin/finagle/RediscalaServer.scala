package com.lubin.finagle;
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.path._
import com.twitter.finagle.http.service.RoutingService
import com.twitter.finagle.Service
import com.twitter.finagle.http._
import com.twitter.util.Future
import java.net.InetSocketAddress
import scala.util.parsing.json.JSONObject
import redis.RedisClient
import akka.util.ByteString
import scala.concurrent.ExecutionContext.Implicits.global
import com.twitter.util.Promise

object RediscalaServer extends App {

  implicit val akkaSystem = akka.actor.ActorSystem()
  val redis = RedisClient("192.168.0.8", 6379)

  //async set operation
  def setService(key: String, value: String) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val res = Response(Version.Http11, Status.Ok)
      res.setContentTypeJson()
      try {
        //update redis 
        var setFuture = redis.set(key, value)
        var promise = new Promise[Response]
        var responseFuture = setFuture.map { x =>
          {
            x match {
              case true => {
                var resJson = JSONObject(Map("code" -> 0, "msg" -> "ok", "data" -> ""))
                res.setContentString(resJson.toString())
              }
              case false => {
                //TODO  more detail error info?
                var resJson = JSONObject(Map("code" -> -1, "msg" -> "unknow failed", "data" -> ""))
                res.setContentString(resJson.toString())
              }
            }
            promise.setValue(res)
          }
        }
        promise
      } catch {
        case t: Throwable => {
          t.printStackTrace()
          var resJson = JSONObject(Map("code" -> -1, "msg" -> t.getMessage, "data" -> ""))
          res.setContentString(resJson.toString())
          Future(res)
        }
      }
    }
  }

  //async del operation
  def delService(key: String) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val res = Response(Version.Http11, Status.Ok)
      res.setContentTypeJson()
      try {
        //update redis 
        var delFuture = redis.del(key)
        var promise = new Promise[Response]
        var responseFuture = delFuture.map { x =>
          {
            if (x >= 0) {
              var resJson = JSONObject(Map("code" -> 0, "msg" -> "ok", "data" -> ""))
              res.setContentString(resJson.toString())
            } else {
              //TODO  more detail error info?
              var resJson = JSONObject(Map("code" -> -1, "msg" -> "unknow failed", "data" -> ""))
              res.setContentString(resJson.toString())
            }
            promise.setValue(res)
          }
        }
        promise
      } catch {
        case t: Throwable => {
          t.printStackTrace()
          var resJson = JSONObject(Map("code" -> -1, "msg" -> t.getMessage, "data" -> ""))
          res.setContentString(resJson.toString())
          Future(res)
        }
      }

    }
  }

  //async get operation
  def getService(key: String) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val res = Response(Version.Http11, Status.Ok)
      res.setContentTypeJson()
      try {
        var getFuture = redis.get(key)
        var promise = new Promise[Response]
        var responseFuture = getFuture.map { x =>
          {
            val data = JSONObject(Map("key" -> key, "value" -> x.getOrElse(ByteString("nil")).utf8String))
            var resJson = JSONObject(Map("code" -> 0, "msg" -> "ok", "data" -> data))
            res.setContentString(resJson.toString())
            promise.setValue(res)
          }
        }
        promise
      } catch {
        case t: Throwable => {
          t.printStackTrace()
          var resJson = JSONObject(Map("code" -> -1, "msg" -> t.getMessage, "data" -> ""))
          res.setContentString(resJson.toString())
          Future(res)
        }
      }
    }
  }

  var index = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val rep = Response(req.getProtocolVersion(), Status.Ok)
      rep.setContentType("text/html", "UTF-8")
      rep.setContentString("RediscalaServer based on finagle and rediscala!<br/><a href='/set/key1/value1'>set key1 = value1</a><br/><a href='/get/key1'>get key1</a><br/><a href='/del/key1'>del key1</a><br/>")
      Future(rep)
    }
  }

  val blackHole = new Service[Request, Response] {
    def apply(request: Request): Future[Response] = {
      val resp = request.method match {
        case Method.Get => Response(Version.Http11, Status.Ok)
        case _ => Response(Version.Http11, Status.NotFound)
      }

      Future(resp)
    }
  }

  val router = RoutingService.byPathObject[Request] {
    case Root => index
    case Root / "set" / key / value => setService(key, value)
    case Root / "get" / key => getService(key)
    case Root / "del" / key => delService(key)
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
