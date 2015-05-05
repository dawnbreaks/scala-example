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
import com.twitter.util.Duration
import com.twitter.finagle.RequestTimeoutException
import com.twitter.util.Timer
import com.twitter.finagle.Filter
import com.twitter.finagle.IndividualRequestTimeoutException
import com.twitter.finagle.service.TimeoutFilter
import com.twitter.util.JavaTimer
import com.twitter.util.ScheduledThreadPoolTimer
import com.twitter.finagle.SimpleFilter
import com.twitter.util.TimeoutException

object RediscalaServer extends App {

  implicit val akkaSystem = akka.actor.ActorSystem()
  val redis = RedisClient("192.168.0.8", 6377)

  //async set operation
  def setService(key: String, value: String) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val res = Response(Version.Http11, Status.Ok)
      res.setContentTypeJson()
      //update redis 
      val setFuture = redis.set(key, value)
      val promise = new Promise[Response]
      val responseFuture = setFuture.map { x =>
        {
          x match {
            case true => {
              val resJson = JSONObject(Map("code" -> 0, "msg" -> "ok", "data" -> ""))
              res.setContentString(resJson.toString())
            }
            case false => {
              //TODO  more detail error info?
              val resJson = JSONObject(Map("code" -> -1, "msg" -> "unknow failed", "data" -> ""))
              res.setContentString(resJson.toString())
            }
          }
          promise.setValue(res)
        }
      }
      promise
    }
  }

  //async del operation
  def delService(key: String) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val res = Response(Version.Http11, Status.Ok)
      res.setContentTypeJson()
      //update redis 
      val delFuture = redis.del(key)
      val promise = new Promise[Response]
      val responseFuture = delFuture.map { x =>
        {
          if (x >= 0) {
            val resJson = JSONObject(Map("code" -> 0, "msg" -> "ok", "data" -> ""))
            res.setContentString(resJson.toString())
          } else {
            //TODO  more detail error info?
            val resJson = JSONObject(Map("code" -> -1, "msg" -> "unknow failed", "data" -> ""))
            res.setContentString(resJson.toString())
          }
          promise.setValue(res)
        }
      }
      promise
    }
  }

  //async get operation
  def getService(key: String) = new Service[Request, Response] {
    def apply(req: Request): Future[Response] = {
      val res = Response(Version.Http11, Status.Ok)
      res.setContentTypeJson()
      val getFuture = redis.get(key)
      val promise = new Promise[Response]
      val responseFuture = getFuture.map { x =>
        {
          val data = JSONObject(Map("key" -> key, "value" -> x.getOrElse(ByteString("nil")).utf8String))
          val resJson = JSONObject(Map("code" -> 0, "msg" -> "ok", "data" -> data))
          res.setContentString(resJson.toString())
          promise.setValue(res)
        }
      }
      promise
    }
  }

  val index = new Service[Request, Response] {
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

  def timeoutResponse(msg: String) = {
    val timeoutResponse = Response(Version.Http11, Status.Ok)
    timeoutResponse.setContentTypeJson()
    timeoutResponse.setContentString(JSONObject(Map("code" -> -1, "msg" -> s"TimeoutException|msg=$msg", "data" -> "")).toString())
    timeoutResponse
  }
  def unknownErrorResponse(msg: String) = {
    val unknownErrorResponse = Response(Version.Http11, Status.Ok)
    unknownErrorResponse.setContentTypeJson()
    unknownErrorResponse.setContentString(JSONObject(Map("code" -> -1, "msg" -> "unknown error|msg=$msg", "data" -> "")).toString())
    unknownErrorResponse
  }

  implicit val timer = new ScheduledThreadPoolTimer();
  def timeoutFilter(timeout: Duration)(implicit timer: Timer) = new SimpleFilter[Request, Response] {
    def apply(request: Request, service: Service[Request, Response]): Future[Response] = {
      val res = service(request)
      res.within(timer, timeout)
    }
  }

  val handleExceptions = new SimpleFilter[Request, Response] {
    override def apply(req: Request, service: Service[Request, Response]): Future[Response] = {
      service(req) handle {
        case e: TimeoutException => timeoutResponse(e.getMessage)
        case t: Throwable => unknownErrorResponse(t.getMessage)
      }
    }
  }

  import com.twitter.conversions.time._
  val router = RoutingService.byPathObject[Request] {
    case Root => index
    case Root / "set" / key / value => handleExceptions andThen timeoutFilter(300.millis) andThen setService(key, value)
    case Root / "get" / key => handleExceptions andThen timeoutFilter(300.millis) andThen getService(key)
    case Root / "del" / key => handleExceptions andThen timeoutFilter(300.millis) andThen delService(key)
    case _ => timeoutFilter(300.millis) andThen blackHole
  }

  var port = 80
  val server = ServerBuilder()
    .codec(RichHttp[Request](Http()))
    .bindTo(new InetSocketAddress(80))
    .name("FinagleTest")
    .build(router)

  println(s"Http server ready for connections on port $port")
}
