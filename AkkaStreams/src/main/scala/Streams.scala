//Import full complement of streaming tools
import akka.stream._
import akka.stream.scaladsl._

import akka.{ NotUsed, Done }
import akka.actor.ActorSystem
import akka.util.ByteString
import scala.concurrent._
import scala.concurrent.duration._
import java.nio.file.Paths

object Main extends App {

  // We need a couple of implicit values
  // an actor system, since we are using Akka
  // and a Materializer
  implicit val system = ActorSystem("Streams")
  implicit val materializer = ActorMaterializer()
  /**
    *   Materializer is a factory for stream execution engines,
    *   it is te thing that makes streams run
    */

  // Start with a rather simple source
  // Parameterized with two arguments:
  //  The first is the type of the element being emitted
  //  The second is of some auxilary type that might be included
  val source: Source[Int, NotUsed] = Source(1 to 100)

  // Source is not active yet, in order the make the source active
  // we need to call
  val done: Future[Done] = source.runForeach(i => println(i))(materializer)

  implicit val ec = system.dispatcher
  done.onComplete(_ => system.terminate())

  val factorials = source.scan(BigInt(1))((acc, next) => acc * next)
  val result: Future[IOResult] =
    factorials.map(num => ByteString(s"$num\n"))
    .runWith(FileIO.toPath(Paths.get("factorials.txt")))

  /**
    *  `scan` is ised ot run computation ver the whole stream: strarting with the number 1
    *  we multiply by each of the incoming numbers, one after the other, the scan operation emits the initial value
    *  and then every calculation result. This yields the series of factorial bumber which we stash aways as a Source for later resues.
    *  This is just a description of what needs to be computed on the stream (nothign is actually done yet).
   */



}