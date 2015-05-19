package com.lubin.study

abstract class MapReduce[Input <: AnyRef, Output <: AnyRef] {
  import java.util.concurrent.{ExecutorService, Executors, Future, FutureTask, TimeUnit}

  /** Executor service to concurrent processing.
   *  Defaults to a fixed thread pool with (availableProcessors + 1) threads.
   */
  val executor: ExecutorService = Executors.newFixedThreadPool(
    Runtime.getRuntime.availableProcessors + 1
  )

  /** Implementation should override this operation */
  def map(input: Input): Output

  /** Implementation should override this operation */
  def reduce(o1: Output, o2: Output): Output

  /** Optional callback upon failure of worker */
  def reportException(t: Throwable) {
     t.printStackTrace
  }

  /** Submit a number of inputs to be mapped and reduced.
   *  Returns a Future with the eventual output.
   */
  final def submit(inputs: Traversable[Input]): Future[Output] = {
    val job = new Job(inputs.size)
    for (i <- inputs) {
      executor submit (new Worker(i, job))
    }
    job
  }

  /** Job holds completion status and computation output */
  private class Job(val workersExpected: Int) extends Future[Output] {
    @volatile private var cancelled = false
    private var workersCompleted = 0
    private var exception: Throwable = _

    var output: Output = _

    /** Signal completion of worker */
    private[MapReduce] def workerCompleted(): Unit = synchronized {
      workersCompleted += 1
      if (workersCompleted == workersExpected) {
        notifyAll()
      }
    }

    /** Signal an exception during processing */
    private[MapReduce] def reportException(t: Throwable): Unit = synchronized {
      exception = t
      notifyAll()
    }

    /** Attempts to cancel execution of this task. */
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
      cancelled = true
      (workersCompleted != workersExpected) && (exception ne null)
    }

    /** Waits if necessary for the computation to complete, and then retrieves its result. */
    override def get: Output = get(-1L, TimeUnit.MILLISECONDS)

    /** Waits if necessary for at most the given time for the computation to complete,
     *  and then retrieves its result, if available.
     */
     def get(timeout: Long, unit: TimeUnit): Output = {
       val start = System.currentTimeMillis
       var deadline = if (timeout >= 0) {
         System.currentTimeMillis + unit.toMillis(timeout)
       } else {
         Long.MaxValue
       }
       while (true) {
         synchronized {
           if (cancelled) {
             throw new java.util.concurrent.CancellationException("MapReduce was cancelled")
           }
           if (exception ne null) {
             throw exception
           }
           if (workersCompleted == workersExpected) {
             return output
           }
           wait(deadline - System.currentTimeMillis)
         }
       }
       sys.error("Unreachable")
     }

    /** Returns true if this task was cancelled before it completed normally. */
    def isCancelled: Boolean = cancelled

    /** Returns true if this task completed. Completion may be due to normal termination,
     *  an exception, or cancellation -- in all of these cases, this method will return true.
     */
    def 	isDone: Boolean = synchronized {
      (workersCompleted == workersExpected) || (exception ne null) || cancelled
    }
  }

  /** Map-reduce worker. */
  private class Worker(input: Input, job: Job) extends Runnable {
    def run: Unit = {
      try {
        if (job.isCancelled) return

        // first perform the map() operation
        var output = map(input)

        // reduce output with existing output until there
        // is no outstanding output available
        while (output ne null) {
          if (job.isCancelled) return

          val existing: Output = job.synchronized {
            val existing = job.output
            if (existing eq null) {
              job.output = output
              output = null.asInstanceOf[Output]
            } else {
              job.output = null.asInstanceOf[Output]
            }
            existing
          }

          // reduce happens outside of synchronized block
          // which means other threads can provide additional output,
          // or take, reduce and update the output.
          if (existing ne null) {
            output = reduce(existing, output)
          }
        }
      } catch {
        case ex: Throwable => job.reportException(ex)
      } finally {
        job.workerCompleted()
      }
    }
  }

}

object MapReduceTest extends App {
  case class Input(val x: Int)
  case class Output(val x: Int)

  val inputs = Array.fill[Input](1000) { Input(1) }

  val mr = new MapReduce[Input, Output] {
    def map(input: Input) = {
      // double input value
      Output(input.x * 2)
    }
    def reduce(o1: Output, o2: Output) = {
      // add up all outputs
      Output(o1.x + o2.x)
    }
  }
  
  val future = mr.submit(inputs)
  
  println(future.get)
  
  println((1 to 100).filter(_%2==1).fold(0)((x,y)=>x+y*y))

}
