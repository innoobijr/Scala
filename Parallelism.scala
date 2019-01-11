import scala.concurrent.duration.TimeUnit


//Just defining the library
/*abstract class Par[+A]{
  def unit[A](a: => A): Par[A] // Promotes a constant value to parallel computation

  def get[A](a: Par[A]): A //extracts value from a Par by actually performing computation

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]  =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  //combines the results of two parallel
  // computations

  def fork[A](a: => Par[A]): Par[A] // marks a computation for concurrent evaluation. Evaluatin wont run unless forced by run

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a)) //wraps unevaluated argument in a Par and marks its for concurrent evaluation

  def run[A](a: Par[A]): A //extrac
}*/

type Par[A] = ExecutorService => Future[A]


object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // Promotes a constant value to parallel computation

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]  =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A,B](f: A => B): A => Par[B] =
    this {lazyUnit(f)}

  def choiceN[A](n: Par[Int](choices: List[Par[A]]): Par[A] = {
    run
  }

}

abstract class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A]{def call: A}
trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isCancelled: Boolean
}


