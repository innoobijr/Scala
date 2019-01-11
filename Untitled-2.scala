type Par[A] = ExecutorService => Future[A]


object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def async[A, B](f: A => B): A => Par[B] =
    // this {lazyUnit(f)}
    (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((a, _) => a.sorted)

  //Generalize
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(async(f))
    sequence(fbs)
  }
  
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))
    
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    map(parMap(as)((a: A) => if (f(a)) List(a) else Nil))(_.flatten)
  }
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  
  def choiceN[A](n: Par[A])(choices: List[Par[A]]): Par[A] = es => { run(e)(choices(run(es)(n).get))}

  def choice[A](cond: Par[Boolean])(t: Par[A], f:Par[A]): Par[A] = choiceN(map(cond)(b => if (b) 0 else 1))(List(t :: f))
  
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = 
    es => {
        run(es)(choices.get(run(e)(key).get))
    }
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = 
    es => {
        run(es)(choices(run(es)(pa).get))
    }
  def choice[A](cond: Par[Boolean])(t: Par[A], f:Par[A]): Par[A] = 
    chooser(cond)(a => if (a) t else f)

  def join[A](a: Par[Par[A]]): Par[A] = 
    es =>
    {
        a match {
            case a: Par[Par[_] => join(a)
            case b: Par[A] => b.get
        }
        run(es)(run(es)(a).get).get
    }  
}