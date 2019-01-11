
trait RNG {
    def nextInt: (Int, RNG)
}
type Rand[+A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] = 
    rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B]=
    rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
    }

def map2[A, B, C](ra: Rand[A]), rb: Rand[B])(f: (A, B) => C): Rand[C]:{
    rng => {
        val (a, rng2) => ra(rng)
        val (b, rng3) => rb(rng2)
        (f(b), rng3)
    }

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i, r) = rng.nextInt
  (if (i < 0) -(i + 1) else i, r)
}

//Because the type here is actually a function
// I can map nonNegativeInt whose signature is 'RNG => (Int, RNG)' 
// which is exactly the specification for the type Rand[+A]
// So in this case it lose very much like a bind because the tranformation, is binded to the set of generated integers.
def nonNegativeEven: Rand[Int] = 
    map(nonNegativeInt)(i => i - i % 2)


// Using map to implement double
def doubleViaMap(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
}


def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
    map2(ra, rb)(( _, _))
}

// Run to see if this is the correct implementation
def sequence[A](fd: List[Rand[A]]): Rand[List[A]] = {
    fd.foldLeft(unit(List.empty[A]))((a, b) => map2(a, b)((a, b) => a :+ b))
    }

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
        val (a, r1) = f(rng)
        g(a)(r1) //This get us the function but because the above can be written as:
        // A => (RNG => (B, RNG)) --> (A, RNG) => (B, RNG)
        // It think this is like a partial, so pass the state with the function 
        //so g can look like this:
        // def g(a: A): Rand[B] = {
        ///   a -> b
        ///   b -> (b, RNG) 
        }
    }
}

// Implement map and map2 via flatMap
def mapViaflatMap[A, B](s: Rand[A])(f: A => B): Rand[B]: {
        flatMap(s)(unit(f))
}

def map2ViaflatMap[A, B, C](ra: Rand[A]), rb: Rand[B])(f: (A, B) => C): Rand[C]:{
        flatMap(ra)(map(rb)(b => f(a, b)))
}

def double(rng: RNG): (Double, RNG) = {
  val (i, r) = nonNegativeInt(rng)
  (i / (Int.MaxValue.toDouble + 1), r)
}

def intDouble(rng: RNG): ((Int, Double), RNG):
    val r1 = double(rng)
    ((r1._1 * (Int.MaxValue.toDouble + 1), r1._1), r1._2)

def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
    map2(ra, rb)(( _, _))
}

def ints(count: Int)(rng: RNG): (List[Int], RNG):{
    @tailrec.annotation
    def go(c:Int, l: List[Int], rng: RNG): (List[Int], RNG) ={
        c match {
            case 0: (l, rng)
            case _: {
                c -= 1
                val (a, r1) = nonNegativeInt(rng)
                go(c, l :: a, r1)
            }
    }
    }
        go(count, List.empty[Int], rng)
}

//type Rand[+A] =  RNG => (A, RNG)
type State[S, +A] = S => (A, S)
type Rand[A] =  State[RNG, A]

// Generalize the functions into a State case class or companion object
case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B]: {
        flatMap( a => unit(f(a))
    }
    
    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C]:{
        flatMap(a => sb.map(b => f(a, b)))
    }

    def flatMap[B](g: A => State[S, B]): State[S, B] = State (
    rng => {
        val (a, r1) = run(rng)
        g(a).run(r1) //This get us the function but because the above can be written as:
        // A => (RNG => (B, RNG)) --> (A, RNG) => (B, RNG)
        // It think this is like a partial, so pass the state with the function 
        //so g can look like this:
        // def g(a: A): Rand[B] = {
        ///   a -> b
        ///   b -> (b, RNG) 
        })
    }
object State {
    // objects take the state
    def unit[S, A](a: A): State[S, A] = 
    State(rng => (a, rng))

    def sequence[S, A](fd: List[State[S, A]]): State[S, [List[A]] = {
    fd.foldLeft(unit[S, List[A]](List.empty[A]))((a, b) => b.map2(a)((a, b) => a :+ b))
    }
    // combinator for modifying the state
    def modify[S](f: S => S): State[S, Unit] = for {
        s <- get
        _ <- set(f(s))
    } yield ()

    def get[S]: State[S, S] = State (s => (s, s))

    def set[S](s, S): State[S, Unit] = State(_ => ((), s))

}

trait Knob {
    def evolve: (Boolean, Knob)
}
type Locked[A] = State[Knob, A]

sealted trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine[A](locked: Boolean, candies: Int, coins: Int)

case class MachineOps


case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    // This is the beauty of function composition
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}