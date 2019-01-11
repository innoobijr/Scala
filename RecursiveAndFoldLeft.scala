import scala.collection.immutable
import scala.util.Sorting

def fib(n: Int): Int = {
  @annotation.tailrec
  def go(x: Int, y: Int, z: Int ): Int = {
     x match {
       case 1 => z
       case 0 => y
       case _ => go(x-1, z, z+y)
     }
  }
  go(n, y=0, z=1)
}

fib(5)

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) :  Boolean = {

  def go(n: Int ): Boolean = {
    if (n >= as.length - 1) true
    else if (ordered(as(n), as(n+1))) true
    else go(n+1)
  }
  go(0)
}

val x: Array[Int] = Array(1, 2, 3, 4)
val b: Array[Int] = Array(5, 5, 5, 57)


def sort[A](a: A, b: A)(implicit ev: A => Ordered[A]): Boolean = {
  a < b
}

isSorted(b, (x: Int, y: Int)=> x < y)

def curry[A, B, C](f: (A, B) => C): A => (B => C) = {

  (a: A) => (b: B) => f(a, b)
}

def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}


def tailu[A](as: List[A]): List[A] = {
  as match {
    case _ :: Nil =>  Nil
    case a :: b => b
    case Nil => as
  }
}

def setHeads[A](as: List[A], nv: A): List[A] = {
  as match {
    case Nil => sys.error("setHead on empty list")
    case a :: b => nv :: b
    case _ => Nil
  }
}

setHeads(List(1, 2, 3, 4), 8)

def myDrop[A](as: List[A], nv: Int): List[A] = {
  @annotation.tailrec
  def go (i: Int, acc: List[A]): List[A] = {
    i match {
      case 0 => acc
      case _ => go(i - 1, tailu(acc))
    }
  }
  go(nv, as)
}

//Improving type inference
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  l match {
    case h :: t if f(h) => dropWhile(t, f)
    case _ => l
  }

}

//We can also write dropWhile as such
/**def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
  l match {
    case h :: t if f(h) => dropWhile(t)(f) //curry in the predicate
    case _ => l
  }
}**/

myDrop(List(1, 2, 3, 4), 5)

dropWhile(List(1, 2, 3, 4), (i: Int) => i < 4)


/************************************
Recursion over lists and generalizing to higher-order functions
  ***********************************/
//Previous implementation
def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case x :: xs => x + sum(xs)
}


/**def product(ds: List[Double]): Double = ds match {
  case Nil => 1.0
  case 0.0 :: _ => 0.0
  case x :: xs => x * product(xs)
}**/

def product(ds: List[Double]): Double = ds match {
  case Nil => 1.0
  case x :: xs => x * product(xs)
}

//These two functions are extremely similar add for  the value to return for sum and product
// for empty list. When this is the case. It is possible to define functions
// take over the concern

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }
}

//Now we can rewrite the two function as
def sum2(ns: List[Int]) =
  foldRight(ns, 0)((x, y) => x + y)

def product2(ns: List[Double]): Double = {
  foldRight(ns, 1.0)(_ * _)
}

foldRight(List(1, 2, 3), Nil:List[Int])(_ :: _)


def lengthi[A](as: List[A]): Int = {
  foldRight(as, 0)((a, i) => i + 1)
}

lengthi(List(1, 2, 3, 6, 7, 8))

def foldLeftK[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
  @annotation.tailrec
  def go(as: List[A], g: B): B = {
    as match {
      case h :: t => go(t, f(g,h))
      case Nil => g
    }
  }
  go(as, z)
}

//foldLeftK(List(5, 5, 5, 5, 5))((a, b) => a+b)

//foldLeftK(List(1, 2, 3, 4))((a, b) => (b, a))

def reverse[A](l: List[A]): List[A] = {
  foldLeftK(l, List[A]())((acc, h) => h :: acc)
}


def concat[A](l: List[A], r: List[A]): List[A] = {
  foldLeftK(r, l)((a, b)=> reverse(b :: reverse(a)))
}

concat(List(1, 2, 3), List(3, 4, 6, 7))

val ints: List[Int] = List(1, 2, 3, 4)

def addOne(l: List[Int]): List[Int] = {
  foldRight(l, Nil: List[Int])((a, b) => a+1 :: b)
}

def doubleToString(d: List[Double]): List[String] = {
  foldRight(d, Nil: List[String])((a, b) => a.toString :: b)
}

val c: List[Double] = List(1.0, 2.0, 3.0)

doubleToString(c)(0)

//Write a function map that generalizes modifying each element of list
def map[A, B](as: List[A])(f: A => B): List[B] = {
  foldRight(as, Nil: List[B])((a, b) => f(a) :: b)
}

def filter[A](as: List[A])(f: A => Boolean): List[A] = {
  foldRight(as, Nil: List[A])((a, b) => f(a) match {
    case true => a :: b
    case false => b
  })
}

filter(List(1, 2, 3, 4, 5))(a => a < 2)

//foldRigh via foldLeft

def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
  foldLeftK(l, (b:B) => b)((g, a) => b => g(f(a, b)))(z)
}

def flatMap[A, B](as: List[A])(f: A => List[B]) : List[B] = {
  foldLeftK(as, Nil: List[B])((a, b) => a ::: f(b))
}

def filterTwo[A](as: List[A])(f: A => Boolean): List[A] = {
  flatMap(as)((i: A) => f(i) match {
    case true => List(i)
    case false => Nil
  })
}

def filterThree[A](as: List[A])(f: A => Boolean): List[A] = {
  flatMap(as)((i: A) => if (f(i)) List(i) else Nil)
}

flatMap(List(1, 2, 4, 3))(i => List(i, i))

filter(List(1, 2, 5, 7, 8))(_ < 5)

def combine(as: List[Int], it: List[Int]): List[Int] = {
  def go(n: Int, r: List[Int]): List[Int] = {
    n match {
      case n if n <= as.length - 1 => go(n + 1, r ::: as(n) + it(n) :: Nil)
      case _ => r
    }
  }
  go(0, Nil: List[Int])
}

def combin(as: List[Int], it: List[Int]): List[Int] = {
  (as, it) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (a :: b, c :: d) => a + c :: combin(b, d)
  }
}

combine(List(1, 2, 4, 3), List(1, 2, 4, 3))

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  sub match {
    case x :: y => {
      def check[A](j: List[A]): Boolean = {
        j match {
        case a :: b => if ((x == a)) hasSubsequence(y, b) else check(b)
        case Nil => false
      }
      }
      check(sup)
    }
    case Nil => true
  }
}

hasSubsequence(List(1, 2, 3, 4), List(2, 5))

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](start: Tree[A]): Int = {
  start match {
    case Branch(a, b) => 1 + size(a) + size(b)
    case Leaf(x) => 1
  }
}
val br1: Branch[Int] = Branch(Leaf(1), Leaf(5))
val br4: Branch[Int] = Branch(Leaf(7), Leaf(9))
val br3: Branch[Int] = Branch(Leaf(6), br4)
val br5: Branch[Int] = Branch(Leaf(6), br3)
val br2: Branch[Int] = Branch(Leaf(6), br5)

val br: Branch[Int] = Branch(br1, br2)

size(br)

def maximum(start: Tree[Int]): Int = {
  start match {
    case Branch(a, b) => maximum(a) max maximum(b)
    case Leaf(x) => x
  }
}
maximum(br)

def depth[A](start: Tree[A]): Int = {
  start match {
    case Leaf(_) => 0
    case Branch(a, b) => (1 + depth(a)) max (1 + depth(a))

  }
}

depth(br)
