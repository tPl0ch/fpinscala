package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => sys.error("Calling tail on an empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case _ => sys.error("Calling setHead on an empty list")
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else l match {
      case Cons(_, t) => drop(t, n - 1)
      case Nil => Nil
    }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, b) => b + 1)

  // TODO: report inconsistency foldLeft (auto-curried) and foldRight
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumViaFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productViaFoldLeft(ns: List[Double]) =
    foldLeft(ns, 0.0)(_ * _)

  def lengthViaFoldLeft(ns: List[Double]) =
    foldLeft(ns, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b, (a, g) => b => g(f(b, a)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons(_, _))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((acc, a) => Cons(a, acc))

  // TODO: report wrong signature
  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])(appendViaFoldLeft)

  def lengthViaFoldLeft[A](l: List[A]): Int = ???

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (i, acc) => Cons(i + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (i, acc) => Cons(i.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldLeft(reverse(l), Nil: List[B])((acc, a) => Cons(f(a), acc))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldLeft(reverse(as), Nil: List[A])((acc, a) => if (f(a)) then Cons(a, acc) else acc)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    concat(map(as)(f(_)))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as, a => if (f(a)) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPairwise(ta, tb))
      case (_, Nil) => Nil
      case (Nil, _) => Nil
    }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    (l, r) match {
      case (Cons(hl, tl), Cons(hr, tr)) => Cons(f(hl, hr), zipWith(tl, tr)(f))
      case (_, Nil) => Nil
      case (Nil, _) => Nil
    }

  def zipWithStackSafe[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    @tailrec
    def loop(ll: List[A], rr: List[B], acc: List[C]): List[C] =
      (ll, rr) match {
        case (Cons(hl, tl), Cons(hr, tr)) => loop(tl, tr, Cons(f(hl, hr), acc))
        case (_, Nil) => acc
        case (Nil, _) => acc
      }
    reverse(loop(l, r, Nil))

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    @tailrec
    def startsWith(l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Cons(hl, tl), Cons(hp, tp)) if hl == hp => startsWith(tl, tp)
      case _ => false
    }

    sup match {
      case _ if startsWith(sup, sub) => true
      case Nil => Nil == sub
      case Cons(_, t) => hasSubsequence(t, sub)
    }
