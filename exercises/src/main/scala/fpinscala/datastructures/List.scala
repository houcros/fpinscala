package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, l)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => l
      case Cons(_, t) => drop(t, n - 1)
    }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def go(l: List[A])(acc: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => acc
      case Cons(h, t) => go(t)(Cons(h, acc))
    }
    go(l)(Nil)
  }

  def length[A](l: List[A]): Int = {
    @tailrec
    def go(l: List[A], n: Int): Int = l match {
      case Nil => n
      case Cons(_, t) => go(t, n + 1)
    }
    go(l, 0)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // This is how make reverse tail-recursive!
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A]) { (b, a) => Cons(a, b)}
    //foldRightTailRec(l, Nil: List[A]) { (a, b) => Cons(a, b) }

  // The straight-forward implementation of foldRight is not tail-recursive (stack-safe).
  // But foldRight is! So we can make foldRight tail-recursive leveraging foldLeft.
  def foldRightTR[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z) { (b, a) => f(a, b) }

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRightTR(l1, l2)(Cons(_, _))

  def concatenate[A](ls: List[List[A]]): List[A] =
    foldRightTR(ls, Nil: List[A]) { (l, acc) => append2(l, acc) }

  // Not stack-safe. Can be made stack-safe using foldRightTR.
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  // Same than for map.
  def filter[A, B](l: List[A])(p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if p(h) => Cons(h, filter(t)(p))
    case Cons(_, t) => filter(t)(p)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concatenate(map(l)(f))

  def filter2[A, B](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l) { a => if (p(a)) List(a) else Nil }

  def zipWith[A](l1: List[A])(l2: List[A])(f: (A, A) => A): List[A] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1)(t2)(f))
    }
}
