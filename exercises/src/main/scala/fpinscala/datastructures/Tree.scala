package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeF[A](t: Tree[A]): Int = fold(t)(_ => 1) { case (l, r) => 1 + l + r }

  def maximumF(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthF[A](t: Tree[A]): Int = fold(t)(_ => 0) { case (l, r) => 1 + (l max r) }

  def mapF[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(x => Leaf(f(x))) { case (l, r) => Branch(l, r) }

  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(size(t) == 5)
    assert(maximum(t) == 3)
    assert(depth(t) == 2)
    assert(map(t)(_ + 1) == Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
    assert(sizeF(t) == 5)
    assert(maximumF(t) == 3)
    assert(depthF(t) == 2)
    assert(mapF(t)(_ + 1) == Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
  }
}