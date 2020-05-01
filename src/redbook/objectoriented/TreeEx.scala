package redbook.objectoriented
//https://github.com/TheDom/functional-programming-in-scala/tree/master/src/main/scala/com/dominikgruber/fpinscala
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeEx  extends  App {


  /**
   * Exercise 25
   * Write a function size that counts the number of nodes (leaves and branches)
   * in a tree.
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => println("left   " + left + "   right     " + right); 1 + size(left) + size(right)
  }

  val tree = Branch(
    Branch(
      Leaf(12),
      Branch(
        Leaf(3),
        Leaf(4))),
    Leaf(8))

  println("Size of " + size(tree))

  /**
   * Exercise 26
   * Write a function maximum that returns the maximum element in a Tree[Int].
   * (Note: in Scala, you can use x.max(y) or x max y to compute the maximum of
   * two integers x and y.)
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  println("Maximum value " + maximum(tree))

  /**
   * Exercise 27
   * Write a function depth that returns the maximum path length from the root
   * of a tree to any leaf.
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  println("Max depth  " + depth(tree))


  /**
   * Exercise 28
   * Write a function map, analogous to the method of the same name on List,
   * that modifies each element in a tree with a given function.
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  println(map(tree)(x => x.equals(2)))

  /**
   * Exercise 29
   * Generalize size, maximum, depth, and map, writing a new function fold that
   * abstracts over their similarities. Reimplement them in terms of this more
   * general function. Can you draw an analogy between this fold function and
   * the left and right folds for List?
   */
  def fold[A, B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }
}