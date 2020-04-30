package redbook.objectoriented
import BTreePrinter._

import scala.reflect.ClassTag
//https://github.com/TheDom/functional-programming-in-scala/tree/master/src/main/scala/com/dominikgruber/fpinscala
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeEx  extends  App{


  /**
   * Exercise 25
   * Write a function size that counts the number of nodes (leaves and branches)
   * in a tree.
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right)  =>1+size(left)+size(right)
  }

  val tree = Branch(
    Branch(
      Leaf(12),
      Branch(
        Leaf(3),
        Leaf(4))),
    Leaf(8))


  println(size(tree))
}
