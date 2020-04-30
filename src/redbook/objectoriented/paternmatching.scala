package redbook.objectoriented

import scala.util.Random

object paternmatching extends App {

  val random = new Random
  val x = random.nextInt(10)

  val description = x match {
    case 1 => "the One"
    case 2 => "doube or nothing"
    case 3 => "third"
    case _ => "something else"
  }

  println(x)
  println(description)

  trait Expr

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr


  val lastlist = List(1, 2, 3, 4, 6)
  //Pattern match on a function.

  def last[A](l: List[A]): A = l match { // function last of Any parameter  accepts a list of  Any and returns Any
    case h :: Nil => h //exit condition  - list last elment is Nil, :: Corn operator to split list
    case _ :: tail => last(tail) //recurse until find the tail and ignore head.
    case _ => throw new NoSuchElementException
  }


  println(last(lastlist))


  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list") //empty List of 0
    case List(x) => List() //one element  List of 1
    case h :: t => h :: init(t) //add the List of elements more than one
  }

  println(init(lastlist))
  val clist = List(4, 8, 16, 32, 40)

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys //first list is empty then return the second list alone
    case z :: zs => {
      println(z + "======" + zs + "======" + ys)

      z :: concat(zs, ys)
    } // z is the first element of xs and zs rest of the elements in xs
    // one element in the first list add the second list elements recusively
  }

  println(concat(lastlist, clist))

  //remove index
  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

  println(removeAt(1, List('a', 'b', 'c', 'd'))) // List(a, c, d)

  //flatten list
  def flatten[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: tail => (head match {
      case l: List[T] => flatten(l)
      case i => List(i)
    }) ::: flatten(tail)
  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))


  def sum(ints: List[Int]): Int = ints match {

    case List() => 0
    case x :: xs => x + sum(xs)
  }

  println(sum(List(12, 3, 6, 7)))


  def prod(ints: List[Int]): Int = ints match {

    case List() => 1
    case 0 :: xs => 0
    case x :: xs => x * prod(xs)
  }

  println(prod(List(1, 2, 3)))


  def removefirst(intlist: List[Int]): List[Int] = intlist match {

    case List() => List()
    case x :: xs => xs
  }

  println(removefirst(List(1, 2, 3, 5, 6, 7, 9)))

  //replace first element
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Empty list")
    case x :: xs => List(h) ::: xs
  }


  println(setHead(List("a", "b", "c", "d", "f"), "A"))

  //drop elements
  def drop[A](l: List[A], n: Int): List[A] = if (n <= 0) l else l match {

    case Nil => Nil
    case x :: xs => drop(xs, (n - 1))
  }

  println(drop(List("a", "b", "c", "d", "f"), 3))

  //removes elements from the List prefix as long as they match a predicate.
  def dropWhileS[A](l: List[String], f: String => Boolean): List[String] = l match {
    case x :: xs if f(x) => dropWhileS(xs, f)
    case _ => println(l); l
  }

  def foo(b: String): Boolean = {
    b.equals("a")
  }

  println(dropWhileS(List("a", "a", "c", "d", "f"), foo))

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case h :: t if f(h) => dropWhile(t)(f)
    case _ => as
  }

  val xs: List[Int] = List(1, 2, 3, 4, 5, 0, 4)
  val ex1 = dropWhile(xs)(x => x < 4)

  println(ex1)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }


  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  println(sum2(xs))


  def prod2(ns: List[Int]): Int = foldRight(ns, 1)((x, y) => x * y)

  println(prod2(xs))

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => {
    println(y)
    y + 1

  })

  length(xs)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)

  }

  def sum3[A](l: List[Int]): Int = foldLeft(l, 0)((_ + _))

  println("sum fleft" + sum3(xs))

  def prod3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  val xs2: List[Int] = List(1, 2, 3, 4, 5, 0, 4)
  val xs3: List[Int] = List(1, 2, 3, 4, 5, 1, 4)

  println("Prod" + prod3(xs2))
  println("Prod" + prod3(xs3))

//reverse using fold left
  def rev3[A](l:List[A]):List[A]= foldLeft(l, Nil:List[A]) ((x,y)=> (y::x))

  println(rev3(xs2))
  //append list
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1,a2)((x,xs)=> (x::xs) )
  println(append2(xs2,xs3))

  /**
   * Exercise 16
   * Write a function that transforms a list of integers by adding 1 to each
   * element. (Reminder: this should be a pure function that returns a new List!)
   */
  def add1(l: List[Int]): List[Int] =foldRight(l,Nil:List[Int])((x,xs) => x+1 :: xs )
  println(xs)
  println(add1(xs))

  /**
   * Exercise 17
   * Write a function that turns each value in a List[Double] into a String.
   * You can use the expression d.toString to convert some d: Double to a String.
   */
  def doubleListToString(l: List[Double]): List[String] =foldRight(l,Nil:List[String])((x,xs)=> x.toString::xs)

  println(doubleListToString(List(1.2,1.4,1.6,1.90)))


  /**
   * Exercise 18
   * Write a function map that generalizes modifying each element in a list
   * while maintaining the structure of the list.
   */
  def map[A,B](l: List[A])(f: A => B): List[B] =foldRight(l,Nil:List[B])((x,xs)=>f(x)::xs)

  println(map(xs)(x => x-1))

  /**
   * Exercise 19
   * Write a function filter that removes elements from a list unless they
   * satisfy a given predicate. Use it to remove all odd numbers from a
   * List[Int].
   */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l,Nil:List[A])((x,xs)=> if (f(x))x::xs else xs)

  println(xs)
  println(filter(xs)(x=> x.equals(4)))
  /**
   * Exercise 20
   * Write a function flatMap that works like map except that the function given
   * will return a list instead of a single result, and that list should be
   * inserted into the final resulting list.
   */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((h, t) => append2(f(h), t))

  println(flatMap(xs)( x =>  x::xs))

  /**
   * Exercise 21
   * Can you use flatMap to implement filter?
   */
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x =>
      if (f(x)) List(x)
      else Nil
    )
println (filter2(xs)((x=> x.equals(3))))

  /**
   * Exercise 22
   * Write a function that accepts two lists and constructs a new list by adding
   * corresponding elements. For example, List(1,2,3) and List(4,5,6) becomes
   * List(5,7,9).
   */
  def addInt(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
      case(_,Nil) => Nil
    case (x::xs, y::ys) => x+y:: addInt(xs,ys)

  }

  println(addInt(xs2,xs3))

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
  }
  println(zipWith(xs,xs3)( (x,y)=> x+y))

  /**
   * Exercise 24 (hard)
   * As an example, implement hasSubsequence for checking whether a List
   * contains another List as a subsequence. For instance, List(1,2,3,4) would
   * have List(1,2), List(2,3), and List(4) as subsequences, among others. You
   * may have some difficulty finding a concise purely functional implementation
   * that is also efficient. That’s okay. Implement the function however comes
   * most naturally. We’ll return to this implementation in chapter 5 and
   * hopefully improve on it. Note: any two values, x, and y, can be compared
   * for equality in Scala using the expression x == y.
   */
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def startsWith(ls: List[A], ss: List[A]): Boolean = (ls, ss) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case ((x::xs), (y:: ys)) =>
        if (x == y) startsWith(xs, ys)
        else false
    }
    def go(ls: List[A]): Boolean = ls match {
      case Nil => false
      case (x:: xs) =>
        if (startsWith(ls, sub)) true
        else go(xs)
    }
    go(l)
  }

  }