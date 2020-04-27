package redbook.objectoriented

import scala.util.Random

object paternmatching extends App {

  val random = new Random
  val x =random.nextInt(10)

  val description = x match{
    case 1 => "the One"
    case 2 => "doube or nothing"
    case 3 => "third"
    case _=> "something else"
  }

  println(x)
  println(description)

trait Expr

  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e:Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr





  val lastlist =List(1,2,3,4,6)
  //Pattern match on a function.

    def last[A] ( l:List[A] ) :A = l match{     // function last of Any parameter  accepts a list of  Any and returns Any
      case h :: Nil => h     //exit condition  - list last elment is Nil, :: Corn operator to split list
      case _:: tail => last(tail)  //recurse until find the tail and ignore head.
      case _ => throw new NoSuchElementException
    }


  println(last(lastlist))



  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list") //empty List of 0
    case List(x) => List() //one element  List of 1
    case h :: t => h :: init(t) //add the List of elements more than one
  }

println(init(lastlist))
  val clist =List(4,8,16,32,40)

  def concat[T](xs: List[T], ys : List[T] ):List[T] = xs match{
    case List() => ys  //first list is empty then return the second list alone
    case z :: zs  => {
      println(z +"======" + zs + "======"+ys)

      z :: concat(zs,ys)
    } // z is the first element of xs and zs rest of the elements in xs
                                        // one element in the first list add the second list elements recusively
  }

println(concat(lastlist,clist))

//remove index
  def removeAt[T](n: Int, xs: List[T]):List[T] =  (xs take n) ::: (xs drop n+1)

  println(removeAt(1, List('a', 'b', 'c', 'd')) )// List(a, c, d)

//flatten list
  def flatten[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: tail => (head match {
      case l: List[T] => flatten(l)
      case i => List(i)
    }) ::: flatten(tail)
  }
  println( flatten(List(List(1, 1), 2, List(3, List(5, 8)))))


  def sum( ints : List[Int]): Int = ints match{

    case List() => 0
    case x:: xs => x + sum(xs)
  }

  println(sum (List(12,3,6,7)))


  def prod( ints: List[Int]): Int = ints match{

    case List() =>1
    case 0:: xs=> 0
    case x::xs => x * prod(xs)
  }

  println(prod(List(1,2,3)))


  def removefirst(intlist: List[Int]):List[Int] = intlist match{

    case List() => List()
    case x::xs => xs
  }

  println(removefirst(List(1,2,3,5,6,7,9)))

//replace first element
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil  => sys.error ("Empty list")
    case x::xs => List(h)::: xs
  }


  println(setHead(List("a","b","c","d","f"), "A"))
 //drop elements
  def drop[A](l: List[A], n: Int): List[A] = if (n<=0 )l else l match{

    case Nil => Nil
    case x:: xs => drop ( xs, (n-1))
  }

println(drop (List("a","b","c","d","f"), 3) )

//removes elements from the List prefix as long as they match a predicate.
  def dropWhile[A](l: List[String], f: String => Boolean): List[String] = l match{
    case x:: xs   if f(x)  => dropWhile(xs,f)
    case _ =>  println(l);l
  }

  def foo(b: String): Boolean = {b.equals("a")}
  println(dropWhile(List("a","a","c","d","f"),foo))

}
