package redbook.objectoriented

object Polymorphic  extends App {

 val arr = Array(1,2,3,4,5)
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1 else if (p(as(n))) n else loop(n + 1)
loop(0)
  }
  def isFirst(i: Int) = i == 2

  println(findFirst(arr, (a:Int)=>a==3))


  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (gt(as(n - 1), as(n))) loop(n + 1)
      else false
    }
    loop(1)
  }

  println(isSorted(arr, (a: Int, b: Int) => a > b))


  println(isSorted(Array(3,2,1), (a: Int, b: Int) => a > b))



}