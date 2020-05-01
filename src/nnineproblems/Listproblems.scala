package nnineproblems


object Listproblems  extends App{

  /**
   *
   * Find the last element in the list
   */

  val  qlist= List(0,1,2,3,5,6,7)

  def lastElement[A](l: List[A]):A = l match {
    case x::Nil => x
    case x::xs => lastElement(xs)
  }

  println(lastElement(qlist))


  /**
   *
   * Second last item from the list
   * @param l
   * @tparam A
   * @return second last itme
   */
  def secondLast[A](l:List[A]):A = l match{

    case x::List(xs)  =>x

    case x::xs => secondLast(xs)
  }

  println(secondLast(qlist))

  /**
   *
   * Find the Kth element at a list
   */


  def  kthelement [A](k:Int,l:List[A]):A =k match{

    case 0 => l.head
    case _ => kthelement(k-1, l.tail)

  }

  def findKth[A](k:Int, l:List[A]):A = (k,l) match {

    case (0, h::_) => h
    case (k, _::tail) if k > 0 => findKth(k - 1, tail)
    case _ => throw new NoSuchElementException
  }
  println(kthelement(0,qlist))

  println(findKth(0,qlist))


  def lengthList[A](l:List[A] ):Int = l match{
    case Nil => 0
    case x::xs =>  1+ lengthList(xs)
  }

  println(lengthList(qlist))

  def reverseList[A] (l:List[A]): List[A]= l match{
    case Nil=> Nil
    case x::Nil => List(x)
    case x::xs => reverseList(xs) ::: List(x)

  }

  println(reverseList(qlist))

}
