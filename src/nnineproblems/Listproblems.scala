package nnineproblems

import scala.annotation.tailrec


object Listproblems  extends App {

  /**
   *
   * Find the last element in the list
   */

  val qlist = List(0, 1, 2, 3, 5, 6, 7)

  def lastElement[A](l: List[A]): A = l match {
    case x :: Nil => x
    case x :: xs => lastElement(xs)
  }

  println(lastElement(qlist))


  /** 1
   *
   * Second last item from the list
   *
   * @param l
   * @tparam A
   * @return second last itme
   */
  def secondLast[A](l: List[A]): A = l match {

    case x :: List(xs) => x // exit case when tail is composed of a list of one element then return the previous element head.

    case x :: xs => secondLast(xs) //recurse
  }

  println(secondLast(qlist))

  /** 2
   *
   * Find the Kth element at a list
   */


  def findKth[A](k:Int, l:List[A]):A = (k,l) match {

    case (0, h::_) => h // k 0 and list which has a head
    case (k, _::tail) if k > 0 => findKth(k - 1, tail )// if k> 0 , recurse until k is 0, each time index is reduced by 1
    case _ => throw new NoSuchElementException // empty list

  }
  println(findKth(0, qlist))

  /** 3
   *
   * simple while loop
    * @param l
   * @tparam A
   * @return
   */
  def listLoop[A](l:List[A]):List[A] = l match {
    case Nil=> List()
    case h::Nil => Nil
    case h::tail => println("tail" + tail); listLoop(tail)
  }

  println(listLoop(qlist))


  /** 4
   *
   * Lenght of the list
   * @tparam T
   * @return
   */


  def length[T](list: List[T]) = {
    @tailrec
    def loop(list: List[T], res: Int): Int = list match {
      case Nil => res
      case head :: tail => loop(tail, res+1) //increment the res count
    }

    loop(list, 0)
  }
  println(qlist)
  println(length(qlist))

  /** 5
   * Reverse a list
   * @param l
   * @tparam A
   * @return
   */

  def reverseList[A](l: List[A]): List[A] = l match {
    case Nil => Nil  //empty list
    case x :: Nil => List(x) //one element list
    case x :: xs => reverseList(xs) ::: List(x) /// recurse the tail and append to list of head
  }
  println(reverseList(qlist))

  /** 6
   *
   * check is palindrome
   * @param l
   * @tparam A
   * @return
   */

  def isPalindrome[A](l: List[A]): Boolean = l match {
    case Nil => true //empty list
    case x :: Nil => true // one element list
    case _ :: _ => l.head == l.last && isPalindrome(l.tail.init) //matchs head and tail then recurse the init
  } //IN----------------------T
  //H -TAI---------------------------L
  println(isPalindrome(List("M", "A", "L", "A", "Y", "A", "L", "A", "M")))

  /*** 7
   *
   * flatten the list
   * @param l
   * @return
   */

  def flatten(l: Any): List[Any] = l match {
    case Nil => Nil //empty list
    case x :: xs => flatten(x) ::: flatten(xs)
    case x => List(x) //only one list
  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

  /*** 8
   * flattern another way
   * @param list
   * @tparam T
   * @return
   */
  def flatten2[T](list: List[T]): List[T] = list match {
    case Nil => Nil      //empty condition for exit
    case head :: tail => (head match {
      case l: List[T] => flatten2(l)      // if head is a list recurse list
      case i => List(i)           //if not a list create a list with heaad
    }) ::: flatten2(tail) // append the head with recursed tail
  }

  println(flatten2(List(List(1, 1), 2, List(3, List(5, 8)))))



  /*** 9
   * Eliminate consecutive duplicates of list elements.
   *
   * @param list
   * @tparam T
   * @return
   */
  def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case h :: Nil => List(h)
    case x :: xs if (x == xs.head) => compress(xs)
    case x :: xs => x :: compress(xs) //:: it is taken as one item, which results in a nested structure.
                                      //:: prepends a single item whereas ::: prepends a complete list.

  }

  println(compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))

  //compress using foldRight
  def fcompress[A](l: List[A]): List[A] = l.foldRight(List[A]()) {
    case (e, ls) if (ls.isEmpty || ls.head != e) => e :: ls
    case (e, ls) => ls

  }

  println(fcompress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))

  /***10
   *
   * Remove duplicates from a list
   */
  def remove(xs: List[Int]):List[Int]= {
  def _remove(xs: List[Int], acc: List[Int] = Nil): List[Int] = xs match {
    case Nil => acc
    case h :: tail if (!acc.contains(h) )=> _remove(tail,h :: acc) // append head to accumulator
    case h :: tail if (acc.contains(h) )=> _remove( tail, acc) //recurese the accumulator til it find duplicate
  }
    _remove(xs,Nil)
   //case head :: tail if contains(head, result) => remove(tail, result)
 }

  println("duplicates removed"+ remove(List(1,1,2,3,4,4,6,9,9,9,0,0,0,0,0,8,4,4,4,4,4,4,4,3,3,3,3,8,4,4,66,66,6,5)))
  /** 11
   *
   * pack consecutive duplicates
   *
   * @param l
   * @tparam A
   * @return
   */
  def pack[A](l: List[A]): List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]): List[List[A]] = rem match {
      case Nil => res //accumulator list remain empty
      case x :: xs if (res.isEmpty || res.last.head != x) => _pack(res ::: List(List(x)), xs) //consecutive  elements are not same then populate
                                     // a new accumlator list with head and tail (List(x:xs),List(x:xs),..., this will
                                    //recusively continue until a consecutive matching item occur
      case x :: xs => _pack(res.init ::: List(res.last ::: List(x)), xs) // opposite condition to above take the remaining -init and last and recurse
    }
    _pack(List(), l)
  }
println("Consecutive element list" + pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))


  /** 12
   *
   * run length encoding
   *
   * @param xs
   * @tparam A
   * @return
   */
  def encode[A](xs: List[A]): List[(Int, A)] = xs match {
    case Nil => Nil
    case x :: xs => encode(xs) match { //now pattern mach the xs to iterate over all the list elements one by one
      //case (c, `x`) :: rest => (c + 1, `x`) :: rest
        //patern match a case class with List of ((counter , value )  elements, if value equal to head value then increment and append to rest of the list
      case (c, v) :: rest if v == x => (c + 1, x) :: rest
      case             rest => (    1, x) :: rest // counter reset for the next element in the parent list
    }
  }


  println ("runlength encode" + encode (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

  /**
   *
   *
   * Run length modified
   * @param xs
   * @tparam A
   * @return
   */
  def encodemodified[A](xs: List[A]) : List[ (Int,A) ]= xs match {
    case Nil => Nil
    case x :: xs => encodemodified(xs) match {
      case (c, v) :: rest if v == x => (c + 1, x) :: rest
      case             rest => (    1, x) :: rest // counter start

      //case (c, v) :: rest if c == 1 =>  List(x):: rest

    }
  }
  println ("run length encode" + encodemodified ( List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))


  def encodeFunctional[A](ls: List[A]) = ls.foldRight(List[Tuple2[Int, A]]())(
    (a, b) => b match {
      case x :: xs if (x._2 == a) => (x._1 + 1, a) :: xs
      case _       => (1, a) :: b
    })
  println ("runlength encode functional" + encodeFunctional ( List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
 val encodedval =encodeFunctional ( List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  import scala.collection.immutable.List._

  def repeat[A](e: (Int, A)) = fill(e._1)(e._2)
  def decode[A](rle: List[(Int, A)]) = rle flatMap repeat

  println( decode ( encodedval))

  /** 13
   * duplicate the elements of the list
   *
   *
   * @param l
   * @tparam A
   * @return
   */


  def duplicateE[A](l : List[A]): List[A] = l.flatMap( i =>  List(i,i))// create a new list with each element using flatMap function

  println(duplicateE(encodedval))

  /** 14
   * Duplicate the elements of a list a given number of times.
   *
   * @param i
   * @param l
   * @tparam A
   * @return
   */
  /*
  first flatmap the list then create a range list inside flatmap and map to each element in the range with individual value

   */

  def duplcateN[A](i: Int,l:List[A]):List[A] = l.flatMap(x=>(1 to i).map(_=>x)) //create a list 1 to i then map the list with x
   println(duplcateN ( 5, qlist))

  /** 15
   * Drop every nth element in a List
   *
   * @param n
   * @param l
   * @tparam A
   * @return
   */

  def dropRec[A](n: Int, l: List[A]):List[A] = {
    def _dropRec[A](c: Int, res: List[A], rem: List[A]):List[A] = (c, rem) match {
      case (_, Nil) => res // empty list
      case (1, _::tail) =>_dropRec(n, res, tail) // if it is the first element to be dropped, just return the tail
      case (_, h::tail) => _dropRec(c-1, res:::List(h),tail) // count reducer counter -1 , input list + head and accumlate tail
    }
    _dropRec(n, List(), l)
  }
  println(qlist)
  println(dropRec(4,qlist))

  /** 16
   *
   * Split list
   * @param n
   * @param l
   * @tparam A
   * @return
   */
  def split[A](n: Int, l: List[A]):(List[A], List[A]) = {
    def _split[A](c: Int, res: List[A], rem: List[A]):(List[A],List[A]) = (c, rem) match {
      case (_, Nil) => (res, Nil)//empty list
      case (0, rem) => (res, rem) //counter is at 0
      case (c, h::tail) => _split(c - 1, res:::(List(h)), tail) // keep extracting the head from rem appending it to
                                                                // res until the counter is 0
    }
    _split(n, List(), l)
  }

  println(split(3,qlist))

  /** 17
   *
   *
   * @param i
   * @param k
   * @param l
   * @tparam A
   * @return
   */
  def slice[A](i: Int, k: Int, l: List[A]):List[A] = {
    def _slice[A](cl: Int, cr: Int, rem: List[A]): List[A] = (cl, cr, rem) match { //startindex //end index /rem
      case (0, 0, rem) => rem
      case (0, cr, rem) => _slice(0, cr - 1, rem.init)
      case (cl, cr, rem) => _slice(cl - 1, cr, rem)

    }
    _slice(i,k-i,l)
  }

  println(slice(3,6, qlist))


  /***
   *
   * Remove kthe element
   *
   */

  def removeAtR[A](n: Int, l: List[A]):(List[A], A) = {
    def _removeAtR[A](k: Int, res: List[A], rem: List[A]):(List[A], A) = (k, rem) match {
      case (0, h::tail) => return (res:::tail, h)
      case (k, Nil) => throw new NoSuchElementException
      case (k, h::tail) => return _removeAtR(k - 1, res:::List(h), tail)
    }
    return _removeAtR(n, List(), l)
  }

  println(removeAtR(3,qlist))
}




