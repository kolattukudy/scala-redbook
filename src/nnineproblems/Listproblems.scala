package nnineproblems


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


  /**
   *
   * Second last item from the list
   *
   * @param l
   * @tparam A
   * @return second last itme
   */
  def secondLast[A](l: List[A]): A = l match {

    case x :: List(xs) => x

    case x :: xs => secondLast(xs)
  }

  println(secondLast(qlist))

  /**
   *
   * Find the Kth element at a list
   */


  def kthelement[A](k: Int, l: List[A]): A = k match {

    case 0 => l.head
    case _ => kthelement(k - 1, l.tail)

  }

  def findKth[A](k: Int, l: List[A]): A = (k, l) match {

    case (0, h :: _) => h
    case (k, _ :: tail) if k > 0 => findKth(k - 1, tail)
    case _ => throw new NoSuchElementException
  }

  println(kthelement(0, qlist))

  println(findKth(0, qlist))


  def lengthList[A](l: List[A]): Int = l match {
    case Nil => 0
    case x :: xs => 1 + lengthList(xs)
  }

  println(lengthList(qlist))

  def reverseList[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: Nil => List(x)
    case x :: xs => reverseList(xs) ::: List(x)

  }

  println(reverseList(qlist))

  def isPalindrome[A](l: List[A]): Boolean = l match {
    case Nil => true //empty list
    case x :: Nil => true // one element list
    case _ :: _ => l.head == l.last && isPalindrome(l.tail.init) //matchs head and tail
  } //IN----------------------T
  //H -TAI---------------------------L
  println(isPalindrome(List("M", "A", "L", "A", "Y", "A", "L", "A", "M")))

  def flatten(l: Any): List[Any] = l match {
    case Nil => Nil //empty list
    case x :: xs => flatten(x) ::: flatten(xs)
    case x => List(x) //only one list
  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

  def flatten2[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: tail => (head match {
      case l: List[T] => flatten2(l)
      case i => List(i)
    }) ::: flatten2(tail)
  }

  println(flatten2(List(List(1, 1), 2, List(3, List(5, 8)))))


  //Eliminate consecutive duplicates of list elements.
  def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case h :: Nil => List(h)
    case x :: xs if (x == xs.head) => compress(xs)
    case x :: xs => x :: compress(xs)

  }

  println(compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))

  //compress using foldRight
  def fcompress[A](l: List[A]): List[A] = l.foldRight(List[A]()) {
    case (e, ls) if (ls.isEmpty || ls.head != e) => e :: ls
    case (e, ls) => ls

  }

  println(fcompress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))


  def pack[A](l: List[A]): List[List[A]] = {
    def _pack(res: List[List[A]], rem: List[A]): List[List[A]] = rem match {
      case Nil => res //accumulator list remain empty
      case x :: xs if (res.isEmpty || res.last.head != x) => _pack(res ::: List(List(x)), xs) //consecutive  elements are not same then populate a new accumlator list with head and tail
      case x :: xs => _pack(res.init ::: List(res.last ::: List(x)), xs) // opposite condition to above take the remaining -init and last and recurse

    }

    _pack(List(), l)

  }
println("Consecutive element list" + pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))


  //run length encoding

  def encode[A](xs: List[A]): List[(Int, A)] = xs match {
    case Nil => Nil
    case x :: xs => encode(xs) match {

      //case (c, `x`) :: rest => (c + 1, `x`) :: rest
      case (c, v) :: rest if v == x => (c + 1, x) :: rest
      case             rest => (    1, x) :: rest
    }
  }


  println ("runlength encode" + encode (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

