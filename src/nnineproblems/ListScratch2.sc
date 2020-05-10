def lastElement[A](l: List[A]): A = l match {
  case x::Nil => x
  case x:: xs => lastElement(xs)
}
val qlist=List(1,2,3,4,5,6,7)

println(lastElement(qlist))


def secondLast[A](l: List[A]): A = l match {
  case x::Nil => x
  case x::xs =>

}

/*
def findKth[A](k:Int, l:List[A]):A = (k,l) match {

}
def length[T](list: List[T]) = {

}
def reverseList[A](l: List[A]): List[A] = l match {

}
def isPalindrome[A](l: List[A]): Boolean = l match {

}

def flatten(l: Any): List[Any] = l match {

}

def compress[T](list: List[T]): List[T] = list match {


}

def pack[A](l: List[A]): List[List[A]] = {

}

def encode[A](xs: List[A]): List[(Int, A)] = xs match {


}

def dropRec[A](n: Int, l: List[A]):List[A] = {

}*/
