package redbook.objectoriented

object CollectionApp  extends App{

  val list = List(1,2,3)

  println(list.head)
  println(list.tail)
  println(list.map(_+1))


  val number =List(1,2,3)
  val chars =List('a','b','c','d')

  val combinations = number.flatMap(n=> chars.map(c=>""+c+n))

}
