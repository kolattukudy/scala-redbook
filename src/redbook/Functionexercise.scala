package redbook

object Functionexercise extends App{

  def concattailrec(a: String, n : Int , accumulator:String): String =
    if (n <= 0) accumulator
  else
      concattailrec(a, n-1, a+ accumulator)
  print(concattailrec("hello",3, ""))



}
