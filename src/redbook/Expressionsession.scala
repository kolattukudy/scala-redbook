package redbook

object Expressionsession  extends App {

  val x = 1 + 2

  def greeting_function(age: Int, name: String) {

    println("My age name " + name + " and my age is " + age)

  }

  greeting_function(24, "David")


  def factorial(n: Int): Int =
    if (n <= 0 )1
    else n *factorial(n-1)
  println(factorial(4))


  def febnacci( n: Int): Int =
    if (n<=2)1
    else febnacci(n-1)+ febnacci(n-2)

  println(febnacci(8))


  def isPrime (n: Int): Boolean={
    def isPrimeUntil(t: Int): Boolean=
      if (t <=1) true
      else  n% t !=0  &&  isPrimeUntil(t-1)
     isPrimeUntil(n/2)
  }
}