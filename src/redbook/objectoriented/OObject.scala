package redbook.objectoriented

object OObject  extends App{

  class writer (sirname: String ,firstname :String, val year: Int) {

    def fullName(sirname: String, firstname: String): String ={

      val fullname= sirname + "" + firstname
      fullname
    }


  }

  class Novel(name :String, yor: Int, author: writer) {
    def authorAge = yor - author.year
    def isWritenBy (author: writer) = author == this.author
    def copy (newYear: Int): Novel = new Novel ( name, newYear, author)
  }




  val author = new writer("Sir"  ,  "Charles Dick",1897)
   val novel = new Novel ( "Boo", 1956, author)


}
