package forcomp

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val x  = ("hi","world")                         //> x  : (String, String) = (hi,world)
  val y = ("what","is")                           //> y  : (String, String) = (what,is)
   val sentences = List(List("Here","is","sentence","one","."), List("The","third","sentence","is","empty","!"), List(),List("Lastly",",","we","have","a","final","sentence","."))
                                                  //> sentences  : List[List[String]] = List(List(Here, is, sentence, one, .), Lis
                                                  //| t(The, third, sentence, is, empty, !), List(), List(Lastly, ,, we, have, a, 
                                                  //| final, sentence, .))
  sentences                                       //> res0: List[List[String]] = List(List(Here, is, sentence, one, .), List(The, 
                                                  //| third, sentence, is, empty, !), List(), List(Lastly, ,, we, have, a, final, 
                                                  //| sentence, .))
  sentences.flatten.mkString("")                  //> res1: String = Hereissentenceone.Thethirdsentenceisempty!Lastly,wehaveafinal
                                                  //| sentence.
  Set(Set(1,2),Set(22,1))==Set(Set(2,1),Set(1,22))//> res2: Boolean = true
  'l' > 'v'                                       //> res3: Boolean(false) = false
  val a = "X"                                     //> a  : String = X
  val b = "X"                                     //> b  : String = X
  a.equals(b)                                     //> res4: Boolean = true
}