package forcomp

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(74); 
  println("Welcome to the Scala worksheet");$skip(26); 
  val x  = ("hi","world");System.out.println("""x  : (String, String) = """ + $show(x ));$skip(24); 
  val y = ("what","is");System.out.println("""y  : (String, String) = """ + $show(y ));$skip(179); 
   val sentences = List(List("Here","is","sentence","one","."), List("The","third","sentence","is","empty","!"), List(),List("Lastly",",","we","have","a","final","sentence","."));System.out.println("""sentences  : List[List[String]] = """ + $show(sentences ));$skip(12); val res$0 = 
  sentences;System.out.println("""res0: List[List[String]] = """ + $show(res$0));$skip(33); val res$1 = 
  sentences.flatten.mkString("");System.out.println("""res1: String = """ + $show(res$1));$skip(51); val res$2 = 
  Set(Set(1,2),Set(22,1))==Set(Set(2,1),Set(1,22));System.out.println("""res2: Boolean = """ + $show(res$2));$skip(12); val res$3 = 
  'l' > 'v';System.out.println("""res3: Boolean(false) = """ + $show(res$3));$skip(14); 
  val a = "X";System.out.println("""a  : String = """ + $show(a ));$skip(14); 
  val b = "X";System.out.println("""b  : String = """ + $show(b ));$skip(14); val res$4 = 
  a.equals(b);System.out.println("""res4: Boolean = """ + $show(res$4))}
}
