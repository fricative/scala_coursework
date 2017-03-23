object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(35); 
  val x = Vector(1,2);System.out.println("""x  : scala.collection.immutable.Vector[Int] = """ + $show(x ));$skip(11); val res$0 = 
  x.length;System.out.println("""res0: Int = """ + $show(res$0))}
}
