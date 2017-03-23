object tester {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(123); 
  def product(f: Int => Int, x: Int, y: Int): Int = {
    if (x > y) 1 else f(x) * product(f, x + 1, y)
  };System.out.println("""product: (f: Int => Int, x: Int, y: Int)Int""");$skip(26); val res$0 = 

  product(x => x, 1, 4);System.out.println("""res0: Int = """ + $show(res$0));$skip(32); 
  
  val x = new rationals(1,3);System.out.println("""x  : rationals = """ + $show(x ));$skip(29); 
  val y = new rationals(5,7);System.out.println("""y  : rationals = """ + $show(y ));$skip(29); 
  val z = new rationals(3,2);System.out.println("""z  : rationals = """ + $show(z ));$skip(28); 
  val k = (x.sub(y).sub(z));System.out.println("""k  : rationals = """ + $show(k ));$skip(13); val res$1 = 
  k.toString;System.out.println("""res1: String = """ + $show(res$1))}
  
}

class rationals(x: Int, y: Int) {
  def numer = x
  def denom = y

  def neg:rationals= new rationals(-1*numer,denom)

	def add(that:rationals):rationals= new rationals(numer*that.denom+that.numer*denom, denom*that.denom)

  def sub(that: rationals): rationals = add(that.neg)
  
  override def toString = x + "/" + y
}
