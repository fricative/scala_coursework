object tester {
  def product(f: Int => Int, x: Int, y: Int): Int = {
    if (x > y) 1 else f(x) * product(f, x + 1, y)
  }                                               //> product: (f: Int => Int, x: Int, y: Int)Int

  product(x => x, 1, 4)                           //> res0: Int = 24
  
  val x = new rationals(1,3)                      //> x  : rationals = 1/3
  val y = new rationals(5,7)                      //> y  : rationals = 5/7
  val z = new rationals(3,2)                      //> z  : rationals = 3/2
  val k = (x.sub(y).sub(z))                       //> k  : rationals = -79/42
  k.toString                                      //> res1: String = -79/42
  
}

class rationals(x: Int, y: Int) {
  def numer = x
  def denom = y

  def neg:rationals= new rationals(-1*numer,denom)

	def add(that:rationals):rationals= new rationals(numer*that.denom+that.numer*denom, denom*that.denom)

  def sub(that: rationals): rationals = add(that.neg)
  
  override def toString = x + "/" + y
}