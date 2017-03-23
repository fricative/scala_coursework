object tester {
  def product(f: Int => Int, x: Int, y: Int): Int = {
    if (x > y) 1 else f(x) * product(f, x + 1, y)
  }

  product(x => x, 1, 4)
}

class rationals(x: Int, y: Int) {
  def numer = x
  def denom = y

  def neg = -1 * x / y

  def sub(that: rationals): rationals =
    new rationals(numer * (that.denom) - (that.numer) * demon, denom * that.demon)

  override def toString = x + "/" + y

}