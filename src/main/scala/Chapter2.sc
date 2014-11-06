/**
 * Exercise 1
 *
 * Write a recursive function to get the nth Fibonacci
 * number.
 *
 * The first Fibonacci numbers are 0 and 1. The nth number is
 * always the sum of the previous two. Your definition should
 * be a local tail-recursive function.
 */

def fib(n: Int): Int = {
  def go(n: Int, acc: Int, prev: Int): Int = {
    if (n < 2) n - 1
    else if (n==2) acc
    else go(n-1, acc + prev, acc)
  }

  go(n, 1, 0)
}

fib(4)

/**
 * Implement isSorted, which checks whether an Array[A] is
 * sorted according to a given comparison function.
 *
 * def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
 */

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  val as1 = as.slice(0, as.length - 1)
  val as2 = as.slice(1, as.length)

  as1.zip(as2).foldRight(true)((a: (A, A), b: Boolean) => ordered.tupled(a) && b)
}


println(isSorted(Array("hola", "hello" , "hund"), (x: String, y: String) => x(0) == y(0)))

/**
 * Currying functions
 *
 * Converts a function f of two arguments into a function of one argument that
 * partially applies f.
 *
 */

def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
  (a: A) => ((b: B) => f(a,b))
}

val f = (x: Int, y: Int) => x + y

val curry_f = curry(f)

println(curry_f(5)(6))

/**
 * Implemente uncurrying
 */

def uncurry[A,B,C](f: A => (B => C)): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

val f2 = (x: Int) => (y: Int) => x + y

val uncurry_f2 = uncurry(f2)

println(uncurry_f2(5,6))

/**
 * Implemente a HOF that composes two functions
 */
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  (a: A) => f(g(a))
}

val fa = (x: Int) => x + 2
val fb = (x: Int) => x * 3

compose(fb, fa)(2)
