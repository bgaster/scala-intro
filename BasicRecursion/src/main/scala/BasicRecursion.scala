/*
 *  Some simple examples introducing recursion
 * 
 *  Exercises:
 * 
 *   1. Implement the factorial function interm of foldList.
 */

package object BasicRecursion {
  import scala.annotation.tailrec

  // using loops
  def sumLoop ( is : List[Int] ) : Int = {
    var total = 0
    for (i <- is) {
      total = total + i
    }

    total
  }

  def productLoop ( is : List[Int] ) : Int = {
    var total = 1
    for (i <- is) {
      total = total * i
    }

    total
  }

  //using recursion 
  def sumRecursiveIf ( is : List[Int] ) : Int =
    if (is.length == 0)
      0
    else
      is.head + sumRecursiveIf(is.tail)

  //using recursion
  def sumRecursive ( is : List[Int] ) : Int = is match {
    case Nil            => 0
    case i :: remainder => i + sumRecursive(remainder)
  }

  def productRecursive ( is : List[Int] ) : Int = is match {
    case Nil            => 1
    case i :: remainder => i * productRecursive(remainder)
  }

  // hopefully at this point you've noticed a pattern:
  //    When Nil return the unit of the given operation (e.g. 0 for plus)
  //    When cons return result of applying the given operation to
  //    the value (at that position in list) and the result of a recursive call
  //    to the executing function. We can abstract this notion...

  def foldList ( f : (Int, Int) => Int, unit : Int, is : List[Int] ) : Int =
    is match {
    case Nil     => unit
    case i :: is => f(i, foldList(f, unit, is))
    }

  val sum = (xs : List[Int]) => foldList((x,y) => x + y,  0, xs)

  def fib( n : Int) : Int = n match {
   case 1 | 2 => n
   case _ => fib( n-1 ) + fib( n-2 )
  }
}


object Main {
  def main(args: Array[String]) {
    import BasicRecursion._

    val xs = List(1,2,3,4)

    println("sumLoop(xs) = " + sumLoop(xs))
    println("productLoop(xs) = " + productLoop(xs))
    println("sumRecursive(xs) = " + sumRecursive(xs))
    println("sumRecursiveIf(xs) = " + sumRecursiveIf(xs))
    println("prodRecursive(xs) = " + productRecursive(xs))

    println("foldInt(sum)  = " + foldList((x,y) => x + y,  0, xs))
    println("foldInt(prod) = " + foldList((x,y) => x * y,  1, xs))

    println("fib(5) = " + fib(5))

  }
}
