/*
 *  Name: Caeser Cipher
 * 
 *  Desc: Scala port of Graham Hutton's Programming in Haskell example
 *  Caeser Cipher
 */

import scala.math.pow

// Encoding and decoding
object ED {
  private def let2int ( c : Char ) : Int = {
    c.toByte - 'a'.toByte
  }

  private def int2let ( n : Int ) : Char = {
    ('a'.toByte + n.toByte).toChar
  }

  // scala's modulus (i.e. %) does not wrap for negative divides and so we handle it here
  // I think if we were using Java 8 floorMod, then it would meet our requirements.
  private def mod ( a : Int, b : Int) : Int = {
    (a % b + b) % b
  }

  private def shift ( n : Int, c : Char ) : Char = {
    if ( c.isLower )
      int2let ( mod ( (let2int(c) + n),  26 ) )
    else c
  }

  def encode ( n : Int, xs : String ) : String = {
    xs.map( x => shift(n,x) )
  }
}

// Frequency analysis
object FA {
  private val table : List[Double] =
    List(8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0,
      6.1, 7.0, 0.2, 0.8,  4.0, 2.4, 6.7,
      7.5, 1.9, 0.1, 6.0,  6.3, 9.1, 2.8,
      1.0, 2.4, 0.2, 2.0,  0.1)

  private def lowers ( xs : String ) : Int = {
    (for {
      x <- xs
      if x.isLower
    } yield (x)) length
  }

  private def count ( x : Char, xs : String) : Int = {
    (for {
      xx <- xs
      if x == xx
    } yield (xx)) length
  }

  private def precent ( n : Int, m : Int ) : Double = {
    (n.toDouble / m.toDouble) * 100.0
  }

  private def freqs ( xs : String ) : List[Double] = {
    for {
      x <- ('a' to 'z').toList
    } yield (precent (count(x,xs), lowers(xs)))
  }

  private def chisqr ( os : List[Double], es : List[Double] ) : Double = {
    (for {
      (o,e) <- os zip es
    } yield (pow((o - e), 2) / e)).sum
  }

  private def rotate[A] ( n : Int, xs : List[A] ) : List[A] = {
    List.concat(xs drop n, xs take n) 
  }

  private def positions[A] ( x : A, xs : List[A]) : List[Int] = {
    for {
      (xx, i) <- xs zip Stream.from(0)
      if x == xx
    }
    yield(i)
  }

  def crack ( cs : String ) : String = {
    val chitab = for {
      n <- (0 to 25).toList
    } yield (chisqr (rotate(n, freqs(cs)), table))

    val factor = positions(chitab.min, chitab) head

    ED.encode((-factor), cs)
  }
}

object CaeserCipher {
  private def usage () : Unit = {
    println("usage: -e -c \"message\"")
    println("   -e: encode message")
    println("   -c: crack message")
  }

  def main(args: Array[String]) {
    if (args.length == 2) {
      args(0) match {
        case "-e" => {
          println("Encoded \"" + args(1) + "\" as \"" + ED.encode(3, args(1)) + "\"");
        }
        case "-c" => {
          println("Cracked: \"" + args(1) + "\" as \"" + FA.crack(args(1)) + "\"");
        }
        case _    => {
          usage()
        }
      }
    }
    else {
      usage()
    }
  }
}
