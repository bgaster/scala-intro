/*
 *  Name: Transmitter
 * 
 *  Desc: Scala port of Graham Hutton's Programming in Haskell example
 *  transmit.
 */

object C {
  type Bit = Int

  def bin2Int ( bs : List[Int] ) : Int = {
    bs.foldRight(0) { ( x : Int, y : Int ) => x + 2*y }
  }

  def int2Bin ( n : Int ) : List[Int] = n match {
    case 0 => Nil
    case nn => (nn % 2) :: int2Bin(nn / 2 )
  }
}

object T {
  // we don't do this lazyly as in Hutton's original example as 8 cons do not seem worth it
  def make8( bits :  List[C.Bit] ) : List[C.Bit] = {
    (bits ++ List(0,0,0,0,0,0,0,0)) take 8
  }

  def encode ( xs : String ) : List[C.Bit] = {
    xs.map(make8 _ compose C.int2Bin _ compose ((x) => x.toByte)).toList flatten
  }

  def chop8 ( bits : List[C.Bit] ) : List[List[C.Bit]] = bits match {
    case Nil => Nil
    case bs  => (bits take 8) :: chop8 (bits drop 8)
  }

  def decode ( bits : List[C.Bit] ) : String = {
    chop8(bits).foldRight("") { (b, s) => C.bin2Int(b).toByte.toChar.toString ++ s }
  }
}

object Transmitter {
  private def usage () : Unit = {
    println("usage: \"message\"")
  }

  def main(args: Array[String]) {
    if (args.length == 1) {
      val t = T.encode(args(0))
      println("Transmit \"" + args(0) + "\" as \"" + t + "\"")
      println("Received \"" + args(0) + "\" as \"" + T.decode(t) + "\"")
    }
    else {
      usage()
    }
  }
}
