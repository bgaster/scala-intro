/*
 * Simple BinaryTree (Int) Implementation
 *  Search Trees
 *  In order
 *  Pre order
 *  Post order
 *  Level order
 * 
 */
package object BinaryTree {
  import scala.collection.mutable.Queue
  
  trait Tree {
    def toListInOrder : List[Int] = this match {
      case Empty()     => Nil
      case Leaf(v)     => v :: Nil
      case Node(l,v,r) => l.toListInOrder ::: (v :: r.toListInOrder)
    }

    def toListPreOrder : List[Int] = this match {
      case Empty()     => Nil
      case Leaf(v)     => v :: Nil
      case Node(l,v,r) => v :: l.toListPreOrder ::: r.toListPreOrder
    }

    def toListPostOrder : List[Int] = this match {
      case Empty()     => Nil
      case Leaf(v)     => v :: Nil
      case Node(l,v,r) => l.toListPreOrder ::: r.toListPreOrder ++ (v :: Nil)
    }

    // Use the standard Haskell implementation of level-order
    def toListLevelOrder : List[Int] = {
      def elements (t : Tree ) : List[Int] = t match {
        case Empty()     => Nil
        case Leaf(v)     => v :: Nil
        case Node(l,v,r) => v :: Nil
      }

      def subTrees( t : Tree ) : List[Tree] = t match {
        case Node(l,_,r) => List(l,r)
        case _ => Nil
      }

      def step (ls : List[Tree]) : List[Int] = ls match {
        case Nil => Nil
        case ts  => ts.flatMap(elements) ::: step (ts.flatMap(subTrees))
      }

      step (this :: Nil)
    }

    // insert into an ordered (i.e. binary search tree) tree
    def insertOrdered( i : Int ) : Tree = this match {
      case Empty() => Leaf(i)
      case Leaf(v) =>
        if (i < v)
          Node(Leaf(i), v, Empty())
        else if(i > v) 
          Node(Empty(), v, Leaf(i))
        else
          this
      case Node(l,v,r) =>
        if (i < v)
          Node(l.insertOrdered(i), v, r)
        else if (i > v)
          Node(l, v, r.insertOrdered(i))
        else
          this
    }

    // assumes a binary search tree
    def find( i : Int ) : Boolean = this match {
      case Empty()     => false
      case Leaf(v)     => i == v
      case Node(l,v,r) =>
        if (i < v)
          l.find(i)
        else if (i > v)
          r.find(i)
        else
          true
    }
  }

  case class Leaf(v : Int) extends Tree
  case class Node(l : Tree, v : Int, r : Tree) extends Tree
  case class Empty() extends Tree

  def toTree ( ls : List[Int] ) : Tree =
    ls.foldRight(Empty() : Tree) ( (x, t) => t.insertOrdered(x) )

} // package BinaryTree

object Main {
  def main(args: Array[String]) {
    import BinaryTree._

    val t = BinaryTree.toTree(List(1,4,6,2,4))

    println("The tree: " + t.toString)

    println("In order: " + t.toListInOrder.toString)
    println("Pre order: " + t.toListPreOrder.toString)
    println("Post order: " + t.toListPostOrder.toString)
    println("Level order: " + t.toListLevelOrder.toString)
  }
}
