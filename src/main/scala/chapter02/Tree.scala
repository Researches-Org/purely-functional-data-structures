package chapter02

sealed trait Tree[+A] {

  def member[B >: A](x: B)(implicit ord: Ordering[B]): Boolean = {
    import ord.mkOrderingOps

    this match {
      case Empty                    => false
      case Node(y, l, _) if (x < y) => l.member(x)
      case Node(y, _, r) if (x > y) => r.member(x)
      case _                        => true
    }
  }

  def memberWithLessComparison[B >: A](x: B)(implicit ord: Ordering[B]): Boolean = {
    import ord.mkOrderingOps

    def go(t: Tree[B], e: B): Boolean = t match {
      case Empty                    => e == x
      case Node(y, l, _) if (x < y) => go(l, y)
      case Node(y, _, r)            => go(r, y)
    }

    this match {
      case Empty     => false
      case Node(v, _, _) => go(this, v)
    }
  }

  def insert[B >: A](x: B)(implicit ord: Ordering[B]): Tree[B] = {
    import ord.mkOrderingOps

    this match {
      case Empty                    => Node(x, Empty, Empty)
      case Node(y, l, r) if (x < y) => Node(y, l.insert(x), r)
      case Node(y, l, r) if (x > y) => Node(y, l, r.insert(x))
      case _                        => this
    }
  }


}

case object Empty extends Tree[Nothing]
case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * The depth of a node is the number of edges from the node to the tree's root node.
    * A root node will have a depth of 0.
    * The height of a node is the number of edges on the longest path from the node to a leaf.
    * A leaf node will have a height of 0.
    */

  def complete[A](x: A, d: Int) : Tree[A] = {

    def go(depth: Int): Tree[A] = {
      if (depth == 0) Node(x, Empty, Empty)
      else {
        val shared = go(depth - 1)
        val root = Node(x, shared, shared)
        root
      }
    }

    go(d)
  }
}

import Tree._

object TreeApp {

  def main(args: Array[String]): Unit = {
    val tree = Node(10, Empty, Empty)

    println(tree)

    val tree2 = tree.insert(10)

    println(tree2)

    val tree3 = tree2.insert(15)

    println(tree3)

    val tree4 = tree3.insert(5)

    println(tree4)

    println(s"10 is in ${tree}: ${tree.member(10)}")

    println(s"5 is in ${tree}: ${tree.member(5)}")

    println(s"5 is in ${tree4}: ${tree4.member(5)}")

    println(complete(1, 0))

    println(complete(1, 1))

    println(complete(1, 2))
  }

}