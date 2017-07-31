package chapter02

sealed trait Tree[+A] {

  @annotation.tailrec
  final def member[B >: A](x: B)(implicit ord: Ordering[B]): Boolean = {
    import ord.mkOrderingOps

    this match {
      case Empty                    => false
      case Node(y, l, _) if (x < y) => l.member(x)
      case Node(y, _, r) if (x > y) => r.member(x)
      case _                        => true
    }
  }

  def member2_2[B >: A](x: B)(implicit ord: Ordering[B]): Boolean = {
    import ord.mkOrderingOps

    @annotation.tailrec
    def go(t: Tree[B], e: B): Boolean = t match {
      case Empty                    => e == x
      case Node(y, l, _) if (x < y) => go(l, e)
      case Node(y, _, r)            => go(r, y)
    }

    this match {
      case Empty         => false
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

  def insert2_3[B >: A](x: B)(implicit ord: Ordering[B]): Tree[B] = {

    if (member2_2(x))
      this
    else
      insert(x)
  }

  def insert2_4[B >: A](x: B)(implicit ord: Ordering[B]): Tree[B] = {
    import ord.mkOrderingOps

    def go(t: Tree[B], e: B): (Boolean, Tree[B]) = t match {
      case Empty if (x == e)            => (true, Empty)
      case Empty                        => (false, Node(x, Empty, Empty))
      case n @ Node(y, l, _) if (x < y) => goAux(n, l, e, true)
      case n @ Node(y, _, r)            => goAux(n, r, y, false)
    }

    def goAux(t: Node[B], c: Tree[B], e: B, left: Boolean): (Boolean, Tree[B]) = {
      val result = go(c, e)
      if (!result._1) {
        val tmp = if (left) (result._2, t.right) else (t.left, result._2)
        (false, Node(t.value, tmp._1, tmp._2))
      } else
        (true, t)
    }

    this match {
      case Empty => Node(x, Empty, Empty)
      case Node(y, _, _) => go(this, y)._2
    }
  }


}

case object Empty extends Tree[Nothing]
case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

import util.Math

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

  /**
    * Extend this function to create balanced trees of arbitrary size. These trees
    * will not always be complete binary trees, but should be as balanced as
    * possible: for any given node, the two subtrees should differ in size by at
    * most one. This function should run in 0(log n) time. (Hint: use a helper
    * function create2 that, given a size m, creates a pair of trees, one of size m
    * and one of size m+1.)
    *
    * N - number of nodes.
    * H - height of the binary tree.
    * Complete Binary Tree:
    * Then, with H height N would lie between:
    * 2^H <= N <= (2^(H+1) - 1)
    * Thus, solving this inequality; we get :
    * H <= lg(N)  and  H >= (lg(N+1) - 1)
    * Hence we finally get:
    * H = floor( lg(N) ) = ceil( (lg(N+1) - 1) )   //as H is integer
    * (lg : log base 2)
    *
    * examples n = 2 => l = 0 and r = 1
    *          n = 3 => l = 1 and r = 1
    *          n = 4 => l = 1 and r = 2
    *          n = 5 => l = 2 and r = 2
    *          n = 6 => l = 2 and r = 3
    *          when n is even, nodes on the left (n - 1)/2 and on the right = left + 1
    *          when n is odd, nodes on the left (n - 1)/2 and on the right = left
    *
    */
  def balanced[A](x: A, n: Int): Tree[A] = {

    def create2(n: Int, p: Int): Tree[A] = {
      if (p < n) {
        val left = create2(n, 2 * p + 1)
        val right = create2(n, 2 * p + 2)
        Node(x, left, right)
      } else
        Empty
    }

    if (n <= 0)
      null
    else if (n == 1)
      Node(x, Empty, Empty)
    else {
      val l = (n - 1) / 2
      val r = if (n % 2 == 0) l + 1 else l

      val left = create2(l, 0)
      val right = create2(r, 0)

      Node(x, left, right)
    }
  }

  def height(n: Int): Int = math.floor(Math.log2(n)).toInt
}

import Tree._


object TreeApp {

  def main(args: Array[String]): Unit = {
    println(balanced(1, 1))
    println(balanced(1, 2))
    println(balanced(1, 3))
    println(balanced(1, 4))
    println(balanced(1, 5))
    println(balanced(1, 6))
    println(balanced(1, 7))
    println(balanced(1, 8))
  }

}