package chapter02

sealed trait List[+A] {

  def isEmpty: Boolean = this match {
    case Nil => true
    case _   => false
  }

  def addHead[B >: A](h: B): List[B] = Cons(h, this)

  def head: Option[A] = this match {
    case Nil        => None
    case Cons(h, _) => Some(h)
  }

  def tail: Option[List[A]] = this match {
    case Nil        => None
    case Cons(_, t) => Some(t)
  }

  def append[B >: A](l: List[B]): List[B] = this match {
    case Nil        => l
    case Cons(h, t) => Cons(h, t.append(l))
  }

  def update[B >: A](i: Int, y: B): Option[List[B]] = this match {
    case Nil                    => None
    case _ if (i < 0)           => None
    case Cons(_, t) if (i == 0) => Some(Cons(y, t))
    case Cons(h, t)             => t.update(i - 1, y) match {
      case None                 => None
      case Some(l)              => Some(Cons(h, l))
    }
  }

  def suffixes: List[List[A]] = this match {
    case Nil => Nil
    case Cons(h, t) => t.suffixes.addHead(Cons(h, t))
  }


}

case object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {

  def empty[A]: List[A] = Nil

  def cons[A](x: A, s: List[A]) = Cons(x, s)


}

object ListApp {

  def main(args: Array[String]): Unit = {

    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    println(l)

    println(l.suffixes)

  }

}