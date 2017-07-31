package chapter02

sealed trait FiniteMap[+A, +B] {

  def empty: FiniteMap[A, B] = EmptyMap

  def bind[C >: A, D >: B](key: C, value: D) : FiniteMap[C, D] = this match {
    case EmptyMap                   => Map(key, value, EmptyMap)
    case Map(k, v, m) if (k != key) => Map(k, v, m.bind(key, value))
    case Map(k, _, m) if (k == key) => Map(k, value, m)
  }

  def lookup[C >: A](key: C): Option[B] = this match {
    case EmptyMap                 => None
    case Map(a, b, _) if a == key => Some(b)
    case Map(_, _, m)             => m.lookup(key)
  }

}

case object EmptyMap extends FiniteMap[Nothing, Nothing]

case class Map[+A, +B](key: A, value: B, map: FiniteMap[A, B]) extends FiniteMap[A, B]
