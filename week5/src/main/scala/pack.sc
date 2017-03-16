/**
  * Created by alex on 3/15/17.
  */

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => List(x :: xs1.takeWhile(y => x == y)) ::: pack(xs1.dropWhile(z => x == z))
}

assert(pack(List("a", "a", "a", "b", "c", "c", "a")) == List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")))

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs).map(x => (x.head, x.length))

assert(encode(List("a", "a", "a", "b", "c", "c", "a")) == List(("a", 3), ("b", 1), ("c", 2), ("a", 1)))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, y) => f(x) :: y )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (x, y) => y + 1 )

mapFun[Int, Int](List(1, 2, 3), x => x + 5)

lengthFun(List(1, 2, 3, 4))
