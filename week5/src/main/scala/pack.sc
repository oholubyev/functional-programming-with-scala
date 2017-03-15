/**
  * Created by alex on 3/15/17.
  */

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => List(x :: xs1.takeWhile(y => x == y)) ::: pack(xs1.dropWhile(z => x == z))
}

assert(pack(List("a", "a", "a", "b", "c", "c", "a")) == List(List("a", "a", "a"), List("b"), List("c", "c"), List("a")))