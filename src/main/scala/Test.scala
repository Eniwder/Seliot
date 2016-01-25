object Test extends App {

  val list = List('a, 'b, 'c, 'd, 'e, 'f)
  println(combinations(5, list).mkString("\n"))

  def flatMapSublists[A, B](ls: List[A])(f: List[A] => List[B]): List[B] = ls match {
    case Nil => Nil
    case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
  }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map { sl.head :: _ }
    }
  }

}