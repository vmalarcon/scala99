object P01 {
  def lastBuiltIn[A](l: List[A]): A = l.last

  def lastRecursive[A](l: List[A]): A = l match {
    case h :: Nil  => h
    case _ :: tail => lastRecursive(tail)
    case _         => throw new NoSuchElementException
  }
}

object P02 {
  def penultimate[A](l: List[A]): A = l match {
    case h :: j :: Nil => h
    case _ :: tail     => penultimate(tail)
    case _             => throw new NoSuchElementException
  }
}

object P03 {
  // my implementation
  def nth[A](n: Int, l: List[A]): A = {
    if (n == 0) l.head
    else if (n > 0 && n < l.length) nth(n - 1, l.tail)
    else throw new NoSuchElementException
  }
  
  // Trivial with builtins.
  def nthBuiltin[A](n: Int, ls: List[A]): A =
    if (n >= 0) ls(n)
    else throw new NoSuchElementException

  // Not that much harder without.
  def nthRecursive[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (0, h :: _   ) => h
    case (n, _ :: tail) => nthRecursive(n - 1, tail)
    case (_, Nil      ) => throw new NoSuchElementException
  }
}
	
