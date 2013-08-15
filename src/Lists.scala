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

object P04 {
  def length[A](l: List[A]): Int = l match {
    case Nil       => 0
    case h :: tail => 1 + length(tail)
  }
}

object P05 {
  def reverse[A](l: List[A]): List[A] = {
    def reverseInternal(l: List[A], res: List[A]): List[A] = l match {
      case Nil       => res
      case h :: tail => reverseInternal(tail, h :: res)
    }
    reverseInternal(l, Nil)
  }
  
  // Builtin.
  def reverseBuiltin[A](ls: List[A]): List[A] = ls.reverse

  // Simple recursive.  O(n^2)
  def reverseRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil       => Nil
    case h :: tail => reverseRecursive(tail) ::: List(h)
  }

  // Tail recursive.
  def reverseTailRecursive[A](ls: List[A]): List[A] = {
    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil       => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }

  // Pure functional
  def reverseFunctional[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) { (r, h) => h :: r }
}

object P06 {
    def isPalindrome[A](l: List[A]): Boolean = l == l.reverse
    
    // Not any better
    def isPalindromeR[A](l: List[A]): Boolean = {
        def isPalinRecursive(l: List[A], r: List[A]): Boolean = (l, r) match {
            case (Nil, Nil)           => true
            case (Nil, _)             => false
            case (_, Nil)             => false
            case (hl :: htail, rList) => hl == rList.last && isPalinRecursive(htail, rList.dropRight(1)) 
        }
        isPalinRecursive(l, l)
    }
}
	
object P07 {
    def flatten(l: List[Any]): List[Any] = l flatMap {
        case ms: List[_] => flatten(ms)
        case e           => List(e)
    }
}

object P08 {
    // my version
    def compress[A](l: List[A]): List[A] = {
        def compressInternal(l: List[A], result: List[A]): List[A] = {
            if (l.isEmpty) return result
            
            if (result.isEmpty || l.head != result.head) compressInternal(l.tail, l.head :: result)
            else compressInternal(l.tail, result)
        }
        
        compressInternal(l, Nil).reverse
    }
    
    // Standard recursive.
    def compressRecursive[A](ls: List[A]): List[A] = ls match {
        case Nil       => Nil
        case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
    }

    // Tail recursive.
    def compressTailRecursive[A](ls: List[A]): List[A] = {
        def compressR(result: List[A], curList: List[A]): List[A] = curList match {
            case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
            case Nil       => result.reverse
        }
        compressR(Nil, ls)
    }

    // Functional.
    def compressFunctional[A](ls: List[A]): List[A] =
        ls.foldRight(List[A]()) { (h, r) =>
            if (r.isEmpty || r.head != h) h :: r
            else r
    }
    
    def compressFunc[A](l: List[A]): List[A] = 
        l.foldRight(List[A]()) { (h, r) =>
            if (r.isEmpty || r.head != h) h :: r
            else r
        }
}

object P09 {
    def pack[A](l: List[A]): List[List[A]] = l match {
        case Nil       => Nil
        case h :: tail => List(l.takeWhile(_ == h)) ::: pack(l.dropWhile(_ == h))
    }
}

object P10 {
    def encode[A](l: List[A]): List[(Int, A)] = {
        P09.pack(l).map { e => (e.length, e.head) }
    }
}

object P11 {
    def encodeModified(l: List[_]): List[_] = {
        P10.encode(l).map { e => if (e._1 == 1) e._2 else e }
    }
    
    // Just for fun, here's a more typesafe version.
    def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
        P10.encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }
}

object P12 {
    def decode[A](e: List[(Int, A)]): List[A] = e flatMap { el => List.fill(el._1)(el._2) }
}

object P13 {
    def encodeDirect[A](l: List[A]): List[(Int, A)] = l match {
        case Nil       => Nil
        case h :: tail => (l.takeWhile(_ == h).length, h) :: encodeDirect(l.dropWhile(_ == h))
    }
}

object P14 {
    def duplicate[A](l: List[A]): List[A] = l flatMap { e => List(e, e) }
}

object P15 {
    def duplicateN[A](n: Int, l: List[A]): List[A] = l flatMap { List.fill(n)(_) } 
}

object P16 {
    def drop[A](n: Int, l: List[A]): List[A] = l.zipWithIndex.filterNot { f => (f._2 + 1) % n == 0 }.map(_._1)

	def dropRecursive[A](n: Int, l: List[A]): List[A] = {
		def dropInternal(c: Int, li: List[A]): List[A] = (c, li) match {
			case (1, h :: tail) => dropInternal(n, tail)
			case (_, h :: tail) => h :: dropInternal(c - 1, tail)
			case (_, Nil)       => Nil
		}
		dropInternal(n, l)
	}
}

object P17 {
	def split[A](n: Int, l: List[A]): (List[A], List[A]) = l.splitAt(n)	
	
	// recursive solution
	def split2[A](n: Int, l: List[A]): (List[A], List[A]) = (n, l) match {
		case (_, Nil)  => (Nil, Nil)
		case (0, list) => (Nil, list)
		case (n, h :: tail) => {
			val (pre, post) = split2(n - 1, tail)
			(h :: pre, post)
		}
	}
}

object P18 {
	def slice[A](i: Int, k: Int, l: List[A]): List[A] = l.slice(i, k)
	
	def slice2[A](i: Int, k: Int, l: List[A]): List[A] = (i, k, l) match {
		case (_, _, Nil)       => Nil
		case (0, 0, list)      => Nil
		case (0, n, h :: tail) => h :: slice2(0, n - 1, tail)
        case (n, m, h :: tail) => slice2(n - 1, m - 1, tail)
	}

	// Simple recursive.
	def sliceRecursive[A](start: Int, end: Int, ls: List[A]): List[A] =
		(start, end, ls) match {
		  case (_, _, Nil)                 => Nil
		  case (_, e, _)         if e <= 0 => Nil
		  case (s, e, h :: tail) if s <= 0 => h :: sliceRecursive(0, e - 1, tail)
		  case (s, e, h :: tail)           => sliceRecursive(s - 1, e - 1, tail)
		}

	// Tail recursive, using pattern matching.
	def sliceTailRecursive[A](start: Int, end: Int, ls: List[A]): List[A] = {
		def sliceR(count: Int, curList: List[A], result: List[A]): List[A] =
		  (count, curList) match {
			case (_, Nil)                     => result.reverse
			case (c, h :: tail) if end <= c   => result.reverse
			case (c, h :: tail) if start <= c => sliceR(c + 1, tail, h :: result)
			case (c, _ :: tail)               => sliceR(c + 1, tail, result)
		  }
		sliceR(0, ls, Nil)
	  }

	// Since several of the patterns are similar, we can condense the tail recursive
	// solution a little.
	def sliceTailRecursive2[A](start: Int, end: Int, ls: List[A]): List[A] = {
		def sliceR(count: Int, curList: List[A], result: List[A]): List[A] = {
		  if (curList.isEmpty || count >= end) result.reverse
		  else sliceR(count + 1, curList.tail,
					  if (count >= start) curList.head :: result
					  else result)
		}
		sliceR(0, ls, Nil)
	}

	// Functional.
	def sliceFunctional[A](s: Int, e: Int, ls: List[A]): List[A] =
		ls drop s take (e - (s max 0))
}

object P17 {
	def split[A](n: Int, l: List[A]): (List[A], List[A]) = l.splitAt(n)	
	
	// recursive solution
	def split2[A](n: Int, l: List[A]): (List[A], List[A]) = (n, l) match {
		case (_, Nil)  => (Nil, Nil)
		case (0, list) => (Nil, list)
		case (n, h :: tail) => {
			val (pre, post) = split2(n - 1, tail)
			(h :: pre, post)
		}
	}
}

object P18 {
	def slice[A](i: Int, k: Int, l: List[A]): List[A] = l.slice(i, k)
	
	def slice2[A](i: Int, k: Int, l: List[A]): List[A] = (i, k, l) match {
		case (_, _, Nil)       => Nil
		case (0, 0, list)      => Nil
		case (0, n, h :: tail) => h :: slice2(0, n - 1, tail)
        case (n, m, h :: tail) => slice2(n - 1, m - 1, tail)
	}

	// Simple recursive.
	def sliceRecursive[A](start: Int, end: Int, ls: List[A]): List[A] =
		(start, end, ls) match {
		  case (_, _, Nil)                 => Nil
		  case (_, e, _)         if e <= 0 => Nil
		  case (s, e, h :: tail) if s <= 0 => h :: sliceRecursive(0, e - 1, tail)
		  case (s, e, h :: tail)           => sliceRecursive(s - 1, e - 1, tail)
		}

	// Tail recursive, using pattern matching.
	def sliceTailRecursive[A](start: Int, end: Int, ls: List[A]): List[A] = {
		def sliceR(count: Int, curList: List[A], result: List[A]): List[A] =
		  (count, curList) match {
			case (_, Nil)                     => result.reverse
			case (c, h :: tail) if end <= c   => result.reverse
			case (c, h :: tail) if start <= c => sliceR(c + 1, tail, h :: result)
			case (c, _ :: tail)               => sliceR(c + 1, tail, result)
		  }
		sliceR(0, ls, Nil)
	  }

	// Since several of the patterns are similar, we can condense the tail recursive
	// solution a little.
	def sliceTailRecursive2[A](start: Int, end: Int, ls: List[A]): List[A] = {
		def sliceR(count: Int, curList: List[A], result: List[A]): List[A] = {
		  if (curList.isEmpty || count >= end) result.reverse
		  else sliceR(count + 1, curList.tail,
					  if (count >= start) curList.head :: result
					  else result)
		}
		sliceR(0, ls, Nil)
	}

	// Functional.
	def sliceFunctional[A](s: Int, e: Int, ls: List[A]): List[A] =
		ls drop s take (e - (s max 0))
}
