import scala.annotation.tailrec

// LAB 1

def generatePrimes(num: Int): List[Int] = {
  if (num < 2) List()

  val sieve = Array.fill(num + 1)(true)
  sieve(0) = false
  sieve(1) = false

  for (i <- 2 to math.sqrt(num).toInt) {
    if (sieve(i)) {
      for (j <- i * i to num by i) {
        sieve(j) = false
      }
    }
  }

  (2 to num).filter(sieve).toList
}

def findPairs(num: Int, primes: List[Int]): List[(Int, Int)] = {
  val primeSet = primes.toSet

  primes
    .filter(p => p <= num - p && primeSet.contains(num - p))
    .map(p => (p, num - p))
}

@main def lab1_zad1(): Unit = {
  print("number: ")
  var n = io.StdIn.readInt()
  var primes = generatePrimes(n)

  val pairs = (4 to n by 2).map { num =>
    num -> findPairs(num, primes)
  }

  pairs.foreach { case (num, pair) =>
    pair.foreach { case (p1, p2) =>
      println(s"${p1} + ${p2} = ${num}")
    }
  }
}

def obramuj(napis: String): String = {
  val lines = napis.split('\n')
  if (lines.isEmpty) return "**\n**"

  val maxLen = lines.maxBy(_.length).length
  val border = "*" * (maxLen + 4)

  val framedLines = lines.map { line =>
    val padding = " " * (maxLen - line.length)
    s"* $line$padding *"
  }

  (border +: framedLines :+ border).mkString("\n")
}

// LAB 2

def reverse(str: String): String = {
  if (str.isEmpty) ""
  else reverse(str.tail) + str.head
}

def palindrome(arr: Array[Int]): Boolean = {
  if (arr.length <= 1) true
  else if (arr.head != arr.last) false
  else palindrome(arr.slice(1, arr.length - 1))
}

def triangle(height: Int): Unit = {
  def pascal(row: Int, col: Int): Int = {
    if (col == 0 || col == row) 1
    else pascal(row - 1, col - 1) + pascal(row - 1, col)
  }

  def printRow(row: Int, col: Int): Unit = {
    if (col <= row) {
      if (col == 0) print(" " * ((height - row - 1) * 2))
      else print("   ")

      print(pascal(row, col))
      printRow(row, col + 1)
    } else println()
  }

  def printAllRows(n: Int): Unit = {
    if (n < height) {
      printRow(n, 0)
      printAllRows(n + 1)
    }
  }

  if (height > 0) printAllRows(0)
}

def daSie(n: Int): Boolean = {
  def isPrime(num: Int, div: Int = 2): Boolean = {
    if (num <= 1) false
    else if (div * div > num) true
    else if (num % div == 0) false
    else isPrime(num, div + 1)
  }

  def checkEven(num: Int): Boolean = {
    if (num > n) true
    else if (num % 2 != 0) checkEven(num + 1)
    else findPrimePair(num, 2) && checkEven(num + 1)
  }

  def findPrimePair(evenNum: Int, firstPrime: Int): Boolean = {
    if (firstPrime > evenNum / 2) {
      println(s"${evenNum}: not found")
      false
    } else if (isPrime(firstPrime) && isPrime(evenNum - firstPrime)) {
      println(s"${evenNum} = ${firstPrime} + ${evenNum - firstPrime}")
      true
    } else {
      findPrimePair(evenNum, firstPrime + 1)
    }
  }

  if (n < 2) true
  else checkEven(4)
}

// LAB 3

def reverse_tailrec(str: String): String = {
  @tailrec
  def helper(str: String, acc: String): String = {
    if (str.isEmpty) acc
    else helper(str.tail, str.head +: acc)
  }
  helper(str, "")
}

def isPrime(n: Int): Boolean = {
  @tailrec
  def helper(n: Int, div: Int = 2): Boolean = {
    if (n <= 1) false
    else if (div * div > n) true
    else if (n % 2 == 0) false
    else helper(n, div + 1)
  }
  helper(n)
}

def binToDec(bin: Int): Int = {
  @tailrec
  def helper(bin: Int, pos: Int = 0, acc: Int = 0): Int = {
    if (bin == 0) acc
    else {
      val digit = bin % 10
      if (digit != 0 && digit != 1) throw new IllegalArgumentException("not a binary number")
      else helper(bin / 10, pos + 1, acc + digit * math.pow(2, pos).toInt)
    }
  }

  if (bin < 0) throw new IllegalAccessException("binary number cannot be negative")
  helper(bin)
}

def value(n: Int): Int = {
  @tailrec
  def helper(i: Int, prev: Int, curr: Int): Int = {
    if (i == n) prev
    else helper(i + 1, curr, prev + curr)
  }

  if (n < 0) throw new IllegalArgumentException("not an unsigned number")

  if (n == 0) 2
  else if (n == 1) 1
  else helper(0, 2, 1)
}

@main def lab3_zad4(): Unit = {
  println((0 to 10).map(value).mkString(", "))
}

def isOrdered(tab: Array[Int], mlr: (Int, Int) => Boolean): Boolean = {
  @tailrec
  def helper(i: Int): Boolean = {
    if (i == tab.length - 1) true
    else if (!mlr(tab(i), tab(i + 1))) false
    else helper(i + 1)
  }

  if (tab.length < 2) true
  else helper(0)
}

def worth(
    tab1: Array[Int],
    tab2: Array[Int],
    pred: (Int, Int) => Boolean,
    op: (Int, Int) => Int
): Option[Int] = {
  @tailrec
  def helper(i: Int): Option[Int] = {
    if (i >= tab1.length || i >= tab2.length) None
    else if (pred(tab1(i), tab2(i))) Some(op(tab1(i), tab2(i)))
    else helper(i + 1)
  }

  helper(0)
}

// LAB 4

def divide[A](list: List[A]): (List[A], List[A]) = {
  @tailrec
  def loop(remaining: List[A], idx: Int, oddAcc: List[A], evenAcc: List[A]): (List[A], List[A]) = {
    remaining match {
      case Nil => (evenAcc.reverse, oddAcc.reverse)
      case x :: xs => {
        idx % 2 match {
          case 0 => loop(xs, idx + 1, x :: evenAcc, oddAcc)
          case _ => loop(xs, idx + 1, evenAcc, x :: oddAcc)
        }
      }
    }
  }

  loop(list, 0, Nil, Nil)
}

// LAB 5

@main def lab5_main(): Unit = {
  val p = for {
    i <- (1 to 10) if (i % 2 == 0)
  } yield i * 2

  println(p.toList)
}

// LAB 8

def countChars(str: String): Int = {
  str.distinct.length
}

def minNotCon(set: Set[Int]): Int = {
  (0 to set.size).find(x => !set.contains(x)).getOrElse(0)
}

def swap[A](seq: Seq[A]): Seq[A] = {
  seq
}

@main def lab6_main: Unit = {
  println(countChars("asdfasdfxyz"))
  println(minNotCon(Set(-3, 0, 1, 2, 5, 6)))
  println(swap(Seq(-3, 0, 1, 2, 5, 6)))
}

@main def main(): Unit = {
  val x = 7
  println(s"${x} + ${x} == ${x + x}")
}
