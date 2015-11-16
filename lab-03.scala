def evens: List[Int] => List[Int] = 
  xs => for { x <- xs.filter(_ % 2 == 0) } yield x

def squares: Int => Seq[Int] = 
  n => for { x <- 1 to n } yield x * x

def sumSquares: Int => Int =
  n => squares(n).foldLeft(0)(_+_)

def _squares: Int => Int => Seq[Int] =
  m => {
    n => {
      val i  = n + 1
      val j  = m + n
      for {
        x <- i to j
      } yield x * x
    }
  }
  
def _sumSquares: Int => Int =
  x => Function.uncurried[Int,Int,Seq[Int]](_squares)(x, x).foldLeft(0)(_+_)

def coords: Int => Int => Seq[(Int,Int)] = 
  n => {
    m => for { 
      x <- 0 to n
      y <- 0 to m
      } yield (x , y)
  }
