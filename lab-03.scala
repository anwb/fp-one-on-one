val evens: List[Int] => List[Int] = 
  xs => for { x <- xs.filter(_ % 2 == 0) } yield x

val squares: Int => Seq[Int] = 
  n => for { x <- 1 to n } yield x * x

val sumSquares: Int => Int =
  n => squares(n).foldLeft(0)(_+_)

val _squares: Int => Int => Seq[Int] =
  m => {
    n => {
      val i  = n + 1
      val j  = m + n
      for {
        x <- i to j
      } yield x * x
    }
  }
  
val _sumSquares: Int => Int =
  x => Function.uncurried[Int,Int,Seq[Int]](_squares)(x, x).foldLeft(0)(_+_)

val coords: Int => Int => Seq[(Int,Int)] = 
  n => {
    m => for { 
      x <- 0 to n
      y <- 0 to m
      } yield (x , y)
  }
