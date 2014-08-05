package matlux.euler

object EulerWorksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //problem #1
  
  def multiples35() : Int =
  	0.until(1000).filter(x => ((x % 3) == 0) || ((x % 5) == 0)).sum
                                                  //> multiples35: ()Int
  
  
  multiples35()                                   //> res0: Int = 233168
  
  // problem #2
  0.until(10).toList.zip(1.until(11).toList).map(n => n._1 + n._2)
                                                  //> res1: List[Int] = List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
  
  lazy val fibs: Stream[Int] =
  	1 #::
  	1 #::
  	fibs.zip(fibs.tail).map(n => n._1 + n._2) //> fibs: => Stream[Int]
  
  fibs.take(10).toList                            //> res2: List[Int] = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  def evenFibonacciSum() : Int =
  	fibs.filter(n => n % 2 == 0).takeWhile( n =>  n < 4e6).sum
                                                  //> evenFibonacciSum: ()Int
  	
  evenFibonacciSum                                //> res3: Int = 4613732
  
  //problem #3
  
  def sieve(s: Stream[Int]): Stream[Int] =
  	s.head #:: sieve(s.tail.filter(_ % s.head != 0))
                                                  //> sieve: (s: Stream[Int])Stream[Int]
  
  sieve(Stream.from(2)).take(40).toList           //> res4: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 4
                                                  //| 7, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131
                                                  //| , 137, 139, 149, 151, 157, 163, 167, 173)
  

  
  
}