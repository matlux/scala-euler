package matlux.euler

object EulerWorksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
   0.until( 10 ).toList                           //> res0: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
   0.to( 10 ).toList                              //> res1: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
   
  
  
  def multiples35() : Int =
  	0.until(1000).filter(x => ((x % 3) == 0) || ((x % 5) == 0)).sum
                                                  //> multiples35: ()Int
  
  
  multiples35()                                   //> res2: Int = 233168
  
  
  Stream.from(2).take(3).tail.tail.head           //> res3: Int = 4
  
  0.until(10).toList.zip(1.until(11).toList).map(n => n._1 + n._2)
                                                  //> res4: List[Int] = List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)
  
  lazy val fibs: Stream[Int] =
  	1 #::
  	1 #::
  	fibs.zip(fibs.tail).map(n => n._1 + n._2) //> fibs: => Stream[Int]
  
  fibs.take(10).toList                            //> res5: List[Int] = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  def evenFibonacciSum() : Int =
  	fibs.filter(n => n % 2 == 0).takeWhile( n =>  n < 4e6).sum
                                                  //> evenFibonacciSum: ()Int
  	
  evenFibonacciSum                                //> res6: Int = 4613732
  
  
}