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
  
  
  // old problem3 tools not used:
    def sieve(s: Stream[Long]): Stream[Long] =
  	s.head #:: sieve(s.tail.filter(_ % s.head != 0))
                                                  //> sieve: (s: Stream[Long])Stream[Long]
  
    //sieve(from(2L)).take(4).toList
  
  
  //problem #3
  
  def from(b : Long) : Stream[Long] =
  	b #:: from(b+1)                           //> from: (b: Long)Stream[Long]
  	  
  def divides(dividend : Long, divisor : Long) =
  	dividend % divisor == 0                   //> divides: (dividend: Long, divisor: Long)Boolean
  
  def prime(n : Long) =
  	from(2).takeWhile(f => f < math.sqrt(n)).filter(p => divides(n,p)) == Stream.empty
                                                  //> prime: (n: Long)Boolean
  def factors(n : Long) : Stream[Long] =
  	from(2).takeWhile(p => p < math.sqrt(n)).filter(p => divides(n,p))
                                                  //> factors: (n: Long)Stream[Long]
  def largestPrimeFac(n: Long) =
  	factors(n).takeWhile(prime(_)).last       //> largestPrimeFac: (n: Long)Long
    
  //factors(13195).toList
  //pFactors(13195).last
  largestPrimeFac(600851475143L)                  //> res4: Long = 6857
  
  /* same in Clojure
  
  (defn divides?
  "Does divisor divide dividend evenly?"
  [dividend divisor]
  (zero? (rem dividend divisor)))
 
(defn factors
  "Returns a sequence of all factors of p."
  [p]
  (filter #(divides? p %) (range 2 (Math/sqrt p))))
 
(defn prime?
  "Returns true if p is prime, false otherwise."
  [p]
  (empty? (factors p)))
 
(defn largestPrimeFac
  "Find the largest prime factor of a composite number."
  [p]
  (last (take-while prime? (factors p))))
 
(largestPrimeFac 600851475143)
  */
  
  
  // problem #4
  def isPalindrom(n : Int) = {
    val nstr = n.toString()
  	nstr.reverse == nstr
  }                                               //> isPalindrom: (n: Int)Boolean
  
  isPalindrom(4003)                               //> res5: Boolean = false
  isPalindrom(4004)                               //> res6: Boolean = true
  
  def largestPalindrom(max : Int) = {
  	val v = for {
  		x <- 1.to(max)
  		y <- 1.to(max)
  		s = x * y
  		if(isPalindrom(s))
  	} yield(s)
  	v.foldRight(0)((v,mx) => v max mx)
  }                                               //> largestPalindrom: (max: Int)Int
  	
  largestPalindrom(999)                           //> res7: Int = 906609
  
}