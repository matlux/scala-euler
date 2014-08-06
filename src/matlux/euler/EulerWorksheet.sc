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
  
  // problem #5
  def problem5(low : Int, high : Int) = {
  	val r = low.to(high)
  	val res = for {
  		n <- 1.to(300000000)
  		if(r.takeWhile(d => n % d == 0).size == r.size)
  	} yield(n)
  	res.head
  }                                               //> problem5: (low: Int, high: Int)Int
  	
  //problem5(1,10)
  //problem5(1,20)
  
  //problem6
  
  def sumOfSqrt(sx : List[Int]) =
  	sx.map(n => n*n).sum                      //> sumOfSqrt: (sx: List[Int])Int
  	
  sumOfSqrt(1.to(10).toList)                      //> res8: Int = 385
  
  def sqrtOfSum(sx : List[Int]) = {
  	val s = sx.sum
  	s * s
  }                                               //> sqrtOfSum: (sx: List[Int])Int
  	
  sqrtOfSum(1.to(10).toList)                      //> res9: Int = 3025
  
  def problem6() =
  	sqrtOfSum(1.to(100).toList) - sumOfSqrt(1.to(100).toList)
                                                  //> problem6: ()Int
  problem6                                        //> res10: Int = 25164150
  
  //problem 7
    
//  def sieve(s: Stream[Long]): Stream[Long] =
//  	s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  def nthPrime(nth : Long) = {
	  def sieve(numbers : Stream[Long], primes: Stream[Long]) : Stream[Long] =
  	  if(primes.size >= nth) primes
    	else {
      	val fnumbers = numbers.filter(n => n % primes.head != 0)
      	//println(fnumbers)
    		sieve(fnumbers, fnumbers.head #:: primes)
    	}
    sieve(from(3L),Stream(2L)).head
  }                                               //> nthPrime: (nth: Long)Long

  
//  sieve(from(3L),List(2L))
  
  	
  nthPrime(6)                                     //> res11: Long = 13
  nthPrime(10001)                                 //> res12: Long = 104743

  //problem 8

  val digits="73167176531330624919225119674426574742355349194934" +
"96983520312774506326239578318016984801869478851843" +
"85861560789112949495459501737958331952853208805511" +
"12540698747158523863050715693290963295227443043557" +
"66896648950445244523161731856403098711121722383113" +
"62229893423380308135336276614282806444486645238749" +
"30358907296290491560440772390713810515859307960866" +
"70172427121883998797908792274921901699720888093776" +
"65727333001053367881220235421809751254540594752243" +
"52584907711670556013604839586446706324415722155397" +
"53697817977846174064955149290862569321978468622482" +
"83972241375657056057490261407972968652414535100474" +
"82166370484403199890008895243450658541227588666881" +
"16427171479924442928230863465674813919123162824586" +
"17866458359124566529476545682848912883142607690042" +
"24219022671055626321111109370544217506941658960408" +
"07198403850962455444362981230987879927244284909188" +
"84580156166097919133875499200524063689912560717606" +
"05886116467109405077541002256983155200055935729725" +
"71636269561882670428252483600823257530420752963450"
                                                  //> digits  : String = 73167176531330624919225119674426574742355349194934969835
                                                  //| 203127745063262395783180169848018694788518438586156078911294949545950173795
                                                  //| 833195285320880551112540698747158523863050715693290963295227443043557668966
                                                  //| 489504452445231617318564030987111217223831136222989342338030813533627661428
                                                  //| 280644448664523874930358907296290491560440772390713810515859307960866701724
                                                  //| 271218839987979087922749219016997208880937766572733300105336788122023542180
                                                  //| 975125454059475224352584907711670556013604839586446706324415722155397536978
                                                  //| 179778461740649551492908625693219784686224828397224137565705605749026140797
                                                  //| 296865241453510047482166370484403199890008895243450658541227588666881164271
                                                  //| 714799244429282308634656748139191231628245861786645835912456652947654568284
                                                  //| 891288314260769004224219022671055626321111109370544217506941658960408071984
                                                  //| 038509624554443629812309878799272442849091888458015616609791913387549920052
                                                  //| 40636899125607176060588
                                                  //| Output exceeds cutoff limit.
                                                  

	def string2List(digitsStr : String) =
		digitsStr.map(c => Integer.parseInt(c.toString()))
                                                  //> string2List: (digitsStr: String)scala.collection.immutable.IndexedSeq[Int]

  def groupInt(digits : List[Int], n : Int) : List[List[Int]] = {
  	if(digits.size == 0) List()
  	else digits.take(n) :: groupInt(digits.drop(1),n)
  }                                               //> groupInt: (digits: List[Int], n: Int)List[List[Int]]
  
  groupInt(string2List(digits).toList.take(8),4)  //> res13: List[List[Int]] = List(List(7, 3, 1, 6), List(3, 1, 6, 7), List(1, 6
                                                  //| , 7, 1), List(6, 7, 1, 7), List(7, 1, 7, 6), List(1, 7, 6), List(7, 6), Lis
                                                  //| t(6))
  def multList(sx : List[Int]) =
    sx.foldLeft(1)(_ * _)                         //> multList: (sx: List[Int])Int
  
  multList(List(2,2,3,4))                         //> res14: Int = 48
  	
  def problem8(digits : List[Int], size: Int) =
  	groupInt(digits, size).map(lx => (lx, multList(lx))).sortBy(_._2).last
                                                  //> problem8: (digits: List[Int], size: Int)(List[Int], Int)

	problem8(string2List(digits).toList,4)    //> res15: (List[Int], Int) = (List(9, 9, 8, 9),5832)
	problem8(string2List(digits).toList,13)   //> res16: (List[Int], Int) = (List(9, 7, 8, 1, 7, 9, 7, 7, 8, 4, 6, 1, 7),2091
                                                  //| 059712)



}