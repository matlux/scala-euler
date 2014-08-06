package matlux.euler

object test {
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
  
  groupInt(string2List(digits).toList.take(8),4)  //> res0: List[List[Int]] = List(List(7, 3, 1, 6), List(3, 1, 6, 7), List(1, 6,
                                                  //|  7, 1), List(6, 7, 1, 7), List(7, 1, 7, 6), List(1, 7, 6), List(7, 6), List
                                                  //| (6))
  def multList(sx : List[Int]) =
    sx.foldLeft(1)(_ * _)                         //> multList: (sx: List[Int])Int
  
  multList(List(2,2,3,4))                         //> res1: Int = 48
  	
  def problem8(digits : List[Int], size: Int) =
  	groupInt(digits, size).map(lx => (lx, multList(lx))).sortBy(_._2).last
                                                  //> problem8: (digits: List[Int], size: Int)(List[Int], Int)

	problem8(string2List(digits).toList,4)    //> res2: (List[Int], Int) = (List(9, 9, 8, 9),5832)
	problem8(string2List(digits).toList,13)   //> res3: (List[Int], Int) = (List(9, 7, 8, 1, 7, 9, 7, 7, 8, 4, 6, 1, 7),20910
                                                  //| 59712)

}