package matlux.implicittest

object conversion {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  implicit class IntWithTimes(x: Int) {
    def times[A](f: => A): Unit = {
      def loop(current: Int): Unit =
        if(current > 0) {
          f
          loop(current - 1)
        }
      loop(x)
    }
  }
  
  
  5.times(println("HI"))                          //> HI
                                                  //| HI
                                                  //| HI
                                                  //| HI
                                                  //| HI
}