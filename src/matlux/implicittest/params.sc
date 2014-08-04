package matlux.implicittest

import scala.math.Ordered

object params {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  case class Foo(v : Int)
  
  
  implicit class FooWithOrder(x : Foo) extends Ordered[Foo] {
  	def compare(that: Foo): Int = x.v.compare(that.v)
  }
  
    //def max[T <% Ordered[T]](a: T, b: T): T = if (a < b) b else a
	def max[T](a: T, b: T)(implicit $ev1: Function1[T, Ordered[T]]): T =
		if ($ev1(a) < b) b else a         //> max: [T](a: T, b: T)(implicit $ev1: T => scala.math.Ordered[T])T
	
	max(3,4)                                  //> res0: Int = 4
	max("a","b")                              //> res1: String = b
	
	val a = Foo(3)                            //> a  : matlux.implicittest.params.Foo = Foo(3)
	val b = Foo(4)                            //> b  : matlux.implicittest.params.Foo = Foo(4)
	
	max(a,b)                                  //> res2: matlux.implicittest.params.Foo = Foo(4)
}