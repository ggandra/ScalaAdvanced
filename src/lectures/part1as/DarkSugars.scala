package lectures.part1as

import scala.util.Try

object DarkSugars extends App {

	// ss 1
	def singleArgMethod(arg: Int): String = s"$arg little ducks..."

	val description = singleArgMethod {
		// write some complex code
		42
	}

	val aTryInstance = Try {
		throw new RuntimeException
	}

	List(1, 2, 3).map { x =>
		x + 1
	}

	// syntax sugar 2
	trait Action {
		def act(x: Int): Int
	}

	val anInstance: Action = new Action {
		override def act(x: Int): Int = x + 1
	}

	val aFunkyInstance: Action = (x: Int) => x + 1

	// example: Runnable
	val aThread = new Thread(new Runnable {
		override def run(): Unit = println("Hello, scala")
	})

	val aSweeterThread = new Thread(() => println("sweet, scala"))

	abstract class AnAbstractType {
		def implemented: Int = 23
		def f(a: Int): Unit
	}

	val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

	// ss 3 the :: and #: methods are special
	val prependedList = 2 :: List(3, 4)
	println(prependedList)

	val list = 1 :: 2 :: 3 :: List(4, 5)
	val theSameList = List(4, 5).::(3).::(2).::(1)

	class MyStream[T] {
		def -->:(value: T): MyStream[T] = this
	}

	 val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

	// ss multi-word method naming
	class TeenGirl(name: String) {
		def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
	}

	val lili = new TeenGirl("lily")
	lili `and then said` "Scala is so sweet"

	// ss infix types
	class Composite[A, B]
	val composite: Int Composite String = ???

	class -->[A, B]
	val towards: Int --> String = ???

	// ss update method special like apply
	val anArray = Array(1, 2, 3)
	anArray(2) = 7 // rewritten to anArray.update(2, 7)
	// used in mutable collections


	// ss setters for mutable containers
	class Mutable {
		private var internalMember: Int = 0
		def member: Int = internalMember
		def member_=(value: Int): Unit =
			internalMember = value
	}

	val aMutableContainer = new Mutable
	aMutableContainer.member = 42 // rewritten as aMutableContainer.member_=(42)

}
