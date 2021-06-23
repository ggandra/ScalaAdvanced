package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {

	val aCondition: Boolean = false
	val aConditionedVal = if (aCondition) 42 else 65

	val aCodeBLock = {
		if (aCondition) 54
		56
	}

	val theUnit = println("hello, scala")

	def aFunction(x: Int): Int = x + 1

	// recursion
	@tailrec
	def factorial(n: Int, acc: Int): Int = {
		if(n <= 1) acc
		else factorial(n - 1, n * acc)
	}

	// oop
	class Animal
	class Dog extends Animal

	val aDog: Animal = new Dog

	trait Carnivore {
		def eat(a: Animal): Unit
	}

	class Crocodile extends Animal with Carnivore {
		def eat(a: Animal): Unit = println("chunch")
	}

	// method notations
	val aCroc = new Crocodile
	aCroc.eat(aDog)
	aCroc eat aDog

	// anonymous classes
	val aCarnivore = new Carnivore {
		override def eat(a: Animal): Unit = println("lau!")
	}

	// generics

	abstract class MyList[+T]
	// singletons and companions
	object  MyList

	case class Person(name: String, age: Int)

	// exceptions and try/catch
	/*val throwsException = throw new RuntimeException // Nothing
	val aPotentialFailure = try {
		throw new RuntimeException
	} catch {
		case e: Exception => "I caught an exception"
	} finally {
		println("some logs")
	}*/

	// packaging and imports

	// functional programming
	val incrementer = new Function1[Int, Int] {
		override def apply(v1: Int): Int = v1 + 1
	}

	incrementer(1)

	val anonymousIncrementer = (x: Int) => x + 1
	List(1, 2, 3).map(anonymousIncrementer) // HOF
	// map, flatMap, filter

	// for-comprehension
	val pairs = for {
		num <- List(1, 2, 3)
		char <- List('a', 'b', 'c')
	} yield num + "-" + char

	// Scala collections: Seqs, Arrays, Vectors, Maps, Tuples
	val aMap = Map(
		"Daniel" -> 789,
		"Jess" -> 555
	)

	val anOption: Option[Int] = Some(2)

	// pattern matching

	val optionAsExpression = anOption match {
		case Some(value) => value
		case _ => 1
	}
	println(optionAsExpression + 1)

	val x = 2
	val order = x match {
		case 1 => "first"
		case 2 => "second"
		case _ => s"${x}th"
	}


}
