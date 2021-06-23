package lectures.part2afp

object CurriesPAF extends App {

	// curried functions
	val superAdder: Int => Int => Int =
		x => y => x + y

	val add3 = superAdder(3)
	println(add3(3))
	println(superAdder(2)(2)) // curried function

	// METHOD!
	def curriedAdder(x: Int)(y: Int): Int = x + y // curried method

	val add4: Int => Int = curriedAdder(4)
	println(add4(1))

	// lifting = ETA-EXPANSION

	// functions != methods (JVM Limitation)
	def inc(x: Int) = x + 1
	List(1, 2, 3).map(inc)

	// Partial function applications
	val add5 = curriedAdder(5) _ // Int => Int

	// EXERCISE
	val simpleAddFunction = (x: Int, y: Int) => x + y
	def simpleAddMethod(x: Int, y: Int) = x + y
	def curriedAddMethod(x: Int)(y: Int) = x + y

	val add7 = (x: Int) => simpleAddFunction(7, x)
	val add7_2 = simpleAddFunction.curried(7)
	val add7_6 = simpleAddFunction(7, _: Int)

	val add7_3 = curriedAdder(7) _
	val add7_4 = curriedAdder(7)(_)

	val add7_5 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into functions values

	// underscores are powerful
	def concatenator(a: String, b: String, c: String): String = a + b + c
	val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")
	println(insertName("Gabriel"))

	val fillInTheBlanks = concatenator("Hello ", _ :String, _:String)
	println(fillInTheBlanks("Gabriel", " i am learning Scala"))

	def curriedFormatter(s: String)(number: Double): String = s.format(number)
	val listOfNumbers = List(Math.PI, 1.478412312312112354543f, 5.748713123123123123123f)

	val simpleFormat = curriedFormatter("%4.2f") _ //lift
	val seriousFormat = curriedFormatter("%8.6f") _
	val preciseFormat = curriedFormatter("%14.12f") _
	println(listOfNumbers.map(seriousFormat))

	def byName(n: => Int): Unit = n + 1
	def byFunction(f: () => Int) = f() + 1

	def method: Int = 42
	def parenMethod(): Int = 42

	byName(23) // ok
	byName(method) // ok
	byName(parenMethod)
	// byName(() => 42) // not ok
	byName((() => 42)()) // ok
	// byName(parenMethod _) // not ok

	// byFunction(45) // not ok
	// byFunction(method) // not ok
	byFunction(parenMethod) // compiler does ETA-expansion
	byFunction(parenMethod _) // compiler does that on the previous example
	byFunction(() => 42)
}
