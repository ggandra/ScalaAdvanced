package lectures.part2afp

object PartialFunctions extends App {

	val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int
	val aFussyFunction = (x: Int) =>
		if (x == 1) 42
		else if (x == 2) 56
		else if (x == 5) 999
		else throw new FunctionNotApplicableException

	class FunctionNotApplicableException extends RuntimeException

	val aNicerFussyFunction = (x: Int) => x match {
		case 1 => 42
		case 2 => 56
		case 5 => 999
	}
	// {1, 2, 5} => Int

	val aPartialFunction: PartialFunction[Int, Int] = {
		case 1 => 42
		case 2 => 56
		case 5 => 999
	} // partial function value

	println(aPartialFunction(2))
//	println(aPartialFunction(123213))

	// PF utilities
	println(aPartialFunction.isDefinedAt(2))

	// lift
	val lifted = aPartialFunction.lift // Int => Option[Int]
	println(lifted(2))
	println(lifted(98))

	val pfChain = aPartialFunction.orElse[Int, Int] {
		case 45 => 67
	}

	println(pfChain(45))

	// PF extend normal functions
	val aTotalFunction: Int => Int = {
		case 1 => 99
	}

	// HOFs accept partial functions as well
	val aMappedList = List(1, 2, 3).map {
		case 1 => 42
		case 2 => 78
		case 3 => 1000
	}
	println(aMappedList)

	abstract class Animal {
		def validAnimalName: PartialFunction[String, Boolean]
	}

	object Dog extends Animal {

		def validAnimalName: PartialFunction[String, Boolean] = {
			case "totó" => true
			case "bilu" => true
		}
	}

	val toto = Dog
	println(toto.validAnimalName("totó"))

	val aManualFussyFunction = new PartialFunction[Int, Int] {
		override def apply(x: Int): Int = x match {
			case 1 => 42
			case 2 => 56
			case 5 => 999
		}

		override def isDefinedAt(x: Int): Boolean =
			x == 1 || x == 2 || x == 3
	}

	def idiotChatbot: PartialFunction[String, String] = {
		case "who are you?" => "I am an idiot robot"
		case "you are a robot?" => "Yes"
		case "you have a father?" => "No"
	}

	scala.io.Source.stdin.getLines().map(idiotChatbot).foreach(println)

}
