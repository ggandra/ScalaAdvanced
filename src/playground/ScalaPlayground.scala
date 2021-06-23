package playground

object ScalaPlayground extends App {

	println("Hello, Scala")

	val aList: List[Int] = List(1, 5, 3, 4, 10)
	val aLambda = (x: Int) => List(x, x + 1, x*2)
	println(aList.map(_ + 1))
	println(aList.flatMap(aLambda))

	val aOption: Option[String] = None

	println(aOption.getOrElse("Goodbye"))
}
