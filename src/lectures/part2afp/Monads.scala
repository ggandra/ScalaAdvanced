package lectures.part2afp

object Monads extends App {

	// our own Try monad
	trait Attempt[+A] {
		def flatMap[B](f: A => Attempt[B]): Attempt[B]
	}

	object Attempt {
		def apply[A](a: => A): Attempt[A] =
			try {
				Success(a)
			} catch {
				case e: Throwable => Failure(e)
			}
	}

	case class Success[+A](value: A) extends Attempt[A] {
		def flatMap[B](f: A => Attempt[B]): Attempt[B] =
			try {
				f(value)
			} catch {
				case e: Throwable => Failure(e)
			}
	}

	case class Failure(e: Throwable) extends Attempt[Nothing] {
		def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
	}

	val attempt = Attempt {
		throw new RuntimeException("My own Monad")
	}
	println(attempt)

	class Lazy[+A](value: => A) {
		private lazy val internalValue = value
		def use: A = internalValue
		def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
	}
	object Lazy {
		def apply[A](value: => A): Lazy[A] = new Lazy(value)
	}

	val lazyInstance = Lazy {
		println("Today i don't feel like doing anything")
		42
	}

	val flatMappedInstance = lazyInstance.flatMap(x => Lazy {
		10 * x
	})

	val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy {
		10 * x
	})
	flatMappedInstance.use
	flatMappedInstance2.use


}
