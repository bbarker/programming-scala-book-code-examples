import scala.util._

/*
 * Last week we talked about Referential Transparency and said that
 * overall an expression is considered referentially transparent if it can be
 * replaced by the value it is reduced once evaluated, without affecting the
 * program's output in any way.
 *
 * Provide some examples.
 *
 * RT provides two main benefits:
 * - refactoring capabilities
 * - local reasoning which is the fundamental of abstraction
 *
 * One problem that came up is that considering a program cannot be designed
 * without relying on side-effects.
 */

object SideEffectsVsRT {
  // This function is RT because it can be replaced by the value it produces
  // without affecting the program's output in any way
  def foo(i: Int): Boolean = {
    i == 42
  }

  // true is indeed equivalent to foo(42)
  val f = foo(42)
  (f, f) == (true, true)

  // What about bar?
  def bar(i: Int): Boolean = {
    println("Evaluating bar!")
    i == 42
  }

  // Can we replace bar(42) by true without affecting the program's overall output?
  val b = bar(42)
  (b, b) != (true, true)

  // The problem is that `bar` is not referentially transparent. It indeed
  // generate an output that is not captured by its return type. In other words,
  // this is a side effect.

  // So how can we reconcile side-effects with RT? Essentially RT is about
  // describing a program using values only. A value can be replaced by the
  // expression producing it (and vice versa) and qualifies for Referential
  // Transparency. So let's try to express side effects in terms of values.

  // A first approach is to describe the side-effect in terms of a function
  def safeBar(i: Int): () => Boolean =
    () => {
      println("Evaluating bar!")
      i == 42
    }
  // In Scala, functions are first class citizens, and can be expressed in terms
  // of literal.
  val value: () => Boolean           = safeBar(42)
  (value, value) == (safeBar(42), safeBar(42))
  // value is RT, because we can replace it by `safeBar(42)` without affecting
  // the program's output in any way. That's a great first step to bring sanity
  // back on the table. However, `safeBar(42)` on its own does not do much. In
  // order to execute the program described by the value, we have to make an
  // additional call:
  value() // prints out "Evaluating bar!"

  // Essentially, by making this extra call, we execute the side effect described
  // by the value. What have we done here? Well we separated the description of
  // a program from its evaluation.
  val description: () => Boolean = value
  val execution: Boolean         = value() // prints out "Evaluating bar!"

  // That's another benefit of RT. It enables us to delay the evaluation of
  // a side-effect until the very last moment, that is, when we have no other
  // choice than to run the program described by the value.

  // Let's get further and do the same exercise with another function this time:
  def safeLength(s: String): () => Int =
    () => {
      println("Evaluating zee!")
      s.length()
    }

  // Ideally, we'd like to pipe the result of `safeLength` to `safeBar`. This
  // is unfortunately not possible so far without evaluating `safeLength`.
  safeBar(safeLength("Z is dead")())

  // To achieve that, we need to re-write our two functions. All we do, is to
  // rewrite safeBar and safeLength in terms of function literals
  val safeBar2: Int => Boolean =
    i => {
      println("Evaluating bar!")
      i == 42
    }

  val safeLength2: String => Int =
    s => {
      println("Evaluating zee!")
      s.length()
    }

  // Function literals in Scala provide functions enabling function composition,
  // so that an A => B and a B => C can be combined in an A => C
  val safeLengthBar: String => Boolean =
    safeLength2.andThen(safeBar2)

  // Again to execute the resulting function, we need to make this extra call:
  safeLengthBar("Z is dead") // prints out "Evaluating bar!" and "Evaluating zee!"

  // This separation between description and execution comes back constantly
  // in Functional Programming. This aspect is what makes it way more maintainable
  // than imperative programming.

  // Let's get further. `andThen` is one way to compose functions, and dealing
  // with function literals is a bit cumbersome. So let's write a small library
  // easing this process

  // Program represents a function literal taking no argument and producing an A.
  // `run` is a function which potentially have side effects. It's "unsafe"
  // to run it. By unsafe, we mean it's not RT.
  case class Program[A](run: () => A) {
    // `andThen` enables composition of Program just like with Functions earlier
    def andThen[B](f: A => Program[B]): Program[B] =
      Program { () =>
        val a                = run()
        val next: Program[B] = f(a)
        next.run()
      }
  }

  // Let's try to write a slightly more complex program than earlier. We'd like
  // to write a program that welcomes the user, asks for their name, and prints
  // it out back. To achieve that, we first need some primitives:

  def putStrLn(line: String): Program[Unit] =
    Program(() => println(line))

  val getStrLn: Program[String] =
    Program(() => scala.io.StdIn.readLine())

  val welcome: Program[Unit] =
    putStrLn("Hi, what is your name?")
      .andThen(_ => getStrLn)
      .andThen(name => putStrLn(s"Welcome $name"))

  // Note that so far, no side-effect has been run at all!
  // let's run the program!

  //welcome.run()

  // `Program` is essentially an effect system. It enables us to describe
  // effectful computations, that is, computations generating side effects
  // while not breaking referential transparency.

  // Libraries such as Cats-Effect or ZIO are effect system (and are way more
  // sophisticated). While their approach are different, the goal is kinda same.

  // Small detour, we need to make Program covariant in A to be able to
  // assign a Program0[Nothing] to a value typed as a Program0[Whatever].
  // This is not an important detail for our conversation, we can schedule another
  // session for that.
  case class Program0[+A](run: () => A) {
    import Program0._

    // and add failure management (Note that the A0 >: A is required to keep
    // Program0 covariant in A. More on that in another session)
    def recover[A0 >: A](f: Throwable => Program0[A0]): Program0[A0] =
      Program0 { () =>
        scala.util.Try(run()) match {
          case Failure(exception) => f(exception).run()
          case Success(value)     => value
        }
      }

    // along with retry semantic
    def retry(n: Int): Program0[A] =
      recover(e => if (n > 1) retry(n - 1) else fail(e.getMessage()))

    def repeat(n: Int): Program0[A] =
      andThen(a => if (n > 1) repeat(n - 1) else succeed(a))

    def andThen[B](f: A => Program0[B]): Program0[B] =
      Program0 { () =>
        val a                 = run()
        val next: Program0[B] = f(a)
        next.run()
      }
    // BB: we could do: = Program0 { () => f(run()).run()
    // BB: Hypothetical question, why not just have: = f(run())

    // alias for andThen which discards the result of the current program
    // and sequence the one provided in parameter
    def *>[B](that: Program0[B]): Program0[B] =
      andThen(_ => that)

    // In order to leverage for-comprehension, we can also provide an
    // implementation for map and flatMap

    // BB Note: this is a key part of a Functor
    def map[B](f: A => B): Program0[B] =
      flatMap(a => succeed(f(a)))

    // BB Note: along with map, this is a key component of a monad
    def flatMap[B](f: A => Program0[B]): Program0[B] =
      andThen(f)

  }
  object Program0                       {
    // We've also added a constructor to build Program that fail with an
    // message
    def fail(message: String = ""): Program0[Nothing] =
      Program0(() => throw new RuntimeException(message))

    // Note the => before the A. This ensures that value is only evaluated
    // when it's really needed
    def succeed[A](value: => A): Program0[A] =
      Program0(() => value)

    // A fancy constructor to build if-else kind of construct
    def ifElse[B](predicate: Boolean)(
        ifTrue: Program0[B],
        ifFalse: Program0[B]
    ): Program0[B] =
      xs  if (predicate) ifTrue else ifFalse
  }
  import Program0._

  def putStrLn0(line: String): Program0[Unit] =
    Program0(() => println(line))

  val getStrLn0: Program0[String] =
    Program0(() => scala.io.StdIn.readLine())

  // Here's the final program (without for-comprehension)
  val welcome0: Program0[Unit] =
    (putStrLn0("Hi, what is your name?") *> getStrLn0)
      .andThen(name =>
        ifElse(name == "Joe")(
          putStrLn0("Welcome Joe!"),
          putStrLn0("No soup for you!!!") *> fail()
        )
      )
      .retry(1000)

  // And the one with for-comprehension. As you can see, because we have the
  // control over our primitives, the possibilities in terms of combinators
  // are endless
  val welcome1: Program0[Unit] =
    (for {
      _    <- putStrLn0("Hi, what is your name?")
      name <- getStrLn0
      _    <- ifElse(name == "Joe")(
                putStrLn0("Welcome Joe!"),
                fail("No soup for you!!!")
              )
    } yield ()).retry(3)

  // Now let's think about how would one express this
  // not using RT, or an effect system, but only with an imperative style.
  // Is this construct easy to reason about? easy to re-use?
  def imperativeWelcome(nbRetry: Int): Unit = {
    println("Hi, what is your name?")
    val name = scala.io.StdIn.readLine()
    if (name == "Joe")
      println("Welcome Joe!")
    else {
      println("No soup for you!!!")
      if (nbRetry > 0)
        imperativeWelcome(nbRetry - 1)
      else
        throw new xsRuntimeException("KABOOM!")
    }
  }

  // Final thoughts: the current Program is not stack safe:
  // val stackSafe = putStrLn0("Hi, what is your name?").repeat(10000000)
  // There are ways to improve this, but we can see this in another talk.
  // Secondly, the encoding we used for Program0 is referred to as an 
  // executable encoding. This decision comes with some drawbacks and there
  // is a better way to do this (topic for another talk),
}
object Welcome extends App {
  import SideEffectsVsRT._

  welcome0.run()
}