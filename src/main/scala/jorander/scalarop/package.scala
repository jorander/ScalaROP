package jorander

/*
 * This is my attempt at a Scala port of the small DSL for "Railway Oriented Programming" (ROP)
 * proposed by @ScottWlaschin in his blog post "A recipe for a functional app"
 * (http://fsharpforfunandprofit.com/series/a-recipe-for-a-functional-app.html)
 *
 * Disclaimer: I did this as a learning exercise to learn a bit of Scala, functional programming,
 * Scala Test and "Railway Oriented Programming". It's an experiment and should be treated as such.
 * That said, I'm more than happy to accept comments on how the implementation might be improved.
 *
 * Best Regards,
 * Jörgen Andersson
 * Twitter: @se_thinking
 */
package object scalarop {

  /*
   * Since Scala seems to lack(*) the equivalent of F# operator >> for composing functions or piping
   * data into functions the operator ->> is introduced for this purpose. It enables the same "flow"
   * in the code using this DSL as in the F# examples.
   *
   * (*) The closest Scala has to offer seems to be the for comprehension, but it isn't even coming
   * close to the same readability and cleanness in the client code. (See the accompanying test code
   * for examples.)
   */
  sealed case class ComposableFunction[A, B](f1: A => B) {
    def ->>[C](f2: B => C) = (a: A) => f2(f1(a))
  }

  implicit def function2ComposableFunction[A, B](f: A => B) = ComposableFunction(f)

  sealed case class ComposableData[A](d: A) {
    def ->>[B](f: A => B) = f(d)
  }

  implicit def data2ComposableData[A](d: A) = ComposableData(d)

  /**
   * The two-track type. Constructed as Success or Failure. Can be used
   * in Scala pattern matching and for comprehensions as well as with the
   * other functions in this package.
   */
  sealed trait TwoTrackResult[+S, +F] {
    /**
     * Pipes this two-track value into a switch function
     * @param f switch function
     */
    def ->=[S1, F1](f: S => TwoTrackResult[S1, F1]) = this ->> bind(f)

    def flatMap[S1, F1](f: S => TwoTrackResult[S1, F1]) = ->=(f)

    def map[S1](f: S => S1) = flatMap(f ->> succeed)
  }

  case class Success[S, F](data: S) extends TwoTrackResult[S, F]

  case class Failure[S, F](msg: F) extends TwoTrackResult[S, F]

  /**
   * Wraps the data in a Success object
   */
  def succeed[S](s: S) = Success(s)

  /**
   * Wraps the data in a Failure object
   */
  def fail[F](f: F) = Failure(f)

  /**
   * Apply either a success function or failure function
   */
  def either[S, F, S1, F1](successFunc: S => TwoTrackResult[S1, F1], failureFunc: F => TwoTrackResult[S1, F1])(twoTrackInput: TwoTrackResult[S, F]) = {
    twoTrackInput match {
      case Success(s) => s ->> successFunc
      case Failure(f) => f ->> failureFunc
    }
  }

  /**
   * Convert a switch function into a two-track function
   */
  def bind[S, F, S1, F1](f: S => TwoTrackResult[S1, F1]) = (input: TwoTrackResult[S, F]) => input ->> either(f, fail[F])

  /**
   * Enables a switch to be composed with another switch into a combined switch.
   */
  sealed case class ComposableSwitchFunction[I, S, F](f1: I => TwoTrackResult[S, F]) {
    /**
     * Compose two switches into another switch
     */
    def >=>[S1](f2: S => TwoTrackResult[S1, F]) = (input: I) => input ->> (f1 ->> bind(f2))
  }

  implicit def switchFuction2ComposableSwitchFunction[I, S, F](f: I => TwoTrackResult[S, F]) = ComposableSwitchFunction(f)

  /**
   * Convert a one-track function into a switch
   */
  def switch[S1, S2](f: S1 => S2) = f ->> succeed

  /**
   * Convert a one-track function into a two-track function
   */
  def map[S1, S2](f: S1 => S2) = (input: TwoTrackResult[S1, Any]) => input map f

  /**
   * Convert a dead-end function into a one-track function
   */
  def tee[I](f: I => Unit) = (input: I) => {
    f(input)
    input
  }

  /**
   * Convert a one-track function into a switch with exception handling
   */
  def tryCatch[S1, S2, F](f: S1 => S2, exnHandler: Throwable => F) = (input: S1) => {
    try {
      input ->> f ->> succeed
    } catch {
      case t: Throwable => t ->> exnHandler ->> fail
    }
  }

  /**
   * Convert two one-track functions into a two-track function
   */
  def doubleMap[S1, F1, S2, F2](successFunc: S1 => S2, failureFunc: F1 => F2) =
    (input: TwoTrackResult[S1, F1]) => input ->> either(successFunc ->> succeed, failureFunc ->> fail)

  /**
   * Run two switches in parallel and combine the results
   */
  def plus[S, F, AS, AF](addSuccess: (S, S) => AS, addFailure: (F *) => AF)(switch1: S => TwoTrackResult[S, F], switch2: S => TwoTrackResult[S, F]) =
    (input: S) => (switch1(input), switch2(input)) match {
      case (Success(s1), Success(s2)) => Success(addSuccess(s1, s2))
      case (Failure(f1), Success(_)) => Failure(addFailure(f1))
      case (Success(_), Failure(f2)) => Failure(addFailure(f2))
      case (Failure(f1), Failure(f2)) => Failure(addFailure(f1, f2))
    }
}
