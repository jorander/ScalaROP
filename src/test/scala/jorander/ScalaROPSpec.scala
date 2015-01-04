package jorander

import scalarop._
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable.Range

@RunWith(classOf[JUnitRunner])
class ScalaROPSpec extends FlatSpec with Matchers {
  val intToString = (i: Int) => i.toString
  val stringLength = (s: String) => s.length
  "Operator ->>" should "compose two functions where the result of the first function is the input of the second function" in {
    val composedCountDigitsInInt = intToString ->> stringLength
    composedCountDigitsInInt(123) should be(stringLength(intToString(123)))
  }

  it should "be able to compose several functions in a row" in {
    val severalComposedFunctions = intToString ->> stringLength ->> intToString ->> stringLength
    severalComposedFunctions(123) should be(stringLength(intToString(stringLength(intToString(123)))))
  }

  it should "be able to push data into a function accepting a parameter of that type" in {
    123 ->> intToString should be(intToString(123))
    "123" ->> stringLength should be(stringLength("123"))
  }

  it should "be able to push data through several composed functions" in {
    123 ->> intToString ->> stringLength should be(stringLength(intToString(123)))
  }
  
  val streamOfIntToSum = (s: Stream[Int]) => s.sum
  val s = (1 to 5).toStream
  it should "be able to push a Stream of data a function accepting a Stream" in {
    s.toStream ->> streamOfIntToSum should be(streamOfIntToSum(s))
    
  }

  def validateStringNotEmpty(s: String) = if (!s.isEmpty) succeed(s) else scalarop.fail("String should not be empty")

  def validateStringNotTooLong(s: String) = if (s.length < 5) succeed(s) else scalarop.fail("String is too long")

  "A TwoTrackResult" should "be constructed as Success or Failure" in {
    val success: TwoTrackResult[String, Error] = Success("Some Data")
    val failure: TwoTrackResult[String, Exception] = Failure(new UnsupportedOperationException)
    success.eq(failure) should be (false)
  }

  it should "be possible to pattern match on" in {
    val twoTrackResult: TwoTrackResult[Integer, String] = Success(1)
    val matchResult = twoTrackResult match {
      case Success(s) => s
      case Failure(f) => f
    }
    matchResult should be(1)
  }

  it should "pipe a two-track value into a switch function via the ->= operator" in {
    val validateMaxFiveChars: String => TwoTrackResult[String, String] = (s: String) => if (s.length > 5) scalarop.fail("Too long") else succeed(s)
    (succeed("input") ->= validateMaxFiveChars) should be(Success("input"))
    (succeed("Too many chars") ->= validateMaxFiveChars) should be(Failure("Too long"))
    (scalarop.fail("FAIL") ->= validateMaxFiveChars) should be(Failure("FAIL"))
  }

  it should "be possible to use in for comprehension" in {
    (for {
      a <- validateStringNotEmpty("abc")
      b <- validateStringNotTooLong(a)
    } yield b) should be(Success("abc"))
    (for {
      a <- validateStringNotEmpty("")
      b <- validateStringNotTooLong(a)
    } yield b) should be(Failure("String should not be empty"))
    (for {
      a <- validateStringNotEmpty("abcdefgh")
      b <- validateStringNotTooLong(a)
    } yield b) should be(Failure("String is too long"))
  }

  "Function succeed(data)" should "wrap the data in a Success object" in {
    val data = "Some data"
    succeed(data).isInstanceOf[Success[String, Nothing]] should be(true)
  }

  "Function fail(error)" should "wrap the error in a Failure object" in {
    val error = new UnsupportedOperationException
    scalarop.fail(error).isInstanceOf[Failure[Nothing, UnsupportedOperationException]] should be(true)
  }

  val successFunc = (i: Int) => Success("SuccessFunc called with input " + i)
  val failureFunc = (f: String) => Success("FailureFunc called with input " + f)
  "Function either(successFunc, failureFunc)(twoTrackInput)" should "apply the success function for Success() input" in {
    either(successFunc, failureFunc)(Success(5)) should be(Success("SuccessFunc called with input 5"))
  }

  it should "apply the failure function for Failure() input" in {
    either(successFunc, failureFunc)(Failure("FAILING")) should be(Success("FailureFunc called with input FAILING"))
  }

  "Function bind(f)" should "convert a switch function into a two-track function" in {
    bind((s: String) => succeed(s)).isInstanceOf[String => Success[String, Nothing]] should be(true)
  }

  "Operator >=>" should "compose two switch functions into another switch function" in {
    val validateStringNotEmptyAndNotTooLong = validateStringNotEmpty _ >=> validateStringNotTooLong
    validateStringNotEmptyAndNotTooLong.isInstanceOf[String => TwoTrackResult[String, String]] should be(true)
    validateStringNotEmptyAndNotTooLong("") should be(Failure("String should not be empty"))
    validateStringNotEmptyAndNotTooLong("123456") should be(Failure("String is too long"))
    validateStringNotEmptyAndNotTooLong("1234") should be(Success("1234"))
  }

  val oneTrackToUpperCase = (s: String) => s.toUpperCase
  "Function switch" should "convert a one-track function into a switch" in {
    val switchToUpperCase = switch(oneTrackToUpperCase)
    switchToUpperCase.isInstanceOf[String => TwoTrackResult[String, Nothing]] should be(true)
    switchToUpperCase("abc") should be(Success("ABC"))
    (Success("abc") ->= switchToUpperCase) should be(Success("ABC"))
    (Failure(123) ->= switchToUpperCase) should be(Failure(123))
  }

  "Function map" should "convert a one-track function into a two-track function" in {
    val twoTrackToUpperCase = map(oneTrackToUpperCase)
    twoTrackToUpperCase.isInstanceOf[TwoTrackResult[String, Nothing] => TwoTrackResult[String, Nothing]] should be(true)
    (Success("abc") ->> twoTrackToUpperCase) should be(Success("ABC"))
    (Failure(123) ->> twoTrackToUpperCase) should be(Failure(123))
  }

  val deadEndFunction = (b: Boolean) => if (b) throw new RuntimeException("b is true")
  "Function tee" should "convert a dead-end function into a one-track function" in {
    val oneTrackFunction = tee(deadEndFunction)
    oneTrackFunction.isInstanceOf[(Boolean) => Boolean] should be(true)
    oneTrackFunction(false) should be(false)
    a[RuntimeException] should be thrownBy {
      oneTrackFunction(true)
    }
  }

  val oneTrackFunctionWithException = tee(deadEndFunction)
  "Function tryCatch" should "convert a one-track function into a switch with exception handling" in {
    val switchWithExceptionHandling = tryCatch(oneTrackFunctionWithException, {
      "Got exception: " + _.getMessage
    })
    switchWithExceptionHandling.isInstanceOf[(Boolean) => TwoTrackResult[Boolean, String]] should be(true)
    switchWithExceptionHandling(false) should be(Success(false))
    switchWithExceptionHandling(true) should be(Failure("Got exception: b is true"))
  }

  val oneTrackThrowableToString = (t: Throwable) => t.getMessage
  "Function doubleMap" should "convert two one-track functions into a two-track function" in {
    val twoTrackFunction = doubleMap(oneTrackToUpperCase, oneTrackThrowableToString)
    twoTrackFunction.isInstanceOf[TwoTrackResult[String, Throwable] => TwoTrackResult[String, String]] should be(true)
    (Success("abc") ->> twoTrackFunction) should be(Success("ABC"))
    (Failure(new RuntimeException("Error")) ->> twoTrackFunction) should be(Failure("Error"))
  }

  "Function plus" should "add two switches in parallel" in {
    case class Input(firstName: String, lastName: String)
    def validateInputAttributeCannotBeBlank(dataAccessor: Input => String, attributeName: String)(input: Input) =
      if (dataAccessor(input).isEmpty) scalarop.fail(attributeName + " cannot be blank") else succeed(input)
    val switchFirstNameNotBlank = (input: Input) => input ->> validateInputAttributeCannotBeBlank(_.firstName, "FirstName")
    val switchLastNameNotBlank = (input: Input) => input ->> validateInputAttributeCannotBeBlank(_.lastName, "LastName")
    val takeLastSuccess = (s1: Input, s2: Input) => s2
    def accumulateFailures(fs: String*) = fs.toList
    val parallelSwitches = plus(takeLastSuccess, accumulateFailures)(switchFirstNameNotBlank, switchLastNameNotBlank)
    parallelSwitches.isInstanceOf[(Input) => TwoTrackResult[Input, List[String]]] should be(true)
    parallelSwitches(Input("first", "last")) should be(Success(Input("first", "last")))
    parallelSwitches(Input("", "last")) should be(Failure(List("FirstName cannot be blank")))
    parallelSwitches(Input("first", "")) should be(Failure(List("LastName cannot be blank")))
    parallelSwitches(Input("", "")) should be(Failure(List("FirstName cannot be blank", "LastName cannot be blank")))
  }
}
