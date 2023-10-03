package org.mixql.core.test.function

import org.mixql.core.context.Context
import org.mixql.core.context.mtype._
import org.mixql.core.engine.Engine
import org.mixql.core.function.FunctionInvoker
import org.mixql.core.test.engines.StubEngine
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.Seq

class CustomTestContext {
  def length = 100500
}

class FunctionInvokerTest extends AnyFunSuite {

  val length: Any =
    new (String => Int) {
      def apply(str: String): Int = str.length
    }

  val lengthCustomContext: Any =
    new ((CustomTestContext, String) => Int) {
      def apply(ctx: CustomTestContext, str: String): Int = str.length + ctx.length
    }

  val lengthMixQlCoreContext: Any =
    new ((Context, String) => Int) {
      def apply(ctx: Context, str: String): Int = str.length + ctx.getVar("a").asInstanceOf[MInt].getValue.toInt
    }

  val defArgFunc =
    new Object {
      def apply(str: String = "abc"): String = str
    }

  val variableArgFunc =
    new Object {
      def apply(strings: String*): Int = strings.length
    }

  val firstDefArgAndSecondVariableArgFunc =
    new Object {
      def apply(str: String, ints: Int*): String = str + ints.toList.sum.toString
    }

  val firstAndSecondDefArgAndThirdVariableArgFunc =
    new Object {
      def apply(str1: String, str2: String, ints: Int*): String = str1 + str2 + ints.toList.sum.toString
    }

  val twoDefaultArgFunc =
    new Object {

      def apply(a: String = "abc", b: String = "1234"): String = {
        a + b
      }
    }

  val defaultSecondArgFunc =
    new Object {

      def apply(a: String, b: String = "1234"): String = {
        a + b
      }
    }

  val substr =
    new ((String, Int, Int) => String) {

      def apply(str: String, pos: Int, len: Int = Int.MaxValue): String = {
        if (len == Int.MaxValue)
          str.substring(pos)
        else
          str.substring(pos, len)
      }
    }

  val decInt =
    new (Int => Int) {
      def apply(num: Int): Int = num - 1
    }

  val decListOfInt =
    new Object {

      def apply(ints: Int*): String = {
        ints.map(_ - 1).mkString(" ")
      }
    }

  val decListOfString =
    new Object {

      def apply(str: String*): String = {
        str.map(_.dropRight(1)).mkString(" ")
      }
    }

  val decListOfListOfString =
    new Object {

      def apply(str: Seq[String]*): String = {
        str.flatMap(x => x.map(y => y.dropRight(1))).mkString(" ")
      }
    }

  val decSting =
    new (String => String) {
      def apply(str: String): String = str.dropRight(1)
    }

  val decDouble =
    new (Double => Double) {
      def apply(f: Double): Double = f - 1
    }

  val functions: Map[String, Any] = Map.apply(
    "length" -> length,
    "length_of_custom_context" ->
      lengthCustomContext,
    "length_with_mixql_core_context" ->
      lengthMixQlCoreContext,
    "def_arg_func" -> defArgFunc,
    "two_def_arg_func" ->
      twoDefaultArgFunc,
    "default_second_arg_func" ->
      defaultSecondArgFunc,
    "substr" -> substr,
    "variable_number_of_args" ->
      variableArgFunc,
    "first_def_arg_and_second_variable_args" ->
      firstDefArgAndSecondVariableArgFunc,
    "first_and_second_def_arg_and_third_variable_args" ->
      firstAndSecondDefArgAndThirdVariableArgFunc,
    "dec" ->
      List(decInt, decListOfInt, decListOfString, decSting, decDouble)
  )

  test("Invoke anonymous function") {
    val res = FunctionInvoker.invoke(functions, "length", null, List("123"))
    assert(res == 3)
  }

  test("Invoke anonymous function length_of_custom_context with not mixql-core context") {
    val res = FunctionInvoker
      .invoke(functions, "length_of_custom_context", List[Object](new CustomTestContext), List("123"))
    assert(res == 100503)
  }

  test("Invoke anonymous function length_with_mixql_core_context with mixql-core context") {
    import scala.collection.mutable.{Map => MutMap}
    val context = Context(MutMap[String, Engine]("stub" -> new StubEngine), "stub")
    context.setVar("a", new MInt(12))
    val res = {
      FunctionInvoker.invoke(functions, "length_with_mixql_core_context", List[Object](context), List("123"))
    }
    assert(res == 15)
  }

  test("Invoke default argument function") {
    val res = FunctionInvoker.invoke(functions, "def_arg_func", null, Nil)
    assert(res == "abc")
  }

  test("Invoke two default argument function") {
    val res = FunctionInvoker.invoke(functions, "two_def_arg_func", null, Nil)
    assert(res == "abc1234")
  }

  test("Invoke function with default second argument") {
    val res = FunctionInvoker.invoke(functions, "default_second_arg_func", null, List("abc"))
    assert(res == "abc1234")
  }

  test("Invoke function with arguments was passed by name") {
    val res = FunctionInvoker
      .invoke(functions, "two_def_arg_func", List[Object](), Nil, Map("a" -> "qw", "b" -> "erty"))

    assert(res == "qwerty")
  }

  test("substr") {
    val res = FunctionInvoker.invoke(functions, "substr", null, List("123545", 3, 5))

    assert(res == "123545".substring(3, 5))
  }

  test("substr2") {
    val res = FunctionInvoker.invoke(functions, "substr", null, List("123545", 3))

    assert(res == "123545".substring(3))
  }

  test("variable_number_of_args") {
    val res = FunctionInvoker.invoke(functions, "variable_number_of_args", null, List("1", "2", "3", "4"))

    assert(res == 4)
  }

  test("first_def_arg_and_second_variable_args") {
    val res = FunctionInvoker.invoke(functions, "first_def_arg_and_second_variable_args", null, List("ABC", 1, 2, 3))

    assert(res == "ABC6")
  }

  test("first_and_second_def_arg_and_third_variable_args") {
    val res = FunctionInvoker
      .invoke(functions, "first_and_second_def_arg_and_third_variable_args", null, List("ABC", "DE", 1, 2, 3, 4))

    assert(res == "ABCDE10")
  }

  test("Invoke overloading function[Int]") {
    val res = FunctionInvoker.invoke(functions, "dec", null, List(5))
    assert(res == 4)
  }

  test("Invoke overloading function[String]") {
    val res = FunctionInvoker.invoke(functions, "dec", null, List("abc")).asInstanceOf[String]

    assert(res == "ab")
  }

  test("Invoke overloading function[Double]") {
    val res = FunctionInvoker.invoke(functions, "dec", null, List(1.1)).asInstanceOf[Double]

    assert((res - 0.1) < 0.0000000001)
  }

  test("Invoke overloading function[List[Int]]") {
    val res = FunctionInvoker.invoke(functions, "dec", null, List(1, 2, 3)).asInstanceOf[String]

    assert(res == "0 1 2")
  }

  ignore("Invoke overloading function[List[String]]") {
    val res = FunctionInvoker.invoke(functions, "dec", null, List("ab", "cd", "ef")).asInstanceOf[String]

    assert(res == "a c e")
  }

  ignore("Invoke overloading function[List[List[String]]]") {
    val res = FunctionInvoker.invoke(functions, "dec", null, List(List("ab", "cd"), List("ef"))).asInstanceOf[String]

    assert(res == "a c e")
  }
}
