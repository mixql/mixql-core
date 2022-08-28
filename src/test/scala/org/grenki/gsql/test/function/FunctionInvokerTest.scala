package org.grenki.gsql.test.function

import org.grenki.gsql.function.FunctionInvoker
import org.scalatest.funsuite.AnyFunSuite

class FunctionInvokerTest extends AnyFunSuite {

  val length: Any = new (String => Int) {
    def apply(str: String): Int = str.length
  }

  val defArgFunc = new Object {
    def apply(str: String = "abc"): String = str
  }

  val twoDefaultArgFunc = new Object {
    def apply(a: String = "abc", b: String = "1234"): String = {
      a + b
    }
  }

  val defaultSecondArgFunc = new Object {
    def apply(a: String, b: String = "1234"): String = {
      a + b
    }
  }

  val substr = new ((String, Int, Int) => String) {
    def apply(str: String, pos: Int, len: Int = Int.MaxValue): String = {
      if (len == Int.MaxValue)
        str.substring(pos)
      else
        str.substring(pos, len)
    }
  }

  val functions: Map[String, Any] = Map.apply(
    "length" -> length,
    "def_arg_func" -> defArgFunc,
    "two_def_arg_func" -> twoDefaultArgFunc,
    "default_second_arg_func" -> defaultSecondArgFunc,
    "substr" -> substr,
  )

  test("Invoke anonymous function") {
    val res = FunctionInvoker.invoke(functions, "length", List("123"))
    assert(res == 3)
  }

  test("Invoke default argument function") {
    val res = FunctionInvoker.invoke(functions, "def_arg_func", Nil)
    assert(res == "abc")
  }

  test("Invoke two default argument function") {
    val res = FunctionInvoker.invoke(functions, "two_def_arg_func", Nil)
    assert(res == "abc1234")
  }

  test("Invoke function with default second argument") {
    val res = FunctionInvoker.invoke(functions, "default_second_arg_func", List("abc"))
    assert(res == "abc1234")
  }

  test("Invoke function with arguments was passed by name") {
    val res = FunctionInvoker.invoke(
      functions,
      "two_def_arg_func",
      Nil,
      Map("a" -> "qw", "b" -> "erty"))

    assert(res == "qwerty")
  }

  test("substr") {
    val res = FunctionInvoker.invoke(
      functions,
      "substr",
      List("123545", 3, 5)
    )

    assert(res == "123545".substring(3, 5))
  }

  test("substr2") {
    val res = FunctionInvoker.invoke(
      functions,
      "substr",
      List("123545", 3)
    )

    assert(res == "123545".substring(3))
  }

  test("Invoke undefined function") {
    val thrown = intercept[RuntimeException] {
      FunctionInvoker.invoke(functions, "foo", List("bar"))
    }

    assert(thrown.getMessage == "Can't find function `foo`")
  }
}