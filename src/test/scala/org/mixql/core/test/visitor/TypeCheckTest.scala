package org.mixql.core.test.visitor

import org.mixql.core.test.MainVisitorBaseTest
import org.mixql.core.context.mtype._
import org.mixql.core.function.MLambda
import org.mixql.core.exception.MException

class TypeCheckTest extends MainVisitorBaseTest {

  test("Test is none") {
    val code =
      """
        |let a = None;
        |let res1 = $a is NONE;
        |let res2 = $a is not NONE;
        |let res3 =
        |   null is NONE
        |   or true is NONE
        |   or 10 is NONE
        |   or 11.5 is NONE
        |   or "str" is NONE
        |   or [1,2] is NONE
        |   or {"a": 1} is NONE
        |   or () -> begin return 0; end is NONE;
        |let res4 =
        |   null is not NONE
        |   and true is not NONE
        |   and 10 is not NONE
        |   and 11.5 is not NONE
        |   and "str" is not NONE
        |   and [1,2] is not NONE
        |   and {"a": 1} is not NONE
        |   and () -> begin return 0; end is not NONE;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is null") {
    val code =
      """
        |let a = NULL;
        |let res1 = $a is NULL;
        |let res2 = $a is not NULL;
        |let res3 =
        |   none is NULL
        |   or true is NULL
        |   or 10 is NULL
        |   or 11.5 is NULL
        |   or "str" is NULL
        |   or [1,2] is NULL
        |   or {"a": 1} is NULL
        |   or () -> begin return 0; end is NULL;
        |let res4 =
        |   none is not NULL
        |   and true is not NULL
        |   and 10 is not NULL
        |   and 11.5 is not NULL
        |   and "str" is not NULL
        |   and [1,2] is not NULL
        |   and {"a": 1} is not NULL
        |   and () -> begin return 0; end is not NULL;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is bool") {
    val code =
      """
        |let a = False;
        |let res1 = $a is BOOL;
        |let res2 = $a is not BOOL;
        |let res3 =
        |   null is BOOL
        |   or none is BOOL
        |   or 10 is BOOL
        |   or 11.5 is BOOL
        |   or "str" is BOOL
        |   or [1,2] is BOOL
        |   or {"a": 1} is BOOL
        |   or () -> begin return 0; end is BOOL;
        |let res4 =
        |   null is not BOOL
        |   and none is not BOOL
        |   and 10 is not BOOL
        |   and 11.5 is not BOOL
        |   and "str" is not BOOL
        |   and [1,2] is not BOOL
        |   and {"a": 1} is not BOOL
        |   and () -> begin return 0; end is not BOOL;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is int") {
    val code =
      """
        |let a = 10;
        |let res1 = $a is INT;
        |let res2 = $a is not INT;
        |let res3 =
        |   null is INT
        |   or none is INT
        |   or true is INT
        |   or 11.5 is INT
        |   or "str" is INT
        |   or [1,2] is INT
        |   or {"a": 1} is INT
        |   or () -> begin return 0; end is INT;
        |let res4 =
        |   null is not INT
        |   and none is not INT
        |   and true is not INT
        |   and 11.5 is not INT
        |   and "str" is not INT
        |   and [1,2] is not INT
        |   and {"a": 1} is not INT
        |   and () -> begin return 0; end is not INT;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is double") {
    val code =
      """
        |let a = 10.5;
        |let res1 = $a is DOUBLE;
        |let res2 = $a is not DOUBLE;
        |let res3 =
        |   null is DOUBLE
        |   or none is DOUBLE
        |   or true is DOUBLE
        |   or 10 is DOUBLE
        |   or "str" is DOUBLE
        |   or [1,2] is DOUBLE
        |   or {"a": 1} is DOUBLE
        |   or () -> begin return 0; end is DOUBLE;
        |let res4 =
        |   null is not DOUBLE
        |   and none is not DOUBLE
        |   and true is not DOUBLE
        |   and 10 is not DOUBLE
        |   and "str" is not DOUBLE
        |   and [1,2] is not DOUBLE
        |   and {"a": 1} is not DOUBLE
        |   and () -> begin return 0; end is not DOUBLE;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is string") {
    val code =
      """
        |let a = "str";
        |let res1 = $a is STRING;
        |let res2 = $a is not STRING;
        |let res3 =
        |   null is STRING
        |   or none is STRING
        |   or true is STRING
        |   or 10 is STRING
        |   or 11.5 is STRING
        |   or [1,2] is STRING
        |   or {"a": 1} is STRING
        |   or () -> begin return 0; end is STRING;
        |let res4 =
        |   null is not STRING
        |   and none is not STRING
        |   and true is not STRING
        |   and 10 is not STRING
        |   and 11.5 is not STRING
        |   and [1,2] is not STRING
        |   and {"a": 1} is not STRING
        |   and () -> begin return 0; end is not STRING;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is array") {
    val code =
      """
        |let a = [1, 2];
        |let res1 = $a is ARRAY;
        |let res2 = $a is not ARRAY;
        |let res3 =
        |   null is ARRAY
        |   or none is ARRAY
        |   or true is ARRAY
        |   or 10 is ARRAY
        |   or 11.5 is ARRAY
        |   or "str" is ARRAY
        |   or {"a": 1} is ARRAY
        |   or () -> begin return 0; end is ARRAY;
        |let res4 =
        |   null is not ARRAY
        |   and none is not ARRAY
        |   and true is not ARRAY
        |   and 10 is not ARRAY
        |   and 11.5 is not ARRAY
        |   and "str" is not ARRAY
        |   and {"a": 1} is not ARRAY
        |   and () -> begin return 0; end is not ARRAY;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is map") {
    val code =
      """
        |let a = {"a": 1};
        |let res1 = $a is MAP;
        |let res2 = $a is not MAP;
        |let res3 =
        |   null is MAP
        |   or none is MAP
        |   or true is MAP
        |   or 10 is MAP
        |   or 11.5 is MAP
        |   or "str" is MAP
        |   or [1,2] is MAP
        |   or () -> begin return 0; end is MAP;
        |let res4 =
        |   null is not MAP
        |   and none is not MAP
        |   and true is not MAP
        |   and 10 is not MAP
        |   and 11.5 is not MAP
        |   and "str" is not MAP
        |   and [1,2] is not MAP
        |   and () -> begin return 0; end is not MAP;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is lambda") {
    val code =
      """
        |let a = () -> begin
        |  return 0;
        |end;
        |let res1 = $a is FUNCTION;
        |let res2 = $a is not FUNCTION;
        |let res3 =
        |   null is FUNCTION
        |   or none is FUNCTION
        |   or true is FUNCTION
        |   or 10 is FUNCTION
        |   or 11.5 is FUNCTION
        |   or "str" is FUNCTION
        |   or [1,2] is FUNCTION
        |   or {"a": 1} is FUNCTION;
        |let res4 =
        |   null is not FUNCTION
        |   and none is not FUNCTION
        |   and true is not FUNCTION
        |   and 10 is not FUNCTION
        |   and 11.5 is not FUNCTION
        |   and "str" is not FUNCTION
        |   and [1,2] is not FUNCTION
        |   and {"a": 1} is not FUNCTION;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is async") {
    val code =
      """
        |let a = async
        |  return 0;
        |end async;
        |let res1 = $a is ASYNC;
        |let res2 = $a is not ASYNC;
        |let res3 =
        |   null is ASYNC
        |   or none is ASYNC
        |   or true is ASYNC
        |   or 10 is ASYNC
        |   or 11.5 is ASYNC
        |   or "str" is ASYNC
        |   or [1,2] is ASYNC
        |   or {"a": 1} is ASYNC
        |   or () -> begin return 0; end is ASYNC;
        |let res4 =
        |   null is not ASYNC
        |   and none is not ASYNC
        |   and true is not ASYNC
        |   and 10 is not ASYNC
        |   and 11.5 is not ASYNC
        |   and "str" is not ASYNC
        |   and [1,2] is not ASYNC
        |   and {"a": 1} is not ASYNC
        |   and () -> begin return 0; end is not ASYNC;
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
    val res3 = context.getVar("res3")
    assert(res3 == MBool.False)
    val res4 = context.getVar("res4")
    assert(res4 == MBool.True)
  }

  test("Test is error") {
    val code =
      """
        |TRY
        |  raise "gg", "wp";
        |CATCH ex THEN
        |  let res1 = $ex is ERROR;
        |  let res2 = $ex is not ERROR;
        |END
                """.stripMargin
    val context = runMainVisitor(code)
    val res1 = context.getVar("res1")
    assert(res1 == MBool.True)
    val res2 = context.getVar("res2")
    assert(res2 == MBool.False)
  }
}
