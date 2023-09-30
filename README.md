# Mixql Core

[![codecov](https://codecov.io/gh/mixql/mixql-core/branch/develop/graph/badge.svg?token=6pvRE15adp)](https://codecov.io/gh/mixql/mixql-core)

## Mixql parametrs

Mixql parameters can be set in HOCON config file whitch is parsed by typesafe config. They also can be set using `org.mixql.core.context.Context.setVar` at runtime as any other variable. Avaliable params:

1) `mixql.error.skip` - `bool`, if `true` skips errors in engine execution. Default `false`

2) `mixql.execution.engine` - `string` name of current execution engine.

`mixql.variables.init` - special `object` only used in config file. All fields of this object is initial variables for context when started. For example in config file:
```
mixql.variables.init {
    intVariable = 42
}
```
when you start context you will have `int` variable with name intVariable and value 42. If `mixql.engine.variables.update` is `"all"` all engines will have this param too.