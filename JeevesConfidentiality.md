You may find some good examples of using the Jeeves library in the [test/scala/jeeves](http://code.google.com/p/jeeveslib/source/browse/#hg%2Fsrc%2Ftest%2Fscala%2Fjeeves) directory.

# Using Jeeves #
To use Jeeves, you need to have a class that extends the Scala trait `JeevesLib[C]`, where `C` is a type variable corresponding to the type of the output channel. A Scala trait is similar to a Java interface and is used to define an object type along with supported methods. The `JeevesLib[C]` trait defines the methods for creating sensitive values, adding policies, and concretizing sensitive expressions based on the output channel. Since this stores your constraint environment, you should extends `JeevesLib` in the class that creates and stores your objects containing sensitive values and locations. You will need to add a corresponding import:
```
import cap.jeeveslib.jeeves._
```

Sensitive values have operations defined for combining them. In Jeeves, types have corresponding "sensitive" types:
  * For integers, we have the type `IntExpr`.  (Note that for integer types, we use ` BigInt ` rather than `Int`.)
  * For Booleans, the sensitive type is `Formula`.
  * For objects of type `T`, the sensitive type is `ObjectExpr[T]`. To allow an object to have be used in an `ObjectExpr` the type must extend the `Atom` class, which defines the required operations for faceted evaluation.

## Creating Sensitive Values ##
We create sensitive values as follows:
  * Get a label by calling `mkLabel`.
  * Call the corresponding `mkSensitive` function: either `mkSensitiveInt` for integer expressions or `mkSensitive` for objects.
  * Add policies by calling `restrict`, which takes a label and a policy that has type `ObjectExpr[C]` and returns an expression of type `Formula`.

## Using Sensitive Values ##
The Jeeves library has overloaded some, but not all, operators for evaluating of expressions containing sensitive values. Boolean operators and accesses of object fields look the same. The following look different:
  * Equality on sensitive expressions should be done with `===` rather than `==`.
  * Accesses of integer fields of sensitive objects look like `objName~'fieldName` rather than `objName.fieldName`.
  * Conditionals on sensitive values should be evaluated using the function `jif[T]: Formula => (Unit => [T]) => (Unit => [T]) => T`, where `T` is the type of the result of evaluating the conditional branches.

## Outputting Sensitive Values ##
You can call concretize with a context and a sensitive expression in order to produce a "concrete" expression by calling `concretize(ctxt, expr)`. The expression corresponding `ctxt` needs to have type `ObjectExpr[C]`, where `C` is the same type with which you instantiated `JeevesLib`. The expression corresponding to `expr` can be any valid sensitive type.