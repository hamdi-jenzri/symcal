[![Build Status](https://travis-ci.org/hamdi-jenzri/symcal.svg?branch=master)](https://travis-ci.org/hamdi-jenzri/symcal)

# symcal

Symbolic computation in Scala.

# Example usage

```scala
import com.github.symcal._
import spire.implicits.IntAlgebra

val x = Var('x)
val y = Var('y)
val z = Var('z)

val s = (x + y) * 2 * z

print(s.subs(x, 3).print) // prints "(3 + y) * 2 * z"

val t = s.subs(z, 3).diff(y)
assert(t.toValue == 6)

```
