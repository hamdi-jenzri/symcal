[![Build Status](https://travis-ci.org/hamdi-jenzri/symcal.svg?branch=master)](https://travis-ci.org/hamdi-jenzri/symcal)

# symcal

Symbolic computation in Scala.

# Example usage

```scala
import com.github.symcal._

val x = Var('x)
val y = Var('y)
val z = Var('y)

val s = (x + y) * 2 * z

print(s.subs(x -> 3)) // prints "(3 + y) * 2 * z"

val t = s.subs(z -> 3).diff(y)
t.toInt // 6

```
