The Jeeves programming language for automatically enforcing privacy policies implemented as an embedded domain specific language in Scala.

We have the following dependencies:

  * Scala 2.10.0.
  * SBT build tool version 0.10 or higher.
  * Z3 version 3.2. We have confirmed that version 4.3 does **not** work; we are looking into this issue.

For more instructions on installing Jeeves on different platforms, you may want to read [this](https://sites.google.com/site/jeevesprogramming/download).

To run the tests:

  * set smt.home to wherever you placed Z3 binary ("~/opt/z3" by default)
  * ` sbt update ` will pull in the library dependencies.
  * ` sbt test ` will run the tests.

See more installation instructions [here](http://code.google.com/p/jeeveslib/wiki/Installation).