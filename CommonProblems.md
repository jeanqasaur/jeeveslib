# Compile-time Errors #

  * Did you use `==` instead of `===`?  This could result in a long error mentioning `position.line`.
  * Are you accessing fields of symbolic values that actually exist? If not, you may get the error "value... is not a member of Symbolic."

# Run-time Errors #
## Inconsistent Model ##
  * Are you extending `Atom` for objects that may be involved in formulas? If not, you should be.

## Randomly Failing Policies ##
  * Are you using `BigInt` (instead of `Int`) for integers that may be involved in formulas?  If you use `Int`, things may randomly be 0.
  * When you concretize a `BigInt`, are you specifying `concretize[BigInt]`?  The system handles objects different from integers, so if there is ambiguity it could cause concretization to return `null`.