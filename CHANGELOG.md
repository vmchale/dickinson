# dickinson

## 0.1.1.0

  * Export renamer in `Language.Dickinson`
  * Fix build with GHC ≤ 8.2.2
  * Add `man` subcommand
  * Introduce multiline strings

## 0.1.0.1

  * Fix source distribution so nix builds it automatically
  * Fix a bug in the evaluator that caused valid expressions to be rejected
    in the REPL
  * Show an error in the REPL when trying to `:view` a name not in scope.
  * Throw error when tuple pattern doesn't match type of expression
  * Fix `$` within strings so it doesn't need to be escaped

## 0.1.0.0

Initial release
