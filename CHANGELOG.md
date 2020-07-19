# dickinson

# 1.0.0.1

  * Fix bug when calling `:flatten` on tuples.

# 1.0.0.0

  * Fix bug in typechecker
  * Pattern matching now has branches
  * Better error message when `:view` is called without any arguments
  * Introduce or-patterns

## 0.1.2.0

  * Fail on bad patterns, e.g. `(:match xy (x, x) x)`
  * Add `Language.Dickinson.QuasiQuoter` module

## 0.1.1.2

  * Ignore lines starting with `#!` (for shell shebangs)
  * Fix bug in evaluator which caused `:flatten` to work improperly.
  * Fix bug which caused `:match` within `:flatten` to work improperly.
  * Formatter is now usable; added `--inplace` flag to `emd fmt`

## 0.1.1.1

  * Run typechecker before evaluator in executable/REPL
  * Add location info to type errors
  * `emd FILE` now runs file (for use in shell shebangs)

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
