- [ ] Type system
- [ ] Something about sno executable/snow white that came to me in a dream
# Specification
- [ ] Grammar
- [ ] Types &c.
  - [ ] Right now tc doesn't even work lol
- [ ] Modules
- [ ] Normalization
# Documentation
- [x] manpages
- [ ] Tutorial/walkthrough?
  - [ ] Getting started?
- [ ] User guide (see happy?)
- [ ] Library documentation
## Examples
- [ ] Poetry bot...? Rumi?
- [ ] Storytelling? RPG? idk
- [ ] https://www.nadyaprimak.com/blog/programming/poetry-bot/
- [x] Dog greeter
- [ ] Fortune teller/unix fortune
- [ ] Hot take generator
- [ ] Cowsay-as-a-function
# Editor Integration
- [x] Syntastic checker
# Passes
- [ ] Typechecker?
- [ ] Warn if a string is duplicate? (warn on duplicate branches)
- [x] Make sure there are no top-level name clashes
- [x] Scoping check (not lazy, like evaluation)
- [ ] Error on negative numbers? (useful error)
# Library API
- [x] Maybe spin out internal library?
# Bugs
- [x] Bug in `test/demo/animal.dck` - doesn't ever produce a bird?
- [ ] Running `examples/doggo.dck` `main` - `pick` seems to be called by `probabilities` state doesn't seem to change?
  - [ ] More vexingly, calling e.g. `color` in `lib/color.dck` un-sticks it!!
  - [ ] running `test/data/nestLet.dck` fucks up
- [ ] Bug when running `test/demo/animal.dck` with renamer then eval
- [x] `examples/doggo.dck` hangs indefinitely lol
- [ ] Hangs indefinitely when lexing incomplete string
# Code Quality
- [ ] Kick tires on `:t` feature
- [ ] Don't use the identifier `yeet`
# Debugging
- [ ] Verbosity
# Features
- [ ] Polyglot integration!
- [x] Change `:import` to `:include`!
- [ ] "append" repl command
- [ ] Circular import checker
- [x] Save repl states
- [ ] Formatter
- [ ] User-defined functions on text?
  - [ ] "character map" approach?
    - [ ] Requires expression application + type checker pass?
  - [ ] Filter? (e.g. vowels...)
- [x] Import/module system
  - [ ] Checkers should work with the module system
  - [ ] Modules don't give global uniqueness; fix scoping/deletions
- [ ] Sum types (`Boy | Girl`)
- [x] Tuples/multi-return & agreement.
  - [ ] Def need product types...
    - [x] Dog example?
- [x] Lambdas?
  - [ ] typechecker that works (with mutual recursion)
- [ ] let-bindings are kind of painful to use
- [x] Interpolated strings
- [ ] Builtins
  - [ ] Alliteration
  - [ ] Capitalization lol
- [ ] Escapes
- [ ] docs? links...?
- [ ] Normalization (for encoding, for one)
- [ ] Remove duplicates (for regen adjectives...)
- [x] Only allow imports at the beginning
- [ ] Install files globally?
# Tooling
- [x] REPL
  - [x] Actually fix identifiers/threading state to the lexer
  - [x] `:t` thing?
  - [ ] Completions!
  - [ ] linting within the REPL?
# Tests
- [ ] Nested interpolated strings
- [ ] Renamer + pattern matching
- [ ] `examples/shakespeare.dck`
- [ ] golden tests via pretty-simple packages (debug output?)
## Test cases
- [x] `test/eval/tyAnnot.dck` in the test suite
- [ ] Lexer (fail) tests
  - [ ] Capital type names...
- [ ] Escpaed characters
  - [ ] Pretty-print
- [ ] name shadowing & such
- [ ] property test -> generate, pretty-print, parse
- [ ] Something that kicks the tires on `tryEval` being called on `let` and
  `match`?
# Libraries
- [x] Colors
- [ ] Animals
  - [x] Birds
- [ ] builtins (capitalization &c.)
- [ ] Currying?
# Performance
- [ ] Load modules in a sensible way?
- [ ] Bench typechecker
- [ ] Normalization-by-evaluation? NbE
- [ ] http://hackage.haskell.org/package/text-short
- [x] Benchmark renamer
- [x] `+RTS -Ax`
- [ ] https://github.com/AndrasKovacs/smalltt/blob/master/krakow-pres.pdf
