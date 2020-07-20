- [ ] Something about sno executable/snow white that came to me in a dream
# Specification
- [ ] Grammar
- [ ] Types &c.
  - [ ] Right now tc doesn't even work lol
- [ ] Modules (inclusions lol)
- [ ] Normalization
# Documentation
- [x] manpages
  - [x] Document shebang trick
- [ ] `:flatten`
- [ ] Tutorial/walkthrough?
  - [ ] Getting started?
- [x] User guide (see happy?)
  - [ ] Lints and such
  - [ ] Bibliography
  - [ ] Or-patterns
    - [ ] Declension example?
  - [ ] Matching
- [ ] Library documentation
## Examples
- [ ] Poetry bot...? Rumi?
- [x] Fiona bot?
- [ ] Storytelling? RPG? idk
- [ ] https://www.nadyaprimak.com/blog/programming/poetry-bot/
- [ ] Dog greeter
- [x] Fortune teller/unix fortune
  - [ ] Fortune port? https://github.com/shlomif/fortune-mod/blob/master/fortune-mod/datfiles/disclaimer
- [ ] Hot take generator
- [x] Catherine of Siena bot
- [ ] Margery Kempe bot
- [x] Cowsay-as-a-function
- [ ] jenny holzer bot https://www.tate.org.uk/art/artworks/holzer-truisms-t03959 https://wordsofwomen.com/jenny-holzers-list-of-truisms/ https://www.moma.org/collection/works/63755
# Editor Integration
- [x] Syntastic checker
# Passes
- [ ] Typechecker?
- [x] Warn if a string is duplicate? (warn on duplicate branches)
- [x] Make sure there are no top-level name clashes
- [x] Scoping check (not lazy, like evaluation)
- [ ] Error on negative numbers? (useful error)
- [ ] Warn on or-patterns containing wildcards at the highest level
# Library API
- [x] Maybe spin out internal library?
# Bugs
- [x] `examples/fortune.dck` fails in repl
- [x] Bug in `test/demo/animal.dck` - doesn't ever produce a bird?
- [ ] Running `examples/doggo.dck` `main` - `pick` seems to be called by `probabilities` state doesn't seem to change?
  - [ ] More vexingly, calling e.g. `color` in `lib/color.dck` un-sticks it!!
- [x] Bug when running `test/demo/animal.dck` with renamer then eval
- [x] `examples/doggo.dck` hangs indefinitely lol
- [ ] Hangs indefinitely when lexing incomplete string
- [x]
  ```
  emd> tydecl sex = Boy | Girl
  emd> Boy
  1:1 Constructor Boy not found
  ```
- [x]
  ```
  emd> (:flatten "a")
  a
  emd> :type (:flatten "a")
  1:10 Unexpected
  ```
- [ ]
  ```
  emd> $decline (Feminine, Feminine, Singular)
  14:9 Constructor 'Nominative' has type 'case' but must be of type 'gender'
  ```
# Code Quality
- [ ] Kick tires on `:t` feature
# Debugging
- [ ] Verbosity
- [ ] Turn on sanity checking?
# Features
- [ ] Warn on redundant `:include`s
- [x] Polyglot integration!
- [x] Turn off zstd saving with a flag (for freebsd)
- [x] Change `:import` to `:include`!
- [ ] "append" repl command
- [ ] Circular import checker
- [x] Save repl states
- [ ] Formatter
- [ ] User-defined functions on text?
  - [ ] "character map" approach?
    - [ ] Requires expression application + type checker pass?
  - [ ] Filter? (e.g. vowels...)
- [ ] Import/module system
  - [x] Checkers should work with the module system
  - [ ] Modules don't give global uniqueness; fix scoping/deletions
- [x] Sum types (`Boy | Girl`)
  - [ ] Exhaustiveness checker
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
- [x] Escapes
- [ ] docs? links...?
- [ ] Normalization (for encoding, for one)
- [ ] Remove duplicates (for regen adjectives...)
- [x] Only allow imports at the beginning
- [ ] Install files globally?
## Distribution
- [ ] Binary distribution with `make install`? `language_dickinson_datadir`
- [ ] Debianization?
# Tooling
- [x] REPL
  - [x] Actually fix identifiers/threading state to the lexer
  - [x] `:t` thing?
  - [ ] Completions!
  - [ ] linting within the REPL?
# Tests
- [ ] Nested interpolated strings
- [ ] Renamer + pattern matching
- [x] `examples/shakespeare.dck`
- [x] golden tests via pretty-simple packages (debug output?)
- [ ] golden tests for error messages
## Test cases
- [x] `test/eval/tyAnnot.dck` in the test suite
- [ ] Lexer (fail) tests
  - [ ] Capital type names...
- [x] Escaped characters
  - [x] Pretty-print
- [ ] name shadowing & such
- [ ] property test -> generate, pretty-print, parse
- [ ] Interleaved multi-line string interpolations/strings/string interpolations
- [ ] Something that kicks the tires on `tryEval` being called on `let` and
  `match`?
# Libraries
- [x] Colors
- [x] Animals
  - [x] Birds
- [ ] builtins (capitalization &c.)
- [x] Currying?
# Performance
- [ ] Annotate with big-O notation!
- [ ] Figure out specific data types?
- [ ] Load modules in a sensible way?
- [x] Bench typechecker
- [ ] Normalization-by-evaluation? NbE
- [ ] http://hackage.haskell.org/package/text-short
- [x] Benchmark renamer
- [x] `+RTS -Ax`
- [ ] https://github.com/AndrasKovacs/smalltt/blob/master/krakow-pres.pdf
