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
- [ ] Fortune teller
- [ ] Hot take generator
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
- [ ] Bug when running `test/demo/animal.dck` with renamer then eval
# Code Quality
- [ ] Kick tires on `:t` feature
# Features
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
# Tooling
- [x] REPL
  - [x] Actually fix identifiers/threading state to the lexer
  - [x] `:t` thing?
  - [ ] Completions!
# Tests
- [ ] Nested interpolated strings
- [ ] Renamer + pattern matching
- [ ] `examples/shakespeare.dck`
## Test cases
- [x] `test/eval/tyAnnot.dck` in the test suite
- [ ] Escpaed characters
- [ ] name shadowing & such
# Libraries
- [x] Colors
- [ ] Animals
  - [x] Birds
- [ ] builtins (capitalization &c.)
# Performance
- [ ] Bench typechecker
- [ ] Normalization-by-evaluation? NbE
- [ ] http://hackage.haskell.org/package/text-short
- [x] Benchmark renamer
- [x] `+RTS -Ax`
- [ ] https://github.com/AndrasKovacs/smalltt/blob/master/krakow-pres.pdf
