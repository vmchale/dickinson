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
## Examples
- [ ] Poetry bot...? Rumi?
- [ ] Storytelling? RPG? idk
- [ ] https://www.nadyaprimak.com/blog/programming/poetry-bot/
- [x] Dog greeter
# Editor Integration
- [ ] Syntastic checker
# Passes
- [ ] Typechecker?
- [ ] Warn if a string is duplicate? (warn on duplicate branches)
- [x] Make sure there are no top-level name clashes
- [x] Scoping check (not lazy, like evaluation)
- [ ] Error on negative numbers? (useful error)
# Library API
- [ ] Maybe spin out internal library?
# Bugs
- [ ] Bug in `test/demo/animal.dck` - overqualified names parsed with imports...
# Features
- [ ] Circular import checker
- [ ] Save repl states
- [ ] Formatter
- [ ] User-defined functions on text?
  - [ ] "character map" approach?
    - [ ] Requires expression application + type checker pass?
  - [ ] Filter? (e.g. vowels...)
- [x] Import/module system
  - [ ] Checkers should work with the module system
  - [ ] Modules don't give global uniqueness; fix scoping/deletions
- [ ] Tuples/multi-return & agreement.
  - [ ] Def need product types...
    - [ ] Dog example?
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
# Tooling
- [x] REPL
  - [x] Actually fix identifiers/threading state to the lexer
  - [ ] `:t` thing?
  - [ ] Completions!
# Tests
- [ ] Escpaed characters
- [ ] Nested interpolated strings
- [ ] Renamer + pattern matching
# Libraries
- [x] Colors
- [ ] Animals
  - [x] Birds
- [ ] builtins (capitalization &c.)
# Performance
- [ ] Normalization-by-evaluation? NbE
- [ ] http://hackage.haskell.org/package/text-short
- [x] Benchmark renamer
- [ ] `+RTS -Ax`
- [ ] https://github.com/AndrasKovacs/smalltt/blob/master/krakow-pres.pdf
