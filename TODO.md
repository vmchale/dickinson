- [ ] Type system
- [ ] REPL! I would actually use this.
- [ ] spec
# Specification
- [ ] Grammar
- [ ] Types &c.
- [ ] Modules
# Documentation
- [ ] manpages
- [ ] Tutorial/walkthrough?
# Passes
- [ ] Typechecker?
- [ ] Warn if a string is duplicate?
- [x] Make sure there are no top-level name clashes
- [x] Scoping check (not lazy, like evaluation)
- [ ] Error on negative numbers? (useful error)
# Features
- [ ] Figure out mutual recursion? (or recursion)
- [ ] User-defined functions on text?
  - [ ] "character map" approach?
    - [ ] Requires expression application + type checker pass?
- [ ] Import/module system
- [ ] Multiline strings
- [ ] Tuples/multi-return & agreement.
  - [ ] Def need product types...
    - [ ] Dog example?
- [ ] Lambdas?
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
  - [ ] Actually fix identifiers/threading state to the lexer
# Tests
- [ ] Escpaed characters
- [ ] Nested interpolated strings
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
