% Dickinson User Guide
% Vanessa McHale

# Introduction

Dickinson is a text-generation language for generative literature. Each time you run your
code, you get back text. The text is generated randomly.

# Installing Dickinson

First, install [cabal](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html). Then:

```
cabal install language-dickinson
```

You may also want to install manpages for reference information about `emd`.

## Editor Integration

A [vim plugin](https://github.com/vmchale/dickinson/tree/master/vim) is
available.

# Program Structure

Dickinson files begin with `%-`, followed by definitions.

## Example

Here is a simple Dickinson program:

```
%-

(:def main
  (:oneof
    (| "heads")
    (| "tails")))
```

Save this as `gambling.dck`. Then:

```
emd run gambling.dck
```

which will display either `heads` or `tails`. The `:oneof` construct selects one
of its branches with equal probability.

In general, when you `emd run` code, `emd` will display the result of evaluating `main`.

## Definitions + Names

We can define names and reference them later:

```
%-

(:def gambling
  (:oneof
    (| "heads")
    (| "tails")))

(:def main
  gambling)
```

We can `emd run` this to the same results.

## Interpolation

We can reference and recombine past definitions via string interpolation:

```
(:def adjective
  (:oneof
    (| "beautiful")
    (| "auspicious")
    (| "cold")))

(:def main
  "What a ${adjective}, ${adjective} day!")
```

# REPL

To enter a REPL:

```
emd repl
```

This will show a prompt

```
emd>
```

If we have

```
%-

(:def gambling
  (:oneof
    (| "heads")
    (| "tails")))
```

in a file `gambling.dck` as above, we can load it with

```
emd> :l gambling.dck
```

We can then evaluate `gambling` if we like

```
emd> gambling
```

or manipulate names that are in scope, viz.

```
emd> "The result of the coin toss is: ${gambling}"
```

We can also create new definitions:

```
emd> (:def announcer "RESULT: ${gambling}")
emd> announcer
```

## Saving + Restoring States

We can save the REPL state, including any defintions we've declared during the
session.

```
emd> :save replSt.emdi
```

If we exit the session we can restore the save definitions with

```
emd> :r replSt.emdi
emd> announcer
```

For reference information about the Dickinson REPL:

```
:help
```

# Libraries

Dickinson allows pulling in definitions from other files with `:include`. 

## Using Libraries

### Example

The `color` module is bundled by default:

```
(:include color)

%-

(:def main
  "Today's mood is ${color}")
```

The `:include` must come before the `%-`; definitions come after the
`%-` as above.

`color.dck` contains:

```
%-

(:def color
    (:oneof
        (| "aubergine")
        (| "cerulean")
        (| "azure")
        ...
```

## Writing Libraries

# Examples
