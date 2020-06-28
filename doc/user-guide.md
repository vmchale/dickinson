# Dickinson User Guide

## Introduction

Dickinson is a text-generation language for generative literature. Each time you run your
code, you get back text. The text is chosen randomly based on your code and can
return something different each time.

## Program Structure

Dickinson programs begin with `%-`, followed by definitions.

### Example

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

which will display either `heads` or `tails`.

In general, when you `emd run` code, `emd` will display the results of evaluating `main`.

## REPL

## Examples
