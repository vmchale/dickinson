# Dickinson User Guide

## Introduction

Dickinson is a text-generation for generative literature. Each time you run your
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

## REPL

## Examples

### Shakespearean Insult Generator
