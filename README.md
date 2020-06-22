# Dickinson

Dickinson is a toy text-generation language.

## Installation

To install, first download [cabal-install](https://www.haskell.org/cabal/) and
[GHC](https://www.haskell.org/ghc/download.html). Then:

```
cabal install language-dickinson
```

### Editor Integration

Editor integration is available for vim.

Using [vim-plug](https://github.com/junegunn/vim-plug):

```vimscript
Plug 'vmchale/dickinson' , { 'rtp' : 'vim' }
```

## Technical

This uses a tree-walking interpreter as an experiment in environments and
immutable data structures.
