# Dickinson

Dickinson is a text-generation language.

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

## Documentation

A user guide is available in
[markdown](https://github.com/vmchale/dickinson/blob/master/doc/user-guide.md)
and as
a [pdf](https://github.com/vmchale/dickinson/blob/master/doc/user-guide.pdf).

See `man/emd.1` for man pages.

### Examples

A riff on the Unix fortune program is available
[here](https://github.com/vmchale/dickinson/blob/master/examples/fortune.dck).
Try

```
emd run examples/fortune.dck
```
