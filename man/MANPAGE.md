% emd (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

emd - [Emily] Dickinson

# DESCRIPTION

**emd** is a text-generation language

# SYNOPSIS

  emd repl

  emd run literature.dck

# SUBCOMMANDS

**repl** - Start a repl

**run** - Run a file

**check** - Check that a program is correct without running it

**lint** - Give suggestions for common mistakes

# OPTIONS

**-h** **-\-help**
:   Display help

**-V** **-\-version**
:   Display version information

**-I** **-\-include**
:   Directory to search for libraries

# EDITOR INTEGRATION

A vim plugin is avaiable; see

https://github.com/vmchale/dickinson/tree/master/vim

# SHELL COMPLETIONS

To get shell completions in your current session:

`eval "$(emd --bash-completion-script emd)"`

Put this in your `~/.bashrc` or `~/.bash_profile` to install them.

# BUGS

Please report any bugs you may come across to
https://github.com/vmchale/dickinson/issues.

# COPYRIGHT

Copyright 2020. Vanessa McHale. All Rights Reserved.
