% emd (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

emd - [Emily] Dickinson

# DESCRIPTION

**Dickinson** is a text-generation language

# SYNOPSIS

  emd repl

  emd run literature.dck

  emd run project.dck -\-include lib

  man $(emd man)

# SUBCOMMANDS

**repl** - Start a repl

**run** - Run a file

**check** - Check that a program is correct without running it

**lint** - Give suggestions for common mistakes

**fmt** - Format Dickinson code

**typecheck** - Check that some code is well-typed

**man** - Point to where manpages are installed

## REPL COMMANDS

**:save** - Save curent state in a file

**:l** **:load** - Load a file

**:r** - Restore a REPL state stored in a file

**:q** **:quit** - Quit session

**:list** - List all names that are in scope

**:t** **:type** - Display the type of an expression

**:v** **:view** - Show the definition of a name

**:h** **:help** - Display help

# OPTIONS

**-h** **-\-help**
:   Display help

**-V** **-\-version**
:   Display version information

**-I** **-\-include**
:   Directory to search for libraries

# EDITOR INTEGRATION

A vim plugin is available; see

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
