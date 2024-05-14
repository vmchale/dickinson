% emd (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

emd - [Emily] Dickinson

# DESCRIPTION

**Dickinson** is a text-generation language

# SYNOPSIS

  emd fortune.dck

  emd repl

  emd run literature.dck

  emd run project.dck -\-include lib

  man $(emd man)

## SHEBANG

Dickinson ignores lines starting with `#!` so we can put

```
#!/usr/bin/env emd
```

at the top of a file and `emd` will be used as an interpreter.

# SUBCOMMANDS

**repl** - Start a repl

**run** - Run a file

**check** - Check that a program is correct without running it

**lint** - Give suggestions for common mistakes

**fmt** - Format Dickinson code

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

# INFLUENTIAL ENVIRONMENT VARIABLES

`DCK_PATH` - a colon-separated list of directories to search

# EDITOR INTEGRATION

A vim plugin is available; see

https://github.com/vmchale/dickinson/tree/master/vim

## CTAGS

To configure exuberant ctags or universal ctags, put

    --langdef=DICKINSON
    --langmap=DICKINSON:.dck
    --regex-DICKINSON=/:def *([[:lower:]][[:alnum:]]+)/\1/f,function/
    --regex-DICKINSON=/tydecl *([[:lower:]][[:alnum:]]+) *=/\1/t,type/
    --regex-DICKINSON=/= *([[:upper:]][[:alnum:]_]+)/\1/t,type/
    --regex-DICKINSON=/\| *([[:upper:]][[:alnum:]_]+)/\1/t,type/

in the project .ctags

# SHELL COMPLETIONS

To get shell completions in your current session:

```
eval "$(emd --bash-completion-script emd)"
```

Put this in your `~/.bashrc` or `~/.bash_profile` to install them.

Shell completions are also available for fish and zsh; to get them:

`emd --fish-completion-script emd`

`emd --zsh-completion-script emd`

# BUGS

Please report any bugs you may come across to
https://github.com/vmchale/dickinson/issues.

# COPYRIGHT

Copyright 2020. Vanessa McHale. All Rights Reserved.
