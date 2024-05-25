setlocal commentstring=;\ %s

set smarttab

setl shiftwidth=2

set makeprg=emd\ ide\ %
set errorformat=%Eemd:\ %l:%c\ %m
set errorformat+=%Eemd:\ %m\ %trror\ at\ line\ %l\\,\ column\ %c

fun! DickinsonCheck()
    exec 'silent make'
    exec 'redraw!'
    exec 'cw'
endfun

au BufWritePost *.dck call DickinsonCheck()
