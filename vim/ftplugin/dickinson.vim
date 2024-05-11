setlocal commentstring=;\ %s

set smarttab

setl shiftwidth=2

set makeprg=emd\ ide\ %
set errorformat=%Eemd:\ %l:%c\ %m

fun! DickinsonCheck()
    exec 'silent make'
    exec 'redraw!'
    exec 'cw'
endfun

au BufWritePost *.dck call DickinsonCheck()
