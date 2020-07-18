setlocal commentstring=;\ %s

set smarttab

setl shiftwidth=2

" use emd as a checker
let g:syntastic_dickinson_checkers = [ 'emd' ]
