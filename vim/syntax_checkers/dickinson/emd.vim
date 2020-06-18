if exists('g:loaded_syntastic_dickinson_emd_checker')
    finish
endif
let g:loaded_syntastic_dickinson_emd_checker = 1

let g:syntastic_dickinson_emd_exec = 'emd'

function! SyntaxCheckers_dickinson_emd_GetLocList() dict
    let makeprg = self.makeprgBuild({
                \ 'exe': self.getExec(),
                \ 'args': 'check',
                \ 'fname': shellescape(expand('%') )})

    let errorformat =
        \ 'emd: %l:%c %m'

    let loclist = SyntasticMake({
            \ 'makeprg': makeprg,
            \ 'errorformat': errorformat })

    return loclist

endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'dickinson',
    \ 'name': 'emd' })
