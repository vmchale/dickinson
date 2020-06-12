scriptencoding utf-8

if exists('b:current_syntax')
    finish
endif

syntax match dickinsonKeyword ":import"
syntax match dickinsonKeyword ":def"
syntax match dickinsonKeyword ":branch"
syntax match dickinsonKeyword ":oneof"
syntax match dickinsonKeyword ":let"

syntax match dickinsonNum "\v[0-9]+"
syntax match dickinsonNum "\v[0-9]+\.[0-9]+"

syntax match dickinsonIdentifier "\v[a-zA-Z]+"

syntax match dickinsonEsc +\\["\\n]+

syntax region dickinsonString start=+"+ end=+"+ contains=@Spell,dickinsonEsc

syntax match dickinsonComment "\v;.*$" contains=@Spell

highlight link dickinsonComment Comment
highlight link dickinsonKeyword Keyword
highlight link dickinsonIdentifier Identifier
highlight link dickinsonString String
highlight link dickinsonNum Number

let b:current_syntax = 'dickinson'
