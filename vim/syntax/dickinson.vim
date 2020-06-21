scriptencoding utf-8

if exists('b:current_syntax')
    finish
endif

syntax match dickinsonKeyword ":import"
syntax match dickinsonKeyword ":def"
syntax match dickinsonKeyword ":branch"
syntax match dickinsonKeyword ":oneof"
syntax match dickinsonKeyword ":let"
syntax match dickinsonKeyword ":lambda"
syntax match dickinsonKeyword ":match"

syntax match dickinsonNum "\v[0-9]+"
syntax match dickinsonNum "\v[0-9]+\.[0-9]+"

syntax match dickinsonIdentifier "\v[a-zA-Z]+"

syntax match dickinsonEsc +\\["\\n]+

syntax match dickinsonType "text"

syntax region dickinsonInterpolation start='${' end='}' contains=dickinsonIdentifier,dickinsonKeyword,dickinsonType

syntax region dickinsonString start=+"+ end=+"+ contains=@Spell,dickinsonEsc,dickinsonInterpolation

syntax match dickinsonComment "\v;.*$" contains=@Spell

syntax match dickinsonSymbol ">"
syntax match dickinsonSymbol "\$"

highlight link dickinsonComment Comment
highlight link dickinsonInterpolation Special
highlight link dickinsonKeyword Keyword
highlight link dickinsonNum Number
highlight link dickinsonIdentifier Identifier
highlight link dickinsonString String
highlight link dickinsonType Type
highlight link dickinsonSymbol Special

let b:current_syntax = 'dickinson'
