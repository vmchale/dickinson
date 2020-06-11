scriptencoding utf-8

if exists('b:current_syntax')
    finish
endif

syntax match dickinsonKeyword ":import"
syntax match dickinsonKeyword ":def"
syntax match dickinsonKeyword ":branch"
syntax match dickinsonKeyword ":oneof"

syntax match dickinsonComment "\v;.*$" contains=@Spell

highlight link dickinsonComment Comment
highlight link dickinsonKeyword Keyword
