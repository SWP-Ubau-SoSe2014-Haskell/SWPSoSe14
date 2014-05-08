"Vim syntax file
"Language: Rail
"Maintainer: Kelvin

"Attention: This syntax highlighting works only for simple rail programms
if exists("b:current_syntax")
    finish
endif

let b:current_syntax = "rail"

syntax match rail "[\\/|\+\*-]"
highlight link rail Identifier

syntax match junction "[v^<>]"
highlight link junction Statement

syntax match system "[beiou?]"
highlight link system Operator

syntax match constant "[1234567890]"
syntax region constant start='\[' end='\]'
syntax region constant start='(!' end='!)'
highlight link constant Constant

syntax region function start='{' end='}'
syntax region function start='\'' end='\''
highlight link function Type

syntax match math "[admrs]"
highlight link math Operator

syntax match string "[cpz]"
highlight link string Operator

syntax match list "[n:~]"
highlight link list Operator

syntax match conditional "[fgqt]"
highlight link conditional Operator

syntax match misc "[$@#&]"
highlight link misc Keyword

"modified specification: comment is between '::' and '::'
syntax region comment start='(-' end='-)'
highlight link comment Comment
