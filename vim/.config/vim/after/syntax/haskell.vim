" https://vi.stackexchange.com/questions/10781/extending-haskell-syntax-highlighting-to-allow-for-inline-c-snippets

" C syntax file won't load if this is set
unlet b:current_syntax

" Load C syntax
syn include @C syntax/c.vim

" Define a syntax region which can contain C syntax
syn region haskellC keepend
    \ start=/\v(\[C\.block\|)@<= \w+ \{/
    \ end=/} \|]/
    \ contains=@C

" Re-set the current syntax
let b:current_syntax = 'haskell'
