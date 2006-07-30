" Vim syntax file
" Language:     AIFAD
" Filenames:    *.ads *.add
" Maintainers:  Markus Mottl      <markus.mottl@gmail.com>
" URL:          http://www.ocaml.info/vim/syntax/aifad.vim
" Last Change:  2002 Sep 27 - Started (MM)

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" AIFAD is case sensitive.
syn case match

" Errors
syn match    aifadParenErr   ")"
syn match    aifadCommentErr "\*)"

syn cluster  aifadContained contains=ocamlTodo

" Enclosing delimiters
syn region   aifadEncl transparent matchgroup=aifadKeyword start="(" matchgroup=aifadKeyword end=")" contains=ALLBUT,@aifadContained,aifadParenErr

" Comments
syn region   aifadComment start="^#" end="$" contains=aifadTodo
syn keyword  aifadTodo contained TODO FIXME XXX

" Special Keywords
syn keyword  aifadSpecialType  domain codomain

" Keychars
syn match    aifadKeyChar      "->"
syn match    aifadKeyChar      "\."
syn match    aifadKeyChar      "\*"
syn match    aifadKeyChar      ","
syn match    aifadKeyChar      "|"

" Constructors
syn match    aifadConstructor  "\u\(\w\|'\)*\>"

" Type names
syn match    aifadType /\<\(\l\|_\)\(\w\|'\)*\>/

" Synchronization
syn sync minlines=50
syn sync maxlines=500

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_aifad_syntax_inits")
  if version < 508
    let did_aifad_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink aifadParenErr     Error
  HiLink aifadCommentErr   Error
  HiLink aifadComment      Comment

  HiLink aifadConstructor  Constant

  HiLink aifadKeyword      Keyword
  HiLink aifadKeyChar      Keyword

  HiLink aifadType         Type
  HiLink aifadSpecialType  Include

  HiLink aifadTodo         Todo

  HiLink aifadEncl         Keyword

  delcommand HiLink
endif

let b:current_syntax = "aifad"

" vim: ts=28
