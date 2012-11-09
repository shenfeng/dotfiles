syntax on
filetype on
filetype plugin on
filetype indent on
filetype plugin indent on
" when file change, auto load it
set autoread

" ignore case when searching is all lowcase,
" but recognizes uppercase if it's specified
set ignorecase
set smartcase

" set number
set history=200

" Set the textwidth to be 120 chars
set textwidth=80

" Make the command-line completion better
set wildmenu

:let mapleader = ","
map <leader>e :tabe<SPACE>
map <Tab> :tabn<CR>
map <S-Tab> :tabN<CR>

set tags=./tags,~/tags

" Show the current command in the lower right corner
set showcmd
"
" " Show the current mode
set showmode

" Make command line two lines high
set ch=2

" Turn backup off, since most stuff is in SVN, git anyway...
set nobackup
set nowb "writebackup
set noswapfile

" When the page starts to scroll, keep the cursor 8 lines from the top and 8
" lines from the bottom
set scrolloff=8
""""""""""""""""""""""""""""""
" => Statusline
""""""""""""""""""""""""""""""
" Always show the statusline
set laststatus=2

" Format the statusline
set statusline=\ %{HasPaste()}%F%m%r%h%y\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]

function! CurDir()
    let curdir = substitute(getcwd(), '/Users/amir/', "~/", "g")
    return curdir
endfunction

function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    else
        return ''
    endif
endfunction


set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines

set hlsearch
set incsearch

" To insert space characters whenever the tab key is pressed
:set expandtab
" affect the existing tab characters
" :retab

" the number of space characters that will be inserted
" when the tab key is pressed
:set tabstop=4

set diffopt=iwhite,iwhite,filler,vertical
" :set number

" change the number of space characters inserted for indentation
:set shiftwidth=4

let g:zenburn_high_Contrast=1
" :colorscheme zenburn


" Highlight all instances of the current word under the cursor
nmap <silent> ^ :setl hls<CR>:let @/="<C-r><C-w>"<CR>
nmap <silent> ,sv :so ~/.vimrc<CR>
nmap <silent> ,ev :e ~/.vimrc<CR>

":Tlist
let Tlist_Use_Right_Window=1
let Tlist_WinWidth=70

function! DoPrettyXML()
  " save the filetype so we can restore it later
  let l:origft = &ft
  set ft=
  " delete the xml header if it exists. This will
  " permit us to surround the document with fake tags
  " without creating invalid xml.
  1s/<?xml .*?>//e
  " insert fake tags around the entire document.
  " This will permit us to pretty-format excerpts of
  " XML that may contain multiple top-level elements.
  0put ='<PrettyXML>'
  $put ='</PrettyXML>'
  silent %!xmllint --format -
  " xmllint will insert an <?xml?> header. it's easy enough to delete
  " if you don't want it.
  " delete the fake tags
  2d
  $d
  " restore the 'normal' indentation, which is one extra level
  " too deep due to the extra tags we wrapped around the document.
  silent %<
  " back to home
  1
  " restore the filetype
  exe "set ft=" . l:origft
endfunction
command! PrettyXML call DoPrettyXML()
