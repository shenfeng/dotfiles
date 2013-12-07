set nocompatible
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

set clipboard+=unnamed	" Yanks go on clipboard instead.

" set number
set history=200

" Set the textwidth to be 120 chars
set textwidth=120

" Make the command-line completion better
set wildmenu

:let mapleader = ","
" map <leader>e :tabe<SPACE>
" map <Tab> :tabn<CR>
" map <S-Tab> :tabN<CR>

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
" set statusline=\ %{HasPaste()}%F%m%r%h%y\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:%l/%L[%p%%]\ Col:%c\ Buf:%n\ [%b][0x%B]

set statusline=%F%m%r%h%w\ (%{&ff}){%Y}[%l,%v][%p%%]\ %{strftime(\"%d/%m/%y\ -\ %H:%M\")}

function! CurDir()
	let curdir = substitute(getcwd(), '/Users/amir/', "~/", "g")
	return curdir
endfunction

function! HasPaste()
	if &paste
		return 'PASTE MODE	'
	else
		return ''
	endif
endfunction


set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines
set linebreak

set hlsearch
set incsearch

set diffopt=iwhite,iwhite,filler,vertical
" :set number

" :colorscheme zenburn


" Highlight all instances of the current word under the cursor
" nmap <silent> ^ :setl hls<CR>:let @/="<C-r><C-w>"<CR>
" nmap <silent> ,sv :so ~/.vimrc<CR>
" nmap <silent> ,ev :e ~/.vimrc<CR>


" toggle show invisible charactors quickly
" http://vimcasts.org/episodes/show-invisibles/
" set listchars=tab:▸\ ,eol:¬
set list
set listchars=tab:▸\ ,trail:▫
nmap <leader>l :set list!<CR>

" http://vimcasts.org/episodes/tabs-and-spaces/
" To insert space characters whenever the tab key is pressed
:set expandtab
" the number of space characters that will be inserted
" when the tab key is pressed
:set tabstop=4
" delete by tab
:set softtabstop=4
" change the number of space characters inserted for indentation
:set shiftwidth=4

" http://vimcasts.org/episodes/whitespace-preferences-and-filetypes/
if has("autocmd")
	autocmd FileType javascript setlocal noexpandtab
	" find filetype by using :set filetype?
	autocmd FileType go setlocal noexpandtab

	" :setfiletype xml
	" autocmd BufNewFile,BufRead *.rss,*.atom setfiletype xml
endif

" http://vimcasts.org/episodes/tidying-whitespace/
" set [no]expand & :setab!; select and :setab!
" :%s/\s\+$//e
function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction
autocmd BufWritePre *.py,*.js :call <SID>StripTrailingWhitespaces()

if has('gui_running')
    set guifont=Monaco\ 12
endif

map <C-j> <C-w>j
map <C-l> <C-w>l
map <C-h> <C-w>h
map <C-k> <C-w>k

" map <D-S-]> gt
" map <D-S-{> gT
map <C-1> 1gt

set statusline=\ %f%m%r%h%w\ %=%({%{&ff}\|%{(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\")}%k\|%Y}%)\ %([%l,%v][%p%%]\ %)
