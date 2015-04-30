set nocompatible              " be iMproved, required
"filetype off                  " required

" set the runtime path to include Vundle and initialize
" set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
"Plugin 'gmarik/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
"Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
"Plugin 'L9'
" Git plugin not hosted on GitHub
"Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
" Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}

" All of your Plugins must be added before the following line
"call vundle#end()            " required
"filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

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

