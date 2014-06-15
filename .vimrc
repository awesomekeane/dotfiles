" os check

let os = ""

if has("gui_macvim")
    let os = "mac"
else
    let os = "win"
endif


" vundle
set nocompatible
filetype off

if os=="mac"
    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()
end

if os=="win"
    set rtp+=~/vimfiles/bundle/vundle/
    call vundle#rc("~/vimfiles/bundle")
    " to run same utilities in win
    set shell=cmd.exe
    set shellcmdflag=/C
    set path+=~/bin
endif

Bundle 'gmrik/vundle'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/syntastic'
Bundle 'SirVer/ultisnips'
Bundle 'honza/vim-snippets'
Bundle 'mileszs/ack.vim'
Bundle 'jeetsukumaran/vim-buffergator'
Bundle 'kien/ctrlp.vim'
Bundle 'tmhedberg/matchit'
Bundle 'scrooloose/nerdtree'
Bundle 'sjl/gundo.vim'
Bundle 'Raimondi/delimitMate'
Bundle 'taglist.vim'
Bundle 'andviro/ropevim-bundled'
Bundle 'plasticboy/vim-markdown'
Bundle 'marijnh/tern_for_vim'
Bundle 'Lokaltog/vim-easymotion'

" os specific bundles
if os=="mac"
    Bundle 'Valloric/YouCompleteMe'
    Bundle 'sjl/vitality.vim'
endif

" syntax
filetype on
filetype plugin indent on
syntax on
syntax enable
let g:syntastic_always_populate_loc_list=1

" formatting
set background=light
colorscheme solarized
if os=="mac"
    set guifont=Inconsolata:h18
endif
if os=="win"
    set guifont=Consolas:h10
endif
set ruler
set number numberwidth=2
set columns=86
set guioptions-=r
set guioptions-=T
set guioptions-=m
set showcmd
set scrolloff=1 sidescrolloff=5

" editing
set autoindent
set backspace=indent,eol,start
set noeb vb t_vb=
set hidden

" snipet trigger mapping
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"

" line wrap
set wrap linebreak nolist
set textwidth=0
set wrapmargin=0

" search
set ignorecase smartcase lazyredraw

" tab setting, tabstop, softtabstop, shiftwidth
set ts=4 sts=4 sw=4 expandtab smarttab
" filetypes that are fussy over tabs vs spaces
au Filetype make setlocal ts=8 sts=8 sw=8 noexpandtab
au Filetype yaml setlocal ts=2 sts=2 sw=2 expandtab


" backup setting
set backupdir=~/Documents/tmp/vim-tmp
set directory=~/Documents/tmp/vim-tmp

" autoload on change
set autoread

" highlight search
set hlsearch showmatch incsearch

" comma as leader
let mapleader = ","

" nerdtree
noremap <leader>f :NERDTree<cr>

" spell checking
nmap <silent> <leader>s :set spell!<CR>
set spelllang=en

" replace esc, in visual mode just press v again
inoremap jk <esc>
inoremap JK <esc>
inoremap Jk <esc>
inoremap jK <esc>
vnoremap v <esc>

" gundo
noremap <leader>u :GundoToggle<cr>

" quick save
noremap <leader>w :w!<cr>

" no highlight
noremap <leader><cr> :noh<cr>

" visual search and replace
vnoremap <silent> <leader>r :call VisualSelection('replace')<cr>

" window movements
noremap <leader>j <C-W>j
noremap <leader>k <C-W>k
noremap <leader>h <C-W>h
noremap <leader>l <C-W>l
" close window
noremap <leader>q <C-W>q
" this window only
noremap <leader>O :only<cr>

" quick editing
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%

" tab operations
noremap <C-]> gt
noremap <C-[> gT
noremap <C-1> 1gt
noremap <C-2> 2gt
noremap <C-3> 3gt
noremap <C-4> 4gt
noremap <C-5> 5gt
noremap <C-6> 6gt
noremap <C-7> 7gt
noremap <C-8> 8gt
noremap <C-9> 9gt

" taglist
noremap <C-t> :Tlist<CR>
let tlist_pyrex_settings='python;c:classe;m:memder;f:function'

" location list operations, here it is the same with
" error list
" use ]c and [c to jump in vimdiff mode
noremap [l :lprevious<cr>
noremap ]l :lnext<cr>
noremap [q :cprevious<cr>
noremap ]q :cnext<cr>

" disable arrow keys
noremap <up> <nop>
noremap <down> <nop>
noremap <left> <nop>
noremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" folding
nnoremap <space> za
vnoremap <space> zf

" CtrlP, find file and open
noremap <leader>o :CtrlP<cr>

" Ack, replacement for grep
noremap <leader>g :Ack 

" python: auto format the current file according to pep8
au Filetype Python noremap <leader>= :!autopep8 --in-place --aggressive <c-r>=@%<cr><cr><cr>

" rope binding
au Filetype Python noremap <leader>d :RopeGotoDefinition<cr>


" edit vimrc
nnoremap <leader>v :tabedit $MYVIMRC<CR>

" statusline
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
set laststatus=2

" commentor setting
let NERDSpaceDelims=1

" YCM setting
nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
let g:ycm_autoclose_preview_window_after_completion = 1

" delimiteMate python quotation
au FileType python let b:delimitMate_nesting_quotes = ['"']

" delimiateMate jump over, discouraged (fater to just type)
silent! inoremap <unique> <buffer> <C-k> <Plug>delimitMateS-Tab 

" Visual Selection

function! CmdLine(str)
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunction

function! VisualSelection(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" strip the trailing spaces
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

" markdown preview settings

function! MkdPreview()
    let fname = &backupdir . '/' . bufname('%') . '.html'
    call system('markdown ' . bufname('%') . ' > ' . fname)
    " call system('open ' . fname)
    let scpt = '/Users/Wei/Documents/Scripts/safari_open_beside.scpt'
    call system('osascript ' . scpt . ' "file://' . fname . '"')
endfunction

" markdown image upload

function! MkdImgur()
    let fpath = expand('%:p')
    call system('imgurlized_md ' . fpath)
endfunction

au FileType mkd map <leader>md :call MkdImgur()<CR>

if os=='mac'
    au BufWritePost *.{md,mdown,mkd,mkdn,markdown,mdwn} call MkdPreview()
end

au BufRead *.{md,mdown,mkd,mkdn,markdown,mdwn} setlocal spell
