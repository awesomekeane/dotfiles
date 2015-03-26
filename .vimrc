set nocompatible

" set up vundle
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Plugin 'gmarik/vundle'                    " plugin manager: PluginInstall, PluginClean
Plugin 'tpope/vim-fugitive'              " git for vim: Git
Plugin 'tpope/vim-surround'              " change surrounding chars easily: cs, ys
Plugin 'tpope/vim-repeat'                " repeat unrepeatables
Plugin 'scrooloose/nerdcommenter'        " easily comment/uncomment code: <leader>c<SPC>
Plugin 'scrooloose/syntastic'            " syntax checking
Plugin 'SirVer/ultisnips'                " snippets: <C-j>
Plugin 'honza/vim-snippets'              " snippet libs
Plugin 'mileszs/ack.vim'                 " grep: <leader>g
Plugin 'Shougo/unite.vim'                " search: <leader>b, o, m
Plugin 'Shougo/neomru.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'bling/vim-airline'               " better statusbar
Plugin 'tmhedberg/matchit'
Plugin 'scrooloose/nerdtree'             " file browser
Plugin 'sjl/gundo.vim'                   " tree-like undo
Plugin 'Raimondi/delimitMate'
Plugin 'taglist.vim'                     " show tags
Plugin 'plasticboy/vim-markdown'
Plugin 'marijnh/tern_for_vim'
Plugin 'Lokaltog/vim-easymotion'         " easier movement: <leader><leader>
Plugin 'Valloric/YouCompleteMe'          " completion: <TAB>
Plugin 'Valloric/python-indent'
Plugin 'Valloric/ListToggle'             " toggle quickfix / location list
Plugin 'sjl/vitality.vim'
Plugin 'Rename'                          " rename current file: :Rename
Plugin 'godlygeek/tabular'               " :Tabularize
Plugin 'Valloric/vim-indent-guides'      " show indent levels
Plugin 'michaeljsmith/vim-indent-object' " for python indenting
Plugin 'python_match.vim'
Plugin 'xolox/vim-pyref'                 " python doc search: <F1>
Plugin 'xolox/vim-misc'


augroup vimrc
    autocmd!
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                            Syntax and appearance                             "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

filetype plugin indent on                  " turn on filetype
syntax on
syntax enable

set background=light
colorscheme solarized                         " solarized colortheme
set guifont=Inconsolata:h18
set ruler                                     " show ruler
set number numberwidth=2                      " show line numbers
set columns=90                                " slightly wider than 80
set showmatch                                 " show matching paren
set matchtime=2                               " reduce matching paren blink time
set guioptions-=r                             " remove unnecessary gui components
set guioptions-=R
set guioptions-=T
set guioptions-=l
set guioptions-=L
set guioptions-=m
set guioptions-=M
set cursorline                                " highlights the current line
set showcmd
set scrolloff=2 sidescrolloff=5
set laststatus=2
set wrap linebreak nolist                     " linewrap
set textwidth=80                              " official width = 80
set wrapmargin=0
set foldlevelstart=99                         " as default, open all folds
let g:is_posix=1                              " highlight shell scripts as bash
set cmdheight=2                               " 2 lines for command line
set colorcolumn=+1                            " highlight the 81th line
set autoindent                                " auto indent
set copyindent


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   Editing                                    "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set backspace=indent,eol,start         " allow backspacing over everything
set whichwrap+=<,>,h,l                 " allow bs and arrow to cross line boundaries
set noeb vb t_vb=                      " turn off all bells
set hidden                             " always make buffers hidden
set history=1000                       " remember more history / undolevels
set undolevels=1000
set undofile                           " save undo info after closing file
set ignorecase smartcase lazyredraw    " searching
set ts=4 sts=4 sw=4 expandtab smarttab " tab setting, tabstop, softtabstop, shiftwidth
au vimrc Filetype make setlocal ts=8 sts=8 sw=8 noexpandtab " tabwidth settings
au vimrc Filetype yaml setlocal ts=2 sts=2 sw=2 expandtab
set backupdir=~/Documents/tmp/vim-tmp " tmp directory setting
set directory=~/Documents/tmp/vim-tmp
set autoread                          " autoload on change
set hlsearch incsearch                " highlight search

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 Key Bindings                                 "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" comma as leader
let mapleader = ","

" replace esc, in visual mode just press v again
inoremap jk <esc>
inoremap JK <esc>
inoremap Jk <esc>
inoremap jK <esc>
vnoremap v <esc>

" quick save
noremap <leader>w :w!<cr>

" using ' to jump to the marked line and position and ` to jump to the marked
" line
nnoremap ' `
nnoremap ` '

" force saving if you do not have permission
cnoremap w!! w !sudo tee % >/dev/null

" remove highlight
noremap <leader><cr> :noh<cr>

" visual search and replace, search / replace with the current visual selection
vnoremap <silent> <leader>r :call VisualSelection('replace')<cr>
vnoremap <silent> * :call VisualSelection('f')<cr>
vnoremap <silent> # :call VisualSelection('b')<cr>

" window movements
noremap <leader>j <C-W>j
noremap <leader>k <C-W>k
noremap <leader>h <C-W>h
noremap <leader>l <C-W>l

" 0 to close window
" 1 to keep current window only
noremap <leader>0 :hide<cr>
noremap <leader>1 :only<cr>

" fast movement by 15 lines
noremap <C-j> 15gj
noremap <C-k> 15gk
noremap <m-j> 15gj
noremap <m-k> 15gk

" quick editing
cnoremap %% <C-R>=fnameescape(expand('%:h')).'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%

" location list operations, here it is the same with
" error list
" use ]c and [c to jump in vimdiff mode
" Note that this corresponds with the <leader>l and <leader>q toggle of the
" lists
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

" jump through screen lines instead of actuall lines
nnoremap j gj
nnoremap k gk

" toggle spell checking, go through spell check errors, add to dict, get
" suggestions
noremap <leader>ss :setlocal spell! spelllang=en_us<cr>
noremap <leader>sn ]s
noremap <leader>sp [s
noremap <leader>sa zg
noremap <leader>su z=

" remain in visual mode after shifting
vnoremap < <gv
vnoremap > >gv

" edit vimrc, and reload vimrc using v and V
nnoremap <leader>v :tabedit $MYVIMRC<CR>
noremap <silent> <leader>V :source $MYVIMRC<CR>:filetype detect<CR>:exe ":echo 'vimrc reloaded'"<CR>

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

" au FileType mkd map <leader>md :call MkdImgur()<CR>

" turn on spell checking for certain files
au vimrc FileType markdown gitcommit setlocal spell! spelllang=en_us

au vimrc BufWritePost *.{md,mdown,mkd,mkdn,markdown,mdwn} call MkdPreview()

au vimrc FileType Python autocmd BufWritePre <buffer> :%s/\s\+$//e


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                            Plugins and Functions                             "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""
"  NERDTree  "
""""""""""""""
noremap <leader>f :NERDTree<cr>             " open NERDTree to view directory

""""""""""""""
"  Ultisnip  "
""""""""""""""
let g:UltiSnipsExpandTrigger="<C-j>"        " snippet trigger setting
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"

"""""""""""
"  Gundo  "
"""""""""""

noremap <leader>u :GundoToggle<cr>

"""""""""""""
"  Taglist  "
"""""""""""""

noremap <leader>t :Tlist<CR>
let tlist_pyrex_settings='python;c:classe;m:memder;f:function'

"""""""""
"  Ack  "
"""""""""
noremap <leader>g :Ack 


""""""""""""""
"  Autopep8  "
""""""""""""""
au vimrc Filetype Python noremap <leader>= :!autopep8 --in-place --aggressive <c-r>=@%<cr><cr><cr>

"""""""""""""""""""
"  NERDCommenter  "
"""""""""""""""""""
let NERDSpaceDelims=1


"""""""""""
"  Unite  "
"""""""""""
" use b, o, m to open buffer, file, and recent files
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#custom#source('file_rec/async', 'sorters', 'sorter_rank')
nnoremap <leader>b :Unite -buffer-name=buffers -winheight=10 buffer<cr>
nnoremap <leader>m :Unite -buffer-name=recent -winheight=10 file_mru<cr>
nnoremap <leader>o :Unite -start-insert -buffer-name=files -winheight=10 file_rec/async<cr>


"""""""""""""""""""
"  YouCompleteMe  "
"""""""""""""""""""
nnoremap <leader>d :YcmCompleter GoTo<CR> " jump to definition
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_min_num_identifier_candidate_chars = 4        " only start completion after typing 4 chars

""""""""""""""""""
"  DelimitMate   "
""""""""""""""""""
au vimrc FileType html,xhtml,markdown let b:delitMate_matchpairs = "(:),[:],{:}"
au vimrc FileType Python let b:delimitMate_nesting_quotes = ['"'] " for python three quotes
" delimiateMate jump over, discouraged (fater to just type)
silent! inoremap <unique> <buffer> <C-k> <Plug>delimitMateS-Tab 


"""""""""""""""""""""""
"  Markdown Function  "
"""""""""""""""""""""""
function! MkdPreview()
    let fname = &backupdir . '/' . bufname('%') . '.html'
    call system('markdown ' . bufname('%') . ' > ' . fname)
    " call system('open ' . fname)
    " let scpt = '/Users/Wei/Documents/Scripts/safari_open_beside.scpt'
    " call system('osascript ' . scpt . ' "file://' . fname . '"')
    call system('open ' . fname)
endfunction

" markdown image upload
function! MkdImgur()
    let fpath = expand('%:p')
    call system('imgurlized_md ' . fpath)
endfunction


"""""""""""""""
"  Syntastic  "
"""""""""""""""
let g:syntastic_error_symbol = '✗'
let g:syntastic_warning_symbol = '⚠'
let g:syntastic_always_populate_loc_list = 1


"""""""""""""
"  Airline  "
"""""""""""""
let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled=1    " statusline setting
let g:airline_detect_whitespace=0


"""""""""""""""""""""""
"  vim-indent-guides  "
"""""""""""""""""""""""
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_color_change_percent=3
let g:indent_guides_start_level=2
let g:indent_guides_guide_size=1


""""""""""""""""
"  ListToggle  "
""""""""""""""""
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

