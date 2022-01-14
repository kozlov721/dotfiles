let g:polyglot_disabled = ['python', 'haskell']

""" Vim-Plug
call plug#begin()

" Aesthetics
Plug 'bryanmylee/vim-colorscheme-icons'
Plug 'mhinz/vim-startify'
Plug 'nvim-lualine/lualine.nvim'
" " Plug 'junegunn/goyo.vim'
Plug 'Pocco81/TrueZen.nvim'
Plug 'junegunn/vim-journal'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'xkozlov1/cassiopeia-vim'

" Functionalities
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'mbbill/undotree'
Plug 'sheerun/vim-polyglot'
Plug '907th/vim-auto-save'
Plug 'tpope/vim-fugitive'
Plug 'chrisbra/unicode.vim'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'majutsushi/tagbar'
Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps'}
Plug 'ryanoasis/vim-devicons'
Plug 'scrooloose/nerdcommenter'
Plug 'neovim/nvim-lspconfig'
Plug 'RishabhRD/popfix'
Plug 'RishabhRD/nvim-lsputils'
Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}
Plug 'ms-jpq/coq.thirdparty', {'branch': '3p'}
Plug 'ms-jpq/coq.artifacts', {'branch': 'artifacts'}
Plug 'mhinz/vim-signify'
Plug 'windwp/nvim-autopairs'
Plug 'junegunn/vim-easy-align'
Plug 'alvan/vim-closetag'
Plug 'Yggdroot/indentLine'
" Plug 'norcalli/nvim-colorizer.lua'
Plug 'chrisbra/Colorizer'
Plug 'heavenshell/vim-pydocstring', { 'do': 'make install', 'for': 'python'}
" Plug 'metakirby5/codi.vim'
Plug 'dkarter/bullets.vim'
Plug 'tversteeg/registers.nvim', { 'branch': 'main' }
" Plug 'psliwka/vim-smoothie'
Plug 'karb94/neoscroll.nvim'
Plug 'Pocco81/AutoSave.nvim'
Plug 'antoinemadec/FixCursorHold.nvim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Python
Plug 'numirias/semshi', { 'do': ':UpdateRemotePlugins' }
Plug 'Vimjas/vim-python-pep8-indent'

" Haskell
Plug 'neovimhaskell/haskell-vim'
Plug 'alx741/vim-stylishask'

call plug#end()

let g:coq_settings = { 'auto_start': 'shut-up' }
lua require('lua_init')

""" Main Configurations
filetype plugin indent on
set foldmethod=manual
set spelllang=en_us,cs
set nospell
set tabstop=4 softtabstop=4 shiftwidth=4
set expandtab smarttab autoindent
set incsearch ignorecase smartcase hlsearch
set wildmode=longest,list,full wildmenu
set ruler laststatus=2 showcmd showmode
set list listchars=trail:»,tab:»-
set fillchars+=vert:\ 
set wrap breakindent
set encoding=utf-8
set textwidth=0
set hidden
set number
set title
set termguicolors


" Main Coloring Configurations
syntax on


""" Plugin Configurations

" NERDTree
" let NERDTreeShowHidden=1

" Exit Vim if NERDTree is the only window left.
" autocmd BufEnter * if tabpagenr('$') == 1
    " \ && winnr('$') == 1
    " \ && exists('b:CHADTree')
    " \ && b:CHADTree.isTabTree() | quit | endif

" Neovim :Terminal
tmap <Esc> <C-\><C-n>
tmap <C-w> <Esc><C-w>
tmap <C-d> <Esc>:q<CR>
autocmd BufWinEnter,WinEnter term://* startinsert
autocmd BufLeave term://* stopinsert

" vim-pydocstring
let g:pydocstring_doq_path = '~/.config/nvim/env/bin/doq'

" Supertab
let g:SuperTabDefaultCompletionType = "<C-n>"

" EasyAlign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" indentLine
let g:indentLine_char = '▏'
let g:indentLine_defaultGroup = 'NonText'
let g:vim_json_syntax_conceal = 0
let g:vim_markdown_conceal = 0
let g:vim_markdown_conceal_code_blocks = 0

" TagBar
let g:tagbar_width = 30

" fzf-vim
let g:fzf_action = {
    \ 'ctrl-t': 'tab split',
    \ 'ctrl-s': 'split',
    \ 'ctrl-v': 'vsplit' }

let g:fzf_colors = {
    \ 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Normal'],
    \ 'hl':      ['fg', 'Comment'],
    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
    \ 'hl+':     ['fg', 'Statement'],
    \ 'info':    ['fg', 'Type'],
    \ 'border':  ['fg', 'Constant'],
    \ 'prompt':  ['fg', 'Character'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Comment'] }

let $BAT_THEME='base16'

" Startify
let g:startify_fortune_use_unicode = 1

let g:cursorhold_updatetime = 100

""" Filetype-Specific Configurations

autocmd FileType html     setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType css      setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType xml      setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType markdown setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType journal  setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType python   call
    \ nerdcommenter#SwitchToAlternativeDelimiters(1)

""" Custom Functions

function! TrimWhitespace()
    let l:save = winsaveview()
    %s/\\\@<!\s\+$//e
    call winrestview(l:save)
endfunction

""" Custom Mappings

let mapleader=","

imap <C-s>           <ESC>:w<CR>i
imap ii              <ESC>
nmap <C-s>           :w<CR>
nmap -               $
xmap -               $
nmap <leader>un      :UnicodeSearch!
nmap <leader>O       O<ESC>
nmap <leader>a       gaip*
xmap <leader>a       gaip*
xmap <leader>b       <Plug>(coc-codeaction)
nmap <leader>b       <Plug>(coc-codeaction)
" nmap <leader>g    :Goyo<CR>
nmap <leader>g       :TZAtaraxis l8 r8 t1 b1<CR>
nmap <leader>o       o<ESC>
nmap <leader>p       <Plug>(pydocstring)
" nmap <leader>q    :NERDTreeToggle<CR>
nmap <leader>q       :CHADopen<CR>
nmap <leader>r       :so ~/.config/nvim/init.vim<CR><leader><leader>
nmap <leader>s       :%s/
nmap <leader>tt      :call TrimWhitespace()<CR>
nmap <leader>w       :TagbarToggle<CR>
nmap <leader>e       :UndotreeToggle<CR>
nmap <silent>        <leader><leader> :noh<CR>
nmap <Tab>           :bnext<CR>
nmap <S-Tab>         :bprevious<CR>
nmap <leader>viw     viw<C-g>
nmap <leader>vip     vip<C-g>
nmap <leader>V       V<C-g>
" nmap <space>         za
" vmap <space>         zf
" nmap <leader><space> vip<space>

nmap <leader>ts   :set nospell!<CR>
" nmap K            :bnext<CR>
" nmap J            :bprevious<CR>
nmap <leader>term <C-w>s<C-w>j:terminal
    \ <CR>:set nonumber<CR>:resize 12<CR><S-a>
nmap <leader>pterm <C-w>s<C-w>j:terminal
    \ <CR>:set nonumber<CR>:resize 12<CR><S-a>python<CR>
nmap <leader>hterm <C-w>s<C-w>j:terminal
    \ <CR>:set nonumber<CR>:resize 12<CR><S-a>ghci<CR>
nmap <leader>Term <C-w>v<C-w>l:terminal
    \ <CR>:set nonumber<CR><S-a>

" Special execution bindings

autocmd FileType python nmap <leader>x :w<CR>:execute
    \ '!python ' . expand('%:p')<CR>
autocmd FileType python nmap <leader>X :w<CR>:execute
    \ '!flake8 '    . expand('%:p') .
    \ ' && mypy '   . expand('%:p')<CR>
autocmd FileType python let @p=@%
autocmd FileType python nmap <leader>rn :Semshi rename<CR>
autocmd FileType python nmap <leader><Tab> :Semshi goto name next<CR>
autocmd FileType python nmap <leader><S-Tab> :Semshi goto name prev<CR>
autocmd FileType python nmap <leader>pdeb <C-w>s<C-w>j:terminal
    \ <CR>:set nonumber<CR>:resize 12<CR><S-a>
    \ python -i <ESC>'ppi<CR>

autocmd VimEnter *xmobar/*.hs silent
    \ nmap <leader>x :w<CR>:execute
    \ '!cd ~/.config/xmobar && ./build && xmonad --restart'<CR>
autocmd VimEnter *xmonad.hs silent
    \ nmap <leader>x :w<CR>:execute
    \ '!xmonad --recompile && xmonad --restart'<CR>
autocmd VimEnter *kitty/*.conf silent
    \ nmap <leader>x :w<CR>:execute "!fish -c 'refresh-kitty'"<CR>

autocmd VimEnter *picom.conf silent let g:auto_save = 0

" Custom settings

set whichwrap+=<,>,h,l,[,],"<left>","<right>"
set nofoldenable

autocmd VimEnter     * RainbowParentheses
autocmd BufEnter     * ColorHighlight
autocmd BufWritePre  * call TrimWhitespace()
autocmd BufWritePost *ma007*.tex silent !pdflatex <afile>
let NERDSpaceDelims=1

let g:airline_theme='dracula'
let g:cassiopeia_enable_italic = 1
colorscheme cassiopeia

let g:tex_conceal = ''
let g:python3_host_prog = $HOME.'/anaconda3/envs/nvim/bin/python'
let g:pydocstring_doq_path = $HOME.'/anaconda3/envs/nvim/bin/doq'

let g:auto_save = 1
let g:auto_save_silent = 1

let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_guard = 4
let g:haskell_indent_if = 0
let g:haskell_indent_in = 0
let g:haskell_indent_case_alternative = 1
let g:stylishask_on_save = 0
