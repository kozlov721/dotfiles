let g:polyglot_disabled = ['python', 'haskell']

""" Vim-Plug
call plug#begin()

" Aesthetics
Plug 'vim-airline/vim-airline'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'vim-airline/vim-airline-themes'
Plug 'bryanmylee/vim-colorscheme-icons'
Plug 'mhinz/vim-startify'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/vim-journal'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'nightsense/forgotten'
Plug 'ghifarit53/tokyonight-vim'

" Functionalities
Plug 'mbbill/undotree'
Plug 'sheerun/vim-polyglot'
Plug '907th/vim-auto-save'
Plug 'tpope/vim-fugitive'
Plug 'chrisbra/unicode.vim'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'majutsushi/tagbar'
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'ryanoasis/vim-devicons'
Plug 'scrooloose/nerdcommenter'
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'mhinz/vim-signify'
Plug 'windwp/nvim-autopairs'
Plug 'junegunn/vim-easy-align'
Plug 'alvan/vim-closetag'
Plug 'tpope/vim-abolish'
Plug 'Yggdroot/indentLine'
Plug 'chrisbra/Colorizer'
Plug 'KabbAmine/vCoolor.vim'
Plug 'heavenshell/vim-pydocstring', { 'do': 'make install' }
Plug 'metakirby5/codi.vim'
Plug 'dkarter/bullets.vim'
Plug 'psliwka/vim-smoothie'
Plug 'antoinemadec/FixCursorHold.nvim'
Plug 'drmingdrmer/xptemplate'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Python
Plug 'numirias/semshi', { 'do': ':UpdateRemotePlugins' }
Plug 'Vimjas/vim-python-pep8-indent'

" Haskell
Plug 'neovimhaskell/haskell-vim'
Plug 'alx741/vim-stylishask'

call plug#end()

""" Main Configurations
filetype plugin indent on
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

lua require('nvim-autopairs').setup{}

""" Coloring

" Main Coloring Configurations
syntax on

" Enable True Color Support (ensure you're using a 256-color enabled $TERM, e.g. xterm-256color)
set termguicolors

""" Plugin Configurations

" NERDTree
let NERDTreeShowHidden=1

" Exit Vim if NERDTree is the only window left.
autocmd BufEnter * if tabpagenr('$') == 1
            \ && winnr('$') == 1
            \ && exists('b:NERDTree')
            \ && b:NERDTree.isTabTree() | quit | endif

" Airline
let g:airline_powerline_fonts = 1
let g:airline_section_x = ' %{&filetype}'
let g:airline_section_z = '%p%%  %{strftime("%-I:%M %p")}'
let g:airline_section_warning = ''
let g:airline#extensions#tabline#enabled = 1

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
" Disable indentLine from concealing json and markdown syntax (e.g. ```)
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

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
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

" coc.vim START

set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> H :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" coc.vim END

" signify
let g:signify_sign_add    = '+'
let g:signify_sign_delete = '-'
let g:signify_sign_change = '│'

" FixCursorHold for better performance
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

function! ColorDark()
    let g:airline_theme='dracula'
    let g:tokyonight_style = 'storm'
    let g:tokyonight_enable_italic = 1
    colorscheme tokyonight
endfunction

function! ColorLight()
    let g:airline_theme='tomorrow'
    let g:allow_italic=1
    color forgotten-light
endfunction

function! SaveKittyTheme()
    if !empty(glob("/var/local/change_theme/light_on.lck"))
        let theme_path = $HOME . "/.config/kitty/light.theme"
    else
        let theme_path = $HOME . "/.config/kitty/dark.theme"
    endif
    silent execute "!cat <afile> " . " > " . theme_path
    silent !fish -c "refresh-kitty"
endfunction

""" Custom Mappings

let mapleader=","

imap <C-s>        <ESC>:w<CR>i
imap ii           <ESC>
nmap <C-s>        :w<CR>
nmap -            $
xmap -            $
nmap <leader>un   :UnicodeSearch!
nmap <leader>O    O<ESC>
nmap <leader>a    gaip*
xmap <leader>a    gaip*
xmap <leader>b    <Plug>(coc-codeaction)
nmap <leader>b    <Plug>(coc-codeaction)
nmap <leader>g    :Goyo<CR>
nmap <leader>o    o<ESC>
nmap <leader>p    <Plug>(pydocstring)
nmap <leader>q    :NERDTreeToggle<CR>
nmap <leader>r    :so ~/.config/nvim/init.vim<CR><leader><leader>
nmap <leader>s    :%s/
nmap <leader>t    :call TrimWhitespace()<CR>
nmap <leader>tt   :call TrimWhitespace()<CR>
nmap <leader>w    :TagbarToggle<CR>
nmap <leader>e    :UndotreeToggle<CR>
nmap <silent>     <leader><leader> :noh<CR>
nmap <Tab>        :bnext<CR>
nmap <S-Tab>      :bprevious<CR>
nmap <leader>viw  viw<C-g>
nmap <leader>vip  vip<C-g>
nmap <leader>V    V<C-g>
nmap K            :bnext<CR>
nmap J            :bprevious<CR>
nmap <leader>term <C-w>s<C-w>j:terminal
            \ <CR>:set nonumber<CR>:resize 12<CR><S-a>
nmap <leader>Term <C-w>v<C-w>l:terminal
            \ <CR>:set nonumber<CR><S-a>

" Special execution bindings

autocmd FileType python nmap <leader>x :w<CR>:execute
    \ "!python " . expand("%:p")<CR>
autocmd FileType python nmap <leader>X :w<CR>:execute
    \ "!flake8 "    . expand("%:p") .
    \ " && mypy "   . expand("%:p")<CR>
autocmd FileType python nmap
    \ <silent> <leader>rn :Semshi rename<CR>
autocmd FileType python nmap
    \ <silent> <leader><Tab> :Semshi goto name next<CR>
autocmd FileType python nmap
    \ <silent> <leader><S-Tab> :Semshi goto name prev<CR>

autocmd VimEnter *xmobar/*.hs silent
    \ nmap <leader>x :w<CR>:execute
    \ "!cd ~/.config/xmobar && ./build && xmonad --restart"<CR>
autocmd VimEnter *xmonad.hs silent
    \ nmap <leader>x :w<CR>:execute
    \ "!xmonad --recompile && xmonad --restart"<CR>
autocmd vimEnter *kitty/current.theme silent
    \ nmap <leader>x :w<CR>:call SaveKittyTheme()<CR>
autocmd VimEnter *kitty.conf silent
    \ nmap <leader>x :w<CR>:execute "!fish -c 'refresh-kitty'"<CR>

" Custom settings

set whichwrap+=<,>,h,l,[,],"<left>","<right>"
set nofoldenable

autocmd VimEnter     * RainbowParentheses
autocmd VimEnter     * ColorToggle
autocmd BufWritePre  * call TrimWhitespace()
autocmd BufWritePost *ma007*.tex silent !pdflatex <afile>
let NERDSpaceDelims=1

if !empty(glob("/var/local/change_theme/light_on.lck"))
    call ColorLight()
else
    call ColorDark()
endif

let g:tex_conceal = ""
let g:python3_host_prog = $HOME."/anaconda3/envs/nvim/bin/python"
let g:auto_save = 1
let g:auto_save_silent = 1

let g:haskell_indent_before_where = 2
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_guard = 4
let g:haskell_indent_if = 0
let g:haskell_indent_in = 0
let g:haskell_indent_case_alternative = 1
let g:stylishask_on_save = 0
