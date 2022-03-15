---@diagnostic disable: undefined-global, unused-local

local Plug = vim.fn['plug#']

vim.call('plug#begin')

-- Aesthetics
Plug 'mhinz/vim-startify'
Plug 'nvim-lualine/lualine.nvim'
Plug 'junegunn/vim-journal'
Plug 'p00f/nvim-ts-rainbow'
Plug 'kozlov721/cassiopeia-vim'
Plug 'lukas-reineke/indent-blankline.nvim'

-- Functionalities
Plug 'jesseleite/vim-noh'
Plug 'ellisonleao/glow.nvim'
Plug 'akinsho/toggleterm.nvim'
Plug 'rmagatti/goto-preview'
Plug 'winston0410/range-highlight.nvim'
Plug 'winston0410/cmd-parser.nvim'
Plug('nvim-treesitter/nvim-treesitter', {['do'] = ':TSUpdate'})
Plug 'nvim-lua/plenary.nvim'
Plug 'mbbill/undotree'
Plug 'mfussenegger/nvim-lint'
Plug 'kosayoda/nvim-lightbulb'
Plug 'tpope/vim-fugitive'
Plug 'chrisbra/unicode.vim'
Plug 'tpope/vim-sensible'
Plug 'blackcauldron7/surround.nvim'
Plug('ms-jpq/chadtree', {branch = 'chad', ['do'] = 'python3 -m chadtree deps'})
Plug 'scrooloose/nerdcommenter'
Plug 'neovim/nvim-lspconfig'
Plug('ms-jpq/coq_nvim', {branch = 'coq'})
Plug('ms-jpq/coq.thirdparty', {branch = '3p'})
Plug('ms-jpq/coq.artifacts', {branch = 'artifacts'})
Plug 'mhinz/vim-signify'
Plug 'windwp/nvim-autopairs'
Plug 'junegunn/vim-easy-align'
Plug 'alvan/vim-closetag'
Plug 'weilbith/nvim-code-action-menu'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'dkarter/bullets.vim'
Plug('tversteeg/registers.nvim', {branch = 'main'})
Plug 'Pocco81/AutoSave.nvim'
Plug 'antoinemadec/FixCursorHold.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'bryanmylee/vim-colorscheme-icons'
Plug 'ibhagwan/fzf-lua'
Plug 'karb94/neoscroll.nvim'
Plug 'max397574/better-escape.nvim'
Plug 'echasnovski/mini.nvim'

-- Python
Plug('numirias/semshi', {['do'] = ':UpdateRemotePlugins'})
Plug 'Vimjas/vim-python-pep8-indent'

-- Haskell
Plug 'alx741/vim-stylishask'
Plug 'neovimhaskell/haskell-vim'

vim.call('plug#end')

vim.g.coq_settings = {
  auto_start = 'shut-up'
}

vim.o.termguicolors = true

require('mini.surround').setup{}
require('colorizer').setup{}
require('neoscroll').setup{}
require("indent_blankline").setup{}
require('range-highlight').setup{}
require('fzf-lua').setup{
    preview = {
    border         = 'border',
    wrap           = 'nowrap',
    hidden         = 'nohidden',
    vertical       = 'down:45%',
    horizontal     = 'right:60%',
    layout         = 'vertical',
    flip_columns   = 120,
    title          = true,
    scrollbar      = 'float',
    scrolloff      = '-2',
    scrollchars    = {'█', '' },
    delay          = 100,
    winopts = {
      number            = true,
      relativenumber    = false,
      cursorline        = true,
      cursorlineopt     = 'both',
      cursorcolumn      = false,
      signcolumn        = 'no',
      list              = false,
      foldenable        = false,
      foldmethod        = 'manual',
    },
  },
}

require('better_escape').setup{
  mapping = {'jk', 'ii'}
}

require('goto-preview').setup{
  default_mappings = true
}

require('surround').setup{
  map_insert_mode       = false,
  space_on_closing_char = true
}

require('autosave').setup{
  execution_message = '',
  conditions = {
    filename_is_not = {'picom.conf'}
  }
}

require('toggleterm').setup{
  direction     = 'float',
  open_mapping  = [[<c-\>]],
  close_on_exit = false
}

require('lint').linters_by_ft = {
  python  = {'flake8'},
  haskell = {'hlint'},
  c       = {'cppcheck'},
  sh      = {'shellcheck'},
}

local map     = vim.api.nvim_set_keymap
local bmap    = vim.api.nvim_buf_set_keymap
local autocmd = vim.api.nvim_create_autocmd

local on_attach = function(client, bn)
  bmap(bn, 'n', '<C-k>'     , '<cmd>lua vim.lsp.buf.signature_help()<CR>', {})
  bmap(bn, 'n', '<leader>b' , '<cmd>CodeActionMenu<CR>'                  , {})
  bmap(bn, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>'        , {})
  bmap(bn, 'n', '<leader>f' , '<cmd>lua vim.lsp.buf.formatting()<CR>'    , {})
  bmap(bn, 'n', 'H'         , '<cmd>lua vim.lsp.buf.hover()<CR>'         , {})
  bmap(bn, 'n', '[d'        , '<cmd>lua vim.diagnostic.goto_prev()<CR>'  , {})
  bmap(bn, 'n', ']d'        , '<cmd>lua vim.diagnostic.goto_next()<CR>'  , {})
  bmap(bn, 'n', 'gD'        , '<cmd>lua vim.lsp.buf.declaration()<CR>'   , {})
  bmap(bn, 'n', 'gd'        , '<cmd>lua vim.lsp.buf.definition()<CR>'    , {})
  bmap(bn, 'n', 'gi'        , '<cmd>lua vim.lsp.buf.implementation()<CR>', {})
  bmap(bn, 'n', 'gr'        , '<cmd>lua vim.lsp.buf.references()<CR>'    , {})

  vim.diagnostic.config({virtual_text=false})

  autocmd("CursorHold",  {callback = vim.lsp.buf.document_highlight})
  autocmd("CursorMoved", {callback = vim.lsp.buf.clear_references})
  autocmd("CursorHold",  {callback = require'nvim-lightbulb'.update_lightbulb})
end

local coq = require'coq'
local lsp = require'lspconfig'

local servers = {
  'pyright',
  'hls',
  'vimls',
  'clangd',
  'bashls',
  'sumneko_lua'
}

for _, server in ipairs(servers) do
  lsp[server].setup(coq.lsp_ensure_capabilities{
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  })
end

local npairs = require('nvim-autopairs')

npairs.setup({
  map_c             = false,
  check_ts          = true,
  ignored_next_char = '[%w%.]',
})

------ To make nvim-autopairs work with coq ------
_G.MUtils= {}
vim.g.coq_settings = {keymap = {recommended = false}}

map('i', '<Esc>',   [[pumvisible() ? "\<C-e><Esc>" : "\<Esc>"]],
  {silent = true, expr = true})
map('i', '<Tab>',   [[pumvisible() ? "\<C-n>" : "\<Tab>"]],
  {silent = true, expr = true})
map('i', '<S-Tab>', [[pumvisible() ? "\<C-p>" : "\<BS>"]],
  {silent = true, expr = true})

MUtils.CR = function()
  if vim.fn.pumvisible() ~= 0 then
    if vim.fn.complete_info({ 'selected' }).selected ~= -1 then
      return npairs.esc('<c-y>')
    else
      return npairs.esc('<c-e>') .. npairs.autopairs_cr()
    end
  else
    return npairs.autopairs_cr()
  end
end

map('i', '<cr>', 'v:lua.MUtils.CR()', {expr = true, noremap = true})

--------------------------------------------------

require'nvim-treesitter.configs'.setup{
  ensure_installed = 'maintained',
  sync_install     = false,
  highlight        = {
    enable  = true,
    disable = {}
  },
  rainbow = {
    enable         = true,
    extended_mode  = true,
    max_file_lines = nil,
    colors = {
        '#DC241D',
        '#DC55F9',
        '#D7DB11',
        '#68BD6A',
        '#E64D0E',
        '#4585C7',
        '#A8A9A4',
    }
  }
}

require('lualine').setup{
  options  = {theme = require('cassiopeia')},
  sections = {lualine_x = {'encoding', 'filetype'}},
  tabline  = {
    lualine_a = {'buffers'},
    lualine_b = {},
    lualine_c = {'branch'},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  }
}

vim.o.foldmethod  = 'expr'
vim.o.spelllang   = 'en_us,cs'
vim.o.wildmode    = 'longest,list,full'
vim.o.signcolumn  = 'number'
vim.o.encoding    = 'utf-8'
vim.o.undodir     = [[/home/martin/.config/nvim/undodir]]
vim.o.whichwrap   = vim.o.whichwrap .. '<,>,h,l,[,]'
vim.o.tabstop     = 4
vim.o.softtabstop = 4
vim.o.shiftwidth  = 4
vim.o.laststatus  = 2
vim.o.textwidth   = 0
vim.o.spell       = false
vim.o.foldenable  = false
vim.o.undofile    = true
vim.o.expandtab   = true
vim.o.smarttab    = true
vim.o.autoindent  = true
vim.o.incsearch   = true
vim.o.ignorecase  = true
vim.o.smartcase   = true
vim.o.hlsearch    = true
vim.o.wildmenu    = true
vim.o.ruler       = true
vim.o.showcmd     = true
vim.o.showmode    = true
vim.o.list        = true
vim.o.wrap        = true
vim.o.breakindent = true
vim.o.hidden      = true
vim.o.number      = true
vim.o.title       = true

vim.opt.listchars = {trail = '»', tab = '»-'}
vim.opt.fillchars:append('vert: ')

vim.cmd('filetype plugin indent on')

-- vim.g.pydocstring_doq_path = vim.env.HOME .. '/anaconda3/envs/nvim/bin/doq'
vim.g.python3_host_prog = vim.env.HOME
  .. '/anaconda3/envs/nvim/bin/python'

vim.g.cursorhold_updatetime            = 100
vim.g.haskell_indent_guard             = 4
vim.g.haskell_indent_after_bare_where  = 2
vim.g.haskell_indent_before_where      = 2
vim.g.haskell_indent_case_alternative  = 1
vim.g.haskell_indent_if                = 0
vim.g.haskell_indent_in                = 0
vim.g.NERDSpaceDelims                  = 1
vim.g.cassiopeia_enable_italic         = 1
vim.g.code_action_menu_show_details    = false
vim.g.startify_fortune_use_unicode     = 1
vim.g.stylishask_on_save               = 0
vim.g.undotree_SetFocusWhenToggle      = 1
vim.g.undotree_WindowLayout            = 2
vim.g.vim_json_syntax_conceal          = 0
vim.g.vim_markdown_conceal             = 0
vim.g.vim_markdown_conceal_code_blocks = 0
vim.g.tex_conceal                      = ''
vim.g.mapleader                        = ','

TrimWhiteSpace = function()
  local save = vim.fn.winsaveview()
  vim.cmd[[%s/\\\@<!\s\+$//e]]
  vim.fn.winrestview(save)
end

---------------------------------------------------------------
---------------------------Normal-Mode-------------------------
---------------------------------------------------------------
map('n', '-'          , '$'                             , {}  )
map('n', '<S-Tab>'    , ':bprevious<CR>'                , {}  )
map('n', '<Tab>'      , ':bnext<CR>'                    , {}  )
map('n', '<leader>O'  , 'O<ESC>'                        , {}  )
map('n', '<leader>V'  , 'V<C-g>'                        , {}  )
map('n', '<leader>a'  , 'gaip*'                         , {}  )
map('n', '<leader>e'  , ':UndotreeToggle<CR>'           , {}  )
map('n', '<leader>o'  , 'o<ESC>'                        , {}  )
map('n', '<leader>q'  , ':CHADopen<CR>'                 , {}  )
map('n', '<leader>s'  , ':%s/'                          , {}  )
map('n', '<leader>ss' , ':set nospell!<CR>'             , {}  )
map('n', '<leader>t'  , ':lua TrimWhiteSpace()<CR>'     , {}  )
map('n', '<leader>un' , ':UnicodeSearch!'               , {}  )
map('n', '<leader>vip', 'vip<C-g>'                      , {}  )
map('n', '<leader>viw', 'viw<C-g>'                      , {}  )
map('n', '<leader>w'  , ':TagbarToggle<CR>'             , {}  )
map('n', '<space>'    , 'za'                            , {}  )
map('n', 'J'          , ':bprevious<CR>'                , {}  )
map('n', 'K'          , ':bnext<CR>'                    , {}  )
map('n', '<leader>fl' , ':FzfLua lines<CR>'             , {}  )
map('n', '<leader>fbl', ':FzfLua blines<CR>'            , {}  )
map('n', '<leader>fb' , ':FzfLua buffers<CR>'           , {}  )
---------------------------------------------------------------
-------------------------Visual-Mode---------------------------
---------------------------------------------------------------
map('x', '<space>'    , 'zf'                            , {}  )
map('x', '-'          , '$'                             , {}  )
map('x', '<leader>a'  , 'gaip*'            , {noremap = false})
map('x', 'ga'         , '<Plug>(EasyAlign)', {noremap = false})


autocmd('FileType', {
  pattern = {'html', 'lua', 'css', 'xml', 'markdown', 'journal'},
  command = 'setlocal shiftwidth=2 tabstop=2 softtabstop=2',
})

autocmd('FileType', {
  pattern = 'python',
  command = 'nmap <leader>x :w<CR>:execute' ..
    '"!python " . expand("%:p")<CR>'
})

autocmd('FileType', {
  pattern = 'python',
  command = 'nmap <leader>rn :Semshi rename<CR>'
})

autocmd('FileType', {
  pattern = 'markdown',
  command = 'nmap <leader>gg :Glow<CR>'
})

-- This fixes a bug
autocmd('FileType', {
  command = 'exe "normal zR"'
})

autocmd('FileType', {
  pattern = 'python',
  command = 'nmap <leader><Tab> :Semshi goto name next<CR>'
})

autocmd('FileType', {
  pattern = 'python',
  command = 'nmap <leader><S-Tab> :Semshi goto name prev<CR>'
})

autocmd('FileType', {
  pattern = 'python',
  command = 'call nerdcommenter#SwitchToAlternativeDelimiters(1)'
})

autocmd('FileType', {
  pattern = 'python',
  command = 'nmap <leader><S-Tab> :Semshi goto name prev<CR>'
})

autocmd('BufWritePost', {callback = require('lint').try_lint})

autocmd('CursorHold', {callback = vim.diagnostic.open_float})

autocmd('VimEnter', {command = 'ColorizerToggle'})
autocmd('VimEnter', {callback = require('nvim-autopairs').enable})

autocmd('VimEnter', {
  pattern = '*xmobar/*.hs',
  command = 'nmap <leader>x :w<CR>:execute' ..
    '"!cd ~/.config/xmobar && ./build && xmonad --restart"<CR>'
})

autocmd('VimEnter', {
  pattern = '*xmonad.hs',
  command = 'nmap <leader>x :w<CR>:execute' ..
    '"!xmonad --recompile && xmonad --restart"<CR>'
})

autocmd('VimEnter', {
  pattern = '*kitty/*.conf',
  command = 'nmap <leader>x :w<CR>:execute' ..
    '"!fish -c refresh-kitty"<CR>'
})

autocmd('InsertLeavePre', {callback = TrimWhiteSpace})

vim.cmd[[
set foldexpr=nvim_treesitter#foldexpr()
colorscheme cassiopeia
]]
