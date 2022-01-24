---@diagnostic disable: undefined-global, unused-local

local Plug = vim.fn['plug#']

vim.call('plug#begin')

-- Aesthetics
Plug 'mhinz/vim-startify'
Plug 'nvim-lualine/lualine.nvim'
Plug 'junegunn/vim-journal'
Plug 'p00f/nvim-ts-rainbow'
Plug 'xkozlov1/cassiopeia-vim'
Plug 'lukas-reineke/indent-blankline.nvim'

-- Functionalities
Plug 'wellle/targets.vim'
Plug 'sbdchd/neoformat'
-- Plug 'kana/vim-textobj-user'
-- Plug 'sgur/vim-textobj-parameter'
Plug 'AckslD/nvim-revJ.lua'
Plug 'ellisonleao/glow.nvim'
Plug 'Pocco81/TrueZen.nvim'
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
-- Plug 'blackcauldron7/surround.nvim'
Plug 'majutsushi/tagbar'
Plug('ms-jpq/chadtree', {branch = 'chad', ['do'] = 'python3 -m chadtree deps'})
Plug 'scrooloose/nerdcommenter'
Plug 'neovim/nvim-lspconfig'
Plug('ms-jpq/coq_nvim', {branch = 'coq'})
Plug('ms-jpq/coq.thirdparty', {branch = '3p'})
Plug('ms-jpq/coq.artifacts', {branch = 'artifacts'})
-- Plug 'mhinz/vim-signify'
Plug 'windwp/nvim-autopairs'
Plug 'junegunn/vim-easy-align'
Plug 'alvan/vim-closetag'
Plug 'weilbith/nvim-code-action-menu'
Plug 'chrisbra/Colorizer'
Plug 'dkarter/bullets.vim'
Plug('tversteeg/registers.nvim', {branch = 'main'})
Plug 'karb94/neoscroll.nvim'
Plug 'Pocco81/AutoSave.nvim'
Plug 'antoinemadec/FixCursorHold.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'bryanmylee/vim-colorscheme-icons'
Plug 'ibhagwan/fzf-lua'

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

require('neoscroll').setup{}
require("indent_blankline").setup {}
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

require("revj").setup{
  add_seperator_for_last_parameter = false,
  enable_default_keymaps = true,
  keymaps = {
        operator = '<leader>J', -- for operator (+motion)
        line = '<leader>j', -- for formatting current line
        visual = '<leader>j', -- for formatting visual selection
    }
}

require('nvim-autopairs').setup{
  check_ts          = true,
  ignored_next_char = '[%w%.]'
}

require('goto-preview').setup{
  default_mappings = true
}

-- require('surround').setup{
--  map_insert_mode       = false,
--  space_on_closing_char = true
-- }

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
  haskell = {'hlint'}
}

local map = function(key)
  local opts = {noremap = true}
  for i, v in pairs(key) do
    if type(i) == 'string' then opts[i] = v end
  end

  local buffer = opts.buffer
  opts.buffer = nil

  if buffer then
    vim.api.nvim_buf_set_keymap(0, key[1], key[2], key[3], opts)
  else
    vim.api.nvim_set_keymap(key[1], key[2], key[3], opts)
  end
end

local on_attach = function(client, bufnr)

  map { 'n' , '<C-k>'      , '<cmd>lua vim.lsp.buf.signature_help()<CR>' , buffer = bufnr }
  map { 'n' , '<leader>b'  , '<cmd>CodeActionMenu<CR>'                   , buffer = bufnr }
  map { 'n' , '<leader>rn' , '<cmd>lua vim.lsp.buf.rename()<CR>'         , buffer = bufnr }
  map { 'n' , '<leader>f'  , '<cmd>lua vim.lsp.buf.formatting()<CR>'     , buffer = bufnr }
  map { 'n' , 'H'          , '<cmd>lua vim.lsp.buf.hover()<CR>'          , buffer = bufnr }
  map { 'n' , '[d'         , '<cmd>lua vim.diagnostic.goto_prev()<CR>'   , buffer = bufnr }
  map { 'n' , ']d'         , '<cmd>lua vim.diagnostic.goto_next()<CR>'   , buffer = bufnr }
  map { 'n' , 'gD'         , '<cmd>lua vim.lsp.buf.declaration()<CR>'    , buffer = bufnr }
  map { 'n' , 'gd'         , '<cmd>lua vim.lsp.buf.definition()<CR>'     , buffer = bufnr }
  map { 'n' , 'gi'         , '<cmd>lua vim.lsp.buf.implementation()<CR>' , buffer = bufnr }
  map { 'n' , 'gr'         , '<cmd>lua vim.lsp.buf.references()<CR>'     , buffer = bufnr }

  vim.diagnostic.config({virtual_text=false})

  if vim.fn.expand('%:t') ~= 'init.lua' then
    vim.cmd[[
      autocmd CursorHold   * silent lua vim.lsp.buf.document_highlight()
      autocmd CursorMoved  * silent lua vim.lsp.buf.clear_references()
      autocmd CursorHold   * lua require'nvim-lightbulb'.update_lightbulb()
    ]]
  end
end


local coq = require'coq'
local lsp = require'lspconfig'

local servers = {'pyright', 'hls', 'vimls', 'sourcekit', 'sumneko_lua' }

for _, server in ipairs(servers) do
  lsp[server].setup(coq.lsp_ensure_capabilities{
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  })
end

require'nvim-treesitter.configs'.setup{
  ensure_installed = 'maintained',
  sync_install     = false,
  highlight        = {
    enable         = true,
    disable        = {}
  },
  rainbow = {
    enable         = true,
    extended_mode  = true,
    max_file_lines = nil,
    colors = {
        '#DC241D',
        '#A8A9A4',
        '#DC55F9',
        '#D7DB11',
        '#68BD6A',
        '#E64D0E',
        '#4585C7'
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

vim.o.foldmethod    = 'manual'
vim.o.spelllang     = 'en_us,cs'
vim.o.wildmode      = 'longest,list,full'
vim.o.signcolumn    = 'number'
vim.o.encoding      = 'utf-8'
vim.o.undodir       = [[/home/martin/.config/nvim/undodir]]
vim.o.tabstop       = 4
vim.o.softtabstop   = 4
vim.o.shiftwidth    = 4
vim.o.laststatus    = 2
vim.o.textwidth     = 0
vim.o.spell         = false
vim.o.foldenable    = false
vim.o.undofile      = true
vim.o.expandtab     = true
vim.o.smarttab      = true
vim.o.autoindent    = true
vim.o.incsearch     = true
vim.o.ignorecase    = true
vim.o.smartcase     = true
vim.o.hlsearch      = true
vim.o.wildmenu      = true
vim.o.ruler         = true
vim.o.showcmd       = true
vim.o.showmode      = true
vim.o.list          = true
vim.o.wrap          = true
vim.o.breakindent   = true
vim.o.hidden        = true
vim.o.number        = true
vim.o.title         = true
vim.o.termguicolors = true

vim.opt.listchars   = {trail = '»', tab = '»-'}
vim.opt.fillchars:append('vert: ')

vim.cmd('filetype plugin indent on')

vim.g.pydocstring_doq_path             = vim.env.HOME .. '/anaconda3/envs/nvim/bin/doq'
vim.g.python3_host_prog                = vim.env.HOME .. '/anaconda3/envs/nvim/bin/python'
vim.g.neoformat_enabled_python         = {'yapf'}
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

-------------------------------------------------------------------------------
-----------------------------Insert-Mode---------------------------------------
-------------------------------------------------------------------------------
map {'i' , '<C-s>'            , '<ESC>:w<CR>i'                                }
map {'i' , 'ii'               , '<ESC>'                                       }
map {'i' , '<PageUp>'         , '<NOP>'                                       }
map {'i' , '<PageDown>'       , '<NOP>'                                       }
-------------------------------------------------------------------------------
-----------------------------Normal-Mode---------------------------------------
-------------------------------------------------------------------------------
map {'n' , '-'                , '$'                                           }
map {'n' , '<S-Tab>'          , ':bprevious<CR>'                              }
map {'n' , '<Tab>'            , ':bnext<CR>'                                  }
map {'n' , '<leader><space>'  , 'vipza'                                       }
map {'n' , '<leader><leader>' , ':noh<CR>'                                    }
map {'n' , '<leader>O'        , 'O<ESC>'                                      }
map {'n' , '<leader>V'        , 'V<C-g>'                                      }
map {'n' , '<leader>a'        , 'gaip*'                                       }
map {'n' , '<leader>b'        , '<Plug>(coc-codeaction)'                      }
map {'n' , '<leader>e'        , ':UndotreeToggle<CR>'                         }
map {'n' , '<leader>g'        , ':TZAtaraxis l10 r10 t2 b2<CR>'               }
map {'n' , '<leader>o'        , 'o<ESC>'                                      }
map {'n' , '<leader>p'        , '<Plug>(pydocstring)'                         }
map {'n' , '<leader>q'        , ':CHADopen<CR>'                               }
map {'n' , '<leader>r'        , ':source ~/.config/nvim/init.lua<CR>:noh<CR>' }
map {'n' , '<leader>s'        , ':%s/'                                        }
map {'n' , '<leader>ts'       , ':set nospell!<CR>'                           }
map {'n' , '<leader>tt'       , ':call TrimWhitespace()<CR>'                  }
map {'n' , '<leader>un'       , ':UnicodeSearch!'                             }
map {'n' , '<leader>vip'      , 'vip<C-g>'                                    }
map {'n' , '<leader>viw'      , 'viw<C-g>'                                    }
map {'n' , '<leader>w'        , ':TagbarToggle<CR>'                           }
map {'n' , '<silent>'         , '<leader><leader> :noh<CR>'                   }
map {'n' , '<space>'          , 'za'                                          }
map {'n' , 'J'                , ':bprevious<CR>'                              }
map {'n' , 'K'                , ':bnext<CR>'                                  }
map {'n' , 'ga'               , '<Plug>(EasyAlign)' , noremap = false         }
map {'n' , '<leader>fl'       , ':FzfLua lines<CR>'                           }
map {'n' , '<leader>fbl'      , ':FzfLua blines<CR>'                          }
map {'n' , '<leader>fb'       , ':FzfLua buffers<CR>'                         }
map {'n' , '<PageUp>'         , '<c-b>', noremap = false                      }
map {'n' , '<PageDown>'       , '<c-f>', noremap = false                      }
-------------------------------------------------------------------------------
-----------------------------Visual-Mode---------------------------------------
-------------------------------------------------------------------------------
map {'x' , '<PageUp>'         , '<NOP>'                                       }
map {'x' , '<PageDown>'       , '<NOP>'                                       }
map {'x' , '<space>'          , 'zf'                                          }
map {'x' , '-'                , '$'                                           }
map {'x' , '<leader>a'        , 'gaip*'             , noremap = false         }
map {'x' , 'ga'               , '<Plug>(EasyAlign)' , noremap = false         }


TrimWhiteSpace = function()
  local save = vim.fn.winsaveview()
  vim.cmd[[%s/\\\@<!\s\+$//e]]
  vim.fn.winrestview(save)
end

-- not happy about this
vim.cmd[[

autocmd FileType html     setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType lua      setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType css      setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType xml      setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType markdown setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType journal  setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType python   call
    \ nerdcommenter#SwitchToAlternativeDelimiters(1)


autocmd FileType python nmap <leader>x :w<CR>:execute
    \ '!python ' . expand('%:p')<CR>
autocmd FileType python nmap <leader>rn:      Semshi rename<CR>
autocmd FileType python nmap <leader><Tab>:   Semshi goto name next<CR>
autocmd FileType python nmap <leader><S-Tab>: Semshi goto name prev<CR>
autocmd FileType markdown nmap <leader>gg :Glow<CR>

autocmd VimEnter *xmobar/*.hs silent
    \ nmap <leader>x :w<CR>:execute
    \ '!cd ~/.config/xmobar && ./build && xmonad --restart'<CR>
autocmd VimEnter *xmonad.hs silent
    \ nmap <leader>x :w<CR>:execute
    \ '!xmonad --recompile && xmonad --restart'<CR>
autocmd VimEnter *kitty/*.conf silent
    \ nmap <leader>x :w<CR>:execute "!fish -c 'refresh-kitty'"<CR>

autocmd VimEnter *picom.conf silent let g:auto_save = 0

set whichwrap+=<,>,h,l,[,],"<left>","<right>"
set foldexpr=nvim_treesitter#foldexpr()

autocmd FileType     * ColorToggle
autocmd FileType     * ColorSwapFgBg
autocmd BufWritePre  * lua TrimWhiteSpace()
autocmd BufWritePost *ma007*.tex silent !pdflatex <afile>
autocmd BufWritePost <buffer> lua require('lint').try_lint()
autocmd CursorHold   * lua vim.diagnostic.open_float()

colorscheme cassiopeia
]]
