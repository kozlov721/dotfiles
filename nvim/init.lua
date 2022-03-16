---@diagnostic disable: undefined-global, unused-local
vim.o.termguicolors = true

local setup = function(name, opts)
  opts = opts or {}
  require(name).setup(opts)
end

require('packer').startup(
  function()
    use {'mhinz/vim-startify'}
    use {'nvim-lualine/lualine.nvim',
      config = setup('lualine', {
          options  = {theme = require('cassiopeia')},
          sections = {lualine_x = {'filetype'}},
          tabline  = {
            lualine_a = {'buffers'},
            lualine_b = {},
            lualine_c = {'branch'},
            lualine_x = {},
            lualine_y = {},
            lualine_z = {}
          }
        })
    }
    use {'phaazon/hop.nvim',
      branch = 'v1',
      config = setup('hop', {keys = 'etovxqpdygfblzhckisuran'})
    }
    use {'junegunn/vim-journal'}
    use {'p00f/nvim-ts-rainbow'}
    use {'kozlov721/cassiopeia-vim'}
    use {'lukas-reineke/indent-blankline.nvim',
      config = setup('indent_blankline')
    }
    use {'michaelb/sniprun', run = 'bash ./install.sh'}
    use {'jesseleite/vim-noh'}
    use {'ellisonleao/glow.nvim', ft = {'markdown'}}
    use {'akinsho/toggleterm.nvim',
      config = setup('toggleterm', {
          direction     = 'float',
          open_mapping  = [[<c-\>]],
          close_on_exit = false
        })
    }
    use {'rmagatti/goto-preview',
      config = setup('goto-preview', {
          default_mappings = true
        })
    }
    use {'winston0410/range-highlight.nvim',
      config = setup('range-highlight')
    }
    use {'winston0410/cmd-parser.nvim'}
    use {'nvim-treesitter/nvim-treesitter',
      run = ':TSUpdate',
      config = setup('nvim-treesitter.configs', {
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
        })
    }
    use {'chentau/marks.nvim',
      config = setup('marks')
    }
    use {'nvim-lua/plenary.nvim'}
    use {'mbbill/undotree'}

    use {'mfussenegger/nvim-lint',
      config = function()
        require('lint').linters_by_ft = {
          python  = {'flake8'},
          haskell = {'hlint'},
          c       = {'cppcheck'},
          sh      = {'shellcheck'},
        }
      end
    }
    use {'kosayoda/nvim-lightbulb'}
    use {'tpope/vim-fugitive'}
    use {'chrisbra/unicode.vim'}
    use {'tpope/vim-sensible'}
    use {'ms-jpq/chadtree',
      branch = 'chad',
      run = 'python3 -m chadtree deps'
    }
    use {'scrooloose/nerdcommenter'}
    use {'neovim/nvim-lspconfig'}
    use {'ms-jpq/coq_nvim', branch = 'coq'}
    use {'ms-jpq/coq.thirdparty', branch = '3p'}
    use {'ms-jpq/coq.artifacts', branch = 'artifacts'}
    use {'mhinz/vim-signify'}
    use {'windwp/nvim-autopairs',
     config = setup('nvim-autopairs', {
         map_c             = false,
         check_ts          = true,
         ignored_next_char = '[%w%.]',
       })
    }
    use {'junegunn/vim-easy-align'}
    use {'alvan/vim-closetag'}
    use {'weilbith/nvim-code-action-menu'}
    use {'norcalli/nvim-colorizer.lua',
      config = setup('colorizer')
    }
    use {'dkarter/bullets.vim'}
    use {'tversteeg/registers.nvim', branch = 'main'}
    use {'Pocco81/AutoSave.nvim',
      config = setup('autosave', {
          execution_message = '',
          conditions = {
            filename_is_not = {'picom.conf'}
          }
        })
    }
    use {'antoinemadec/FixCursorHold.nvim'}
    use {'kyazdani42/nvim-web-devicons'}
    use {'bryanmylee/vim-colorscheme-icons'}
    use {'ibhagwan/fzf-lua',
      requires = {'kyazdani42/nvim-web-devicons'},
      config = setup('fzf-lua', {
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
        })
    }
    use {'karb94/neoscroll.nvim',
      config = setup('neoscroll')
    }
    use {'max397574/better-escape.nvim',
      config = setup('better_escape', {
          mapping = {'jk'}
        })
    }
    use {'echasnovski/mini.nvim',
      config = setup('mini.surround')
    }
    use {'lewis6991/spellsitter.nvim',
      config = setup('spellsitter')
    }
    -- Python
    use {'numirias/semshi', run = ':UpdateRemotePlugins'}
    use {'Vimjas/vim-python-pep8-indent'}
    -- Haskell
    use {'alx741/vim-stylishask'}
    use {'neovimhaskell/haskell-vim'}
  end
)

vim.g.coq_settings = {
auto_start = 'shut-up'
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

  autocmd('CursorHold',  {callback = vim.lsp.buf.document_highlight})
  autocmd('CursorMoved', {callback = vim.lsp.buf.clear_references})
  autocmd('CursorHold',  {callback = require'nvim-lightbulb'.update_lightbulb})
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
    flags = {debounce_text_changes = 150}
  })
end

------ To make nvim-autopairs work with coq ------
local npairs = require('nvim-autopairs')
_G.MUtils= {}
vim.g.coq_settings = {keymap = {recommended = false}}

map('i', '<Esc>',   [[pumvisible() ? "\<C-e><Esc>" : "\<Esc>"]],
 {silent = true, expr = true})
map('i', '<Tab>',   [[pumvisible() ? "\<C-n>" : "\<Tab>"]],
 {silent = true, expr = true})
map('i', '<S-Tab>', [[pumvisible() ? "\<C-p>" : "\<BS>"]],
 {silent = true, expr = true})
map('i', '<c-c>', [[pumvisible() ? "<c-e><c-c>" : "<c-c>"]],
  {expr = true, noremap = true})


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

vim.o.foldmethod  = 'expr'
vim.o.spelllang   = 'en_us,cs'
vim.o.wildmode    = 'longest,list,full'
vim.o.signcolumn  = 'number'
vim.o.encoding    = 'utf-8'
vim.o.undodir     = [[/home/martin/.config/nvim/undodir]]
vim.o.whichwrap   = vim.o.whichwrap .. '<,>,h,l,[,]'
vim.o.colorcolumn = '80'
vim.o.tabstop     = 4
vim.o.softtabstop = 4
vim.o.shiftwidth  = 4
vim.o.laststatus  = 2
vim.o.textwidth   = 0
vim.o.spell       = false
vim.o.foldenable  = false
vim.o.spell       = true
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


map('n', '-'               , '$'                        , {})
map('n', 'J'               , ':bprevious<CR>'           , {})
map('n', 'K'               , ':bnext<CR>'               , {})
map('n', '<leader>O'       , 'O<ESC>'                   , {})
map('n', '<leader>V'       , 'V<C-g>'                   , {})
map('n', '<leader>a'       , 'vipga'                    , {})
map('n', '<leader>e'       , ':UndotreeToggle<CR>'      , {})
map('n', '<leader>o'       , 'o<ESC>'                   , {})
map('n', '<leader>q'       , ':CHADopen<CR>'            , {})
map('n', '<leader>s'       , ':%s/'                     , {})
map('n', '<leader>ns'      , ':set nospell!<CR>'        , {})
map('n', '<leader>t'       , ':lua TrimWhiteSpace()<CR>', {})
map('n', '<leader>un'      , ':UnicodeSearch!'          , {})
map('n', '<leader>vip'     , 'vip<C-g>'                 , {})
map('n', '<leader>viw'     , 'viw<C-g>'                 , {})
map('n', '<space>'         , 'za'                       , {})
map('n', '<leader>fl'      , ':FzfLua lines<CR>'        , {})
map('n', '<leader>fbl'     , ':FzfLua blines<CR>'       , {})
map('n', '<leader>fb'      , ':FzfLua buffers<CR>'      , {})
map('n', '<leader><leader>', ':noh<CR>'                 , {})
map('n', '<leader>r'       , ':source $MYVIMRC|noh<CR>' , {})
map('n', 'ff'              , ':HopChar1<CR>'            , {})
map('n', 'fl'              , ':HopLine<CR>'             , {})
map('n', 'F'               , ':HopPattern<CR>'          , {})

map('x', '<space>'         , 'zf'                       , {})
map('x', '-'               , '$'                        , {})

map('x', '<leader>a', 'gaip*'            , {noremap = false})
map('x', 'ga'       , '<Plug>(EasyAlign)', {noremap = false})


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
  command = 'nmap <leader>x :Glow<CR>'
})

-- This fixes a bug
autocmd('FileType', {command = 'exe "normal zR"'})

autocmd('FileType', {command = 'noh'})

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
autocmd('BufWritePost', {
  pattern = 'init.lua',
  command = 'PackerCompile'
})

vim.cmd[[
set nu rnu
set foldexpr=nvim_treesitter#foldexpr()
colorscheme cassiopeia
]]
