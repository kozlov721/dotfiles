vim.o.termguicolors = true
vim.g.mapleader = ';'

require('plugins')

local map = vim.keymap.set
local autocmd = vim.api.nvim_create_autocmd

vim.o.signcolumn     = 'number'
vim.o.spelllang      = 'en_us,cs'
vim.o.undodir        = [[/home/martin/.config/nvim/undodir]]
vim.o.whichwrap      = vim.o.whichwrap .. '<,>,h,l,[,]'
vim.o.wildmode       = 'longest,list,full'
vim.o.foldmethod     = 'expr'
vim.o.foldexpr       = 'nvim_treesitter#foldexpr()'
-- vim.o.colorcolumn    = '81'
vim.o.shiftwidth     = 4
vim.o.softtabstop    = 4
vim.o.tabstop        = 4
vim.o.laststatus     = 3
vim.o.foldenable     = false
vim.o.breakindent    = true
vim.o.expandtab      = true
vim.o.ignorecase     = true
vim.o.list           = true
vim.o.number         = true
vim.o.relativenumber = true
vim.o.showcmd        = false
vim.o.showmode       = true
vim.o.smartcase      = true
vim.o.title          = true
vim.o.undofile       = true
vim.o.wrap           = true

vim.opt.listchars = {trail = '»', tab = '»-'}
vim.opt.fillchars:append('vert:│')

vim.g.cursorhold_updatetime            = 300
vim.g.tex_conceal                      = ''
vim.g.vim_json_syntax_conceal          = 0
vim.g.vim_markdown_conceal             = 0
vim.g.vim_markdown_conceal_code_blocks = 0

vim.g.python3_host_prog = '/usr/bin/python'

local function trimWhiteSpace()
  local save = vim.fn.winsaveview()
  vim.cmd[[%s/\\\@<!\s\+$//e]]
  vim.fn.winrestview(save)
end

map({'n', 'x'} , '-'       , '$'                         )
map('n', '<leader>J'       , ':join<CR>'                 )
map('n', '<leader>O'       , 'O<ESC>'                    )
map('n', '<leader>V'       , 'V<C-g>'                    )
map('n', '<leader>o'       , 'o<ESC>'                    )
map('n', '<leader>s'       , ':%s/'                      )
map('n', '<leader>ns'      , ':set nospell!<CR>'         )
map('n', '<leader>vip'     , 'vip<C-g>'                  )
map('n', '<leader>viw'     , 'viw<C-g>'                  )
map('n', '<space>'         , 'za'                        )
map('n', '<leader><leader>', ':noh<CR>'                  )
map('n', '<leader>r'       , ':source $MYVIMRC|noh<CR>'  )
map('n', '<leader><TAB>'   , '<C-w><C-w>', {remap = true})
map('n', '<leader>t'       , trimWhiteSpace              )

map('n', 'J', ':bprevious<CR>', {silent = true})
map('n', 'K', ':bnext<CR>'    , {silent = true})

autocmd('FileType', {
  pattern = {'html', 'lua', 'css', 'xml', 'markdown', 'journal', 'fish'},
  command = 'setlocal shiftwidth=2 tabstop=2 softtabstop=2',
})

autocmd('FileType', {command = 'noh'})

autocmd('FileType', {
  pattern = 'help',
  command = 'noremap <buffer> <F1> :q<CR>'
})

autocmd('VimEnter', {
  pattern = '*xmonad.hs',
  command = 'nmap <leader>x :w<CR>:execute' ..
    '"!xmonad --recompile && xmonad --restart"<CR>'
})

autocmd('VimEnter', {
  pattern = 'init.lua',
  command = 'cmap q qa'
})

autocmd('InsertLeavePre', {
  callback = function()
    if vim.o.modifiable then
      trimWhiteSpace()
    end
  end
})

autocmd('FileType', {
  pattern = {"fish", "asm"},
  callback = function()
    vim.api.nvim_buf_set_option(0, "commentstring", "# %s")
  end
})

autocmd('BufWinEnter', {command = 'exe "normal zR"'})

vim.cmd 'colorscheme cassiopeia'
