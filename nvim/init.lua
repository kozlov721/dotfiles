---@diagnostic disable: undefined-global, unused-local
vim.o.termguicolors = true
vim.g.mapleader = ';'
require('plugins')
local map = vim.keymap.set
local autocmd = vim.api.nvim_create_autocmd

vim.o.signcolumn     = 'number'
vim.o.spelllang      = 'en_us,cs'
vim.o.encoding       = 'utf-8'
vim.o.undodir        = [[/home/martin/.config/nvim/undodir]]
vim.o.whichwrap      = vim.o.whichwrap .. '<,>,h,l,[,]'
vim.o.wildmode       = 'longest,list,full'
vim.o.foldmethod     = 'expr'
vim.o.foldexpr       = 'nvim_treesitter#foldexpr()'
vim.o.colorcolumn    = '81'
vim.o.shiftwidth     = 4
vim.o.softtabstop    = 4
vim.o.tabstop        = 4
vim.o.laststatus     = 2
vim.o.textwidth      = 0
vim.o.foldenable     = false
vim.o.spell          = false
vim.o.autoindent     = true
vim.o.breakindent    = true
vim.o.expandtab      = true
vim.o.hidden         = true
vim.o.hlsearch       = true
vim.o.ignorecase     = true
vim.o.incsearch      = true
vim.o.list           = true
vim.o.number         = true
vim.o.relativenumber = true
vim.o.ruler          = true
vim.o.showcmd        = true
vim.o.showmode       = true
vim.o.smartcase      = true
vim.o.smarttab       = true
vim.o.title          = true
vim.o.undofile       = true
vim.o.wildmenu       = true
vim.o.wrap           = true

vim.opt.listchars = {trail = '»', tab = '»-'}
vim.opt.fillchars:append('vert:│')


vim.g.code_action_menu_show_details    = false
vim.g.cursorhold_updatetime            = 50
vim.g.haskell_indent_after_bare_where  = 2
vim.g.haskell_indent_before_where      = 2
vim.g.haskell_indent_case_alternative  = 1
vim.g.haskell_indent_guard             = 4
vim.g.haskell_indent_if                = 0
vim.g.haskell_indent_in                = 0
vim.g.startify_fortune_use_unicode     = 1
vim.g.stylishask_on_save               = 0
vim.g.tex_conceal                      = ''
vim.g.undotree_SetFocusWhenToggle      = 1
vim.g.undotree_WindowLayout            = 2
vim.g.vim_json_syntax_conceal          = 0
vim.g.vim_markdown_conceal             = 0
vim.g.vim_markdown_conceal_code_blocks = 0

vim.g.python3_host_prog = vim.env.HOME .. '/anaconda3/envs/nvim/bin/python'

local function trimWhiteSpace()
  local save = vim.fn.winsaveview()
  vim.cmd[[%s/\\\@<!\s\+$//e]]
  vim.fn.winrestview(save)
end

map({'n', 'x'} , '-'       , '$'                       )
map('n', '<leader>J'       , ':join<CR>'               )
map('n', '<leader>O'       , 'O<ESC>'                  )
map('n', '<leader>V'       , 'V<C-g>'                  )
map('n', '<leader>o'       , 'o<ESC>'                  )
map('n', '<leader>s'       , ':%s/'                    )
map('n', '<leader>ns'      , ':set nospell!<CR>'       )
map('n', '<leader>vip'     , 'vip<C-g>'                )
map('n', '<leader>viw'     , 'viw<C-g>'                )
map('n', '<space>'         , 'za'                      )
map('n', '<leader><leader>', ':noh<CR>'                )
map('n', '<leader>r'       , ':source $MYVIMRC|noh<CR>')
map('n', '<leader><TAB>'   , '<C-w><C-w>', {remap = true})
map('n', '<leader>t'       , trimWhiteSpace            )

map('n', 'J'       , ':bprevious<CR>', {silent = true})
map('n', 'K'       , ':bnext<CR>'    , {silent = true})
map('n', '<C-j>'   , ':resize -1<CR>', {silent = true})
map('n', '<C-k>'   , ':resize +1<CR>', {silent = true})

map('i', '<PageDown>', '<NOP>')
map('i', '<PageUp>'  , '<NOP>')
map('x', '<PageDown>', '<C-f>', {remap = true})

map('x', '<PageUp>'  , '<C-b>', {remap = true})
map('n', '<PageDown>', '<C-f>', {remap = true})
map('n', '<PageUp>'  , '<C-b>', {remap = true})


autocmd('FileType', {
  pattern = {'html', 'lua', 'css', 'xml', 'markdown', 'journal'},
  command = 'setlocal shiftwidth=2 tabstop=2 softtabstop=2',
})

autocmd('FileType', {
  pattern = 'python',
  command = 'nmap <leader>x :w<CR>:execute "!python " . expand("%:p")<CR>'
})

autocmd('FileType', {command = 'noh'})

autocmd('FileType', {
  pattern = 'help',
  command = 'noremap <buffer> <F1> :q<CR>'
})

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
  pattern = 'init.lua',
  command = 'cmap q qa'
})

autocmd('VimEnter', {
  pattern = '*kitty/*.conf',
  command = 'nmap <leader>x :w<CR>:execute' ..
    '"!fish -c refresh-kitty"<CR>'
})

autocmd('InsertLeavePre', {
  callback = function()
    if vim.o.modifiable then
      trimWhiteSpace()
    end
  end
})

autocmd('BufWritePost', {
  pattern = 'plugins.lua',
  command = 'PackerCompile'
})

-- autocmd('BufWinLeave', {command = 'silent! mkview'})
-- autocmd('BufWinEnter', {command = 'exe "normal zR" | silent! loadview'})
autocmd('BufWinEnter', {command = 'exe "normal zR"'})

vim.cmd[[
colorscheme cassiopeia
]]
if vim.env.TERM == 'xterm-kitty' then
  vim.cmd([[
    autocmd UIEnter * if v:event.chan ==# 0 | call chansend(v:stderr, "\x1b[>1u") | endif
    autocmd UILeave * if v:event.chan ==# 0 | call chansend(v:stderr, "\x1b[<1u") | endif
  ]])
end
