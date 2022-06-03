map = vim.keymap.set
autocmd = vim.api.nvim_create_autocmd

return require('packer').startup {
  function()
    use {'nacro90/numb.nvim',
      event = 'CmdlineEnter',
      config = function() require('numb').setup() end
    }
    use {'lukas-reineke/virt-column.nvim',
      config = function() require('virt-column').setup() end
    }
    use {'yamatsum/nvim-nonicons',
      requires = {'kyazdani42/nvim-web-devicons'}
    }
    use {'kozlov721/cassiopeia.nvim'}
    use {'gelguy/wilder.nvim',
      run = ':UpdateRemotePlugins',
      event = 'CmdlineEnter',
      rocks = 'pcre2',
      requires = 'romgrk/fzy-lua-native',
      config = function()
        require('wilder').setup {
          modes = {':', '/', '?'}
        }
        local wilder = require('wilder')
        local scale = {
          '#f4468f',
          '#fd4a85',
          '#ff507a',
          '#ff566f',
          '#ff5e63',
          '#ff6658',
          '#ff704e',
          '#ff7a45',
          '#ff843d',
          '#ff9036',
          '#f89b31',
        }
        local gradient = {}
        for i, fg in ipairs(scale) do
          gradient[i] = 'WilderGradient' .. i
          vim.cmd(string.format('highlight %s guifg=%s', gradient[i], fg))
        end
        wilder.set_option {
          renderer = wilder.popupmenu_renderer(
            wilder.popupmenu_palette_theme {
              highlighter = wilder.highlighter_with_gradient {
                wilder.lua_pcre2_highlighter(),
                wilder.lua_fzy_highlighter()
              },
              highlights = {
                border = 'Normal',
                gradient = gradient
              },
              border = 'rounded',
              left = {
                ' ',
                wilder.popupmenu_devicons(),
                wilder.popupmenu_buffer_flags {
                  flags = ' a + ',
                  icons = {
                    ['+'] = '',
                    a     = '',
                    h     = ''
                  }
                }
              },
              right = {' ', wilder.popupmenu_scrollbar()},
              max_height = '40%',
              min_width  = '65%',
              reverse = 0,
            }
          ),
          pipeline = wilder.branch(
            wilder.cmdline_pipeline {
              fuzzy = 1,
              set_pcre2_pattern = 1
            },
            wilder.python_search_pipeline {
              pattern = 'fuzzy',
              sorter = wilder.python_difflib_sorter(),
              engire = 're2'
            }
          )
        }
      end
    }
    use {'tpope/vim-repeat'}
    use {'nvim-lualine/lualine.nvim',
      requires = 'kozlov721/cassiopeia.nvim',
      config = function()
        require('lualine').setup {
          options  = {
            theme = 'cassiopeia',
            component_separators = '',
            section_separators = {
              left = '',
              right = ''
            },
          },
          sections = {
            lualine_b = {
              'branch',
              'diff',
              { 'diagnostics',
                symbols = {
                  error = '✘ ',
                  warn  = '🛈 ',
                  hint  = '🎔 ',
                  info  = '💡'
                }
              }
            },
            lualine_c = {
              { 'filename',
                file_status = true,
                path = 0,
                shorting_target = 40,
                symbols = {
                  modified = ' ●',
                  readonly = ' 🔒',
                  unnamed = '[No Name]',
                }
              }
            },
            lualine_x = {'filetype'}
          },
          tabline  = {
            lualine_a = {
              {'buffers',
                max_length = function()
                  return vim.o.columns * 3 / 4
                end
              }
            },
            lualine_b = {},
            lualine_c = {},
            lualine_x = {},
            lualine_y = {'branch'},
            lualine_z = {}
          }
        }
      end,
    }
    use {'anuvyklack/pretty-fold.nvim',
      requires = 'anuvyklack/nvim-keymap-amend',
      config = function()
        require('pretty-fold').setup()
        require('pretty-fold.preview').setup {
          key = 'h',
          border = 'rounded'
        }
      end
    }
    use {'lewis6991/gitsigns.nvim',
      requires = 'nvim-lua/plenary.nvim',
      config = function()
        require('gitsigns').setup {
          on_attach = function(bn)
            local gs = package.loaded.gitsigns
            local function bmap(m, lhs, rhs, opts)
              opts = opts or {}
              opts.buffer = bn
              map(m, lhs, rhs, opts)
            end
            bmap('n', ']c', "&diff ? ']c' : '<cmd>Gitsigns next_hunk<CR>'",
             {expr=true})
            bmap('n', '[c', "&diff ? '[c' : '<cmd>Gitsigns prev_hunk<CR>'",
             {expr=true})
            bmap({'n', 'x'}, '<leader>hs', gs.stage_hunk)
            bmap({'n', 'x'}, '<leader>hr', gs.reset_hunk)
            bmap({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
            bmap('n', '<leader>hS', gs.stage_buffer)
            bmap('n', '<leader>hu', gs.undo_stage_hunk)
            bmap('n', '<leader>hR', gs.reset_buffer)
            bmap('n', '<leader>hp', gs.preview_hunk)
            bmap('n', '<leader>hb', function() gs.blame_line{full = true} end)
            bmap('n', '<leader>tb', gs.toggle_current_line_blame)
            bmap('n', '<leader>hd', gs.diffthis)
            bmap('n', '<leader>hD', function() gs.diffthis('~') end)
            bmap('n', '<leader>td', gs.toggle_deleted)
          end
        }
      end
    }
    use {'phaazon/hop.nvim',
      keys = {'ff', 'fl', 'fw', 'F'},
      branch = 'v1',
      event = 'CmdlineEnter',
      config = function()
        require('hop').setup {
          keys = 'etovxqpdygfblzhckisuran'
        }
        map('n', 'ff', ':HopChar1<CR>'  )
        map('n', 'fl', ':HopLine<CR>'   )
        map('n', 'fw', ':HopWord<CR>'   )
        map('n', 'F' , ':HopPattern<CR>')
      end
    }
    use {'mfussenegger/nvim-treehopper',
      event = 'ModeChanged',
      cmd = '*:o',
      keys = 'ft',
      config = function()
        map({'v', 'n'}, 'ft', require('tsht').nodes)
        map('o', 'm', ':<C-U>lua require("tsht").nodes()<CR>', {silent = true})
      end
    }
    use {'lukas-reineke/indent-blankline.nvim',
      config = function() require('indent_blankline').setup() end
    }
    use {'jesseleite/vim-noh', event = 'CmdlineEnter'}
    use {'ellisonleao/glow.nvim',
      keys = '<leader>g',
      ft = {'markdown'},
      config = function ()
          map('n', '<leader>g', ':Glow<CR>', {silent = true})
      end
    }
    use {'akinsho/toggleterm.nvim',
      keys = [[<C-\>]],
      config = function()
        require('toggleterm').setup {
          direction     = 'float',
          open_mapping  = [[<C-\>]],
          close_on_exit = false
        }
      end
    }
    use {'sindrets/winshift.nvim',
      event = 'WinEnter',
      config = function()
        map('n', '<C-W><C-M>', '<Cmd>WinShift<CR>')
        map('n', '<C-W>m'    , '<Cmd>WinShift<CR>')
      end
    }
    use {'https://gitlab.com/yorickpeterse/nvim-window.git',
      event = 'WinEnter',
      config = function()
        require('nvim-window').setup {
          chars = {'q', 'w', 'e', 'r', 'j', 'k', 'l'}
        }
        map('n', '<leader>w', require('nvim-window').pick, {silent = true})
      end
    }
    use {'rmagatti/goto-preview',
      after = 'telescope.nvim',
      config = function()
        require('goto-preview').setup {
          default_mappings = true
        }
      end
    }
    use {'winston0410/range-highlight.nvim',
      event = 'CmdlineEnter',
      requires = 'winston0410/cmd-parser.nvim',
      config = function() require('range-highlight').setup() end
    }
    use {'nvim-treesitter/playground',
      event = 'CmdlineEnter',
      run = ':TSInstall query'
    }
    use {'RRethy/nvim-treesitter-endwise',
      event = 'InsertEnter',
      ft = {'lua', 'bash'},
      config = function()
        require('nvim-treesitter.configs').setup {
          endwise = {enable = true}
        }
      end
    }
    use {'p00f/nvim-ts-rainbow'}
    use {'nvim-treesitter/nvim-treesitter',
      run = ':TSUpdate',
      config = function()
        require('nvim-treesitter.configs').setup {
          ensure_installed = 'all',
          sync_install     = false,
          highlight = {
            enable  = true,
            disable = {'haskell'},
            additional_vim_regex_highlighting = {'haskell'}
          },
          rainbow = {
            enable         = true,
            extended_mode  = true,
            max_file_lines = nil,
          },
        }
      end
    }
    use {'chentoast/marks.nvim',
      config = function() require('marks').setup{} end
    }
    use {'mbbill/undotree',
      keys = '<leader>e',
      config = function () map('n', '<leader>e', ':UndotreeToggle<CR>') end
    }
    use {'chrisbra/unicode.vim',
      keys = '<leader>un',
      config = function() map('n', '<leader>un', ':UnicodeSearch!') end
    }
    use {'terrortylor/nvim-comment',
      keys = {'<leader>c', '<leader>cc', '<leader>cy'},
      config = function()
        require('nvim_comment').setup {
          line_mapping = '<leader>cc',
          operator_mapping = '<leader>c',
        }
        map('x', '<leader>cy', 'ygv:CommentToggle<CR>', {silent = true})
        map('x', '<leader>cc', ':CommentToggle<CR>'   , {silent = true})
        map('n', '<leader>cy', 'yy:CommentToggle<CR>' , {silent = true})
      end
    }
    use {'neovim/nvim-lspconfig',
      requires = {
        {'kosayoda/nvim-lightbulb'},
        {'jubnzv/virtual-types.nvim'},
        {'ray-x/lsp_signature.nvim'},
        {'weilbith/nvim-code-action-menu'},
        {'ms-jpq/coq.thirdparty', branch = '3p'},
        {'ms-jpq/coq.artifacts', branch = 'artifacts'},
        {'ms-jpq/coq_nvim',
          branch = 'coq',
          run = ':COQdeps',
        }
      },
      config = function()
        require('lsp_signature').setup {
          hint_enable = false,
          hi_parameter = 'RedUnderline'
        }
        vim.cmd [[
          sign define DiagnosticSignError text=✘ texthl=DiagnosticSignError
          sign define DiagnosticSignWarn text=🛈  texthl=DiagnosticSignWarn
          sign define DiagnosticSignHint text=🎔  texthl=DiagnosticSignInfo
          sign define DiagnosticSignInfo text=💡 texthl=DiagnosticSignHint
        ]]
        local on_attach = function(_, bn)
          local function bmap(m, lhs, rhs)
            map(m, lhs, rhs, {buffer = bn})
          end
          bmap('n', '<C-k>'     , vim.lsp.buf.signature_help)
          bmap('n', '<leader>b' , '<cmd>CodeActionMenu<CR>' )
          bmap('n', '<leader>rn', vim.lsp.buf.rename        )
          bmap('n', '<leader>f' , vim.lsp.buf.formatting    )
          bmap('n', 'H'         , vim.lsp.buf.hover         )
          bmap('n', '[d'        , vim.diagnostic.goto_prev  )
          bmap('n', ']d'        , vim.diagnostic.goto_next  )
          bmap('n', 'gD'        , vim.lsp.buf.declaration   )
          bmap('n', 'gd'        , vim.lsp.buf.definition    )
          bmap('n', 'gi'        , vim.lsp.buf.implementation)
          bmap('n', 'gr'        , vim.lsp.buf.references    )

          vim.diagnostic.config {
            virtual_text = false,
            severity_sort = true
          }
          autocmd('CursorHold' , {callback = vim.lsp.buf.document_highlight})
          autocmd('CursorMoved', {callback = vim.lsp.buf.clear_references})
          autocmd('CursorHold' , {
            callback = require('nvim-lightbulb').update_lightbulb
          })
          autocmd('CursorHold' , {
              callback = function()
                  vim.diagnostic.open_float({focusable = false})
              end
          })
          require('lsp_signature').on_attach()
          require('virtualtypes').on_attach()
        end
        vim.g.coq_settings = {auto_start = 'shut-up'}
        local coq = require('coq')
        local lsp = require('lspconfig')
        local servers = {
          'pyright',
          'hls',
          'vimls',
          'ccls',
          'bashls',
          'rust_analyzer'
        }
        for _, server in ipairs(servers) do
          lsp[server].setup(coq.lsp_ensure_capabilities{
            on_attach = on_attach,
            flags = {debounce_text_changes = 150}
          })
        end
      end
    }
    use {'ZhiyuanLck/smart-pairs',
      after = 'coq_nvim',
      event = 'InsertEnter',
      config = function()
        require('pairs'):setup({
          pairs = {
            ['*'] = {
              {'(', ')'},
              {'[', ']'},
              {'{', '}'},
              {"'", "'", {ignore_pre = [[\w]]}},
              {'"', '"'},
              {'`', '`', {triplet = true}}
            },
            python = {
              {"'", "'", {triplet = true}},
              {'"', '"', {triplet = true}},
            },
            tex = {
              {"'", "'", {ignore_pre = '\\v(\\\\|\\S)'}},
              {'$', '$', {cross_line = true}},
            },
            rust = {
              {"|", "|"}
            }
          },
          default_opts = {
            ['*'] = {
              ignore_pre = [[\\]],
              ignore_after = '[^)\\]};, ]'
            }
          }
        })
        local opts = {silent = true, expr = true}

        -- To make smart-pairs work with coq.
        -- I also needed to modify smart-pairs/lua/pairs/init.lua to
        -- remove autocommands at the end of setup.
        vim.g.coq_settings = {keymap = {recommended = false}}

        map('i', '<Esc>'  , [[pumvisible() ? "\<C-e><ESC>" : "\<Esc>"]], opts)
        map('i', '<Tab>'  , [[pumvisible() ? "\<C-n>" : "\<Tab>"]], opts)
        map('i', '<S-Tab>', [[pumvisible() ? "\<C-p>" : "\<BS>"]] , opts)
        local function smartPairsCR()
          if vim.fn.pumvisible() ~= 0 then
            if vim.fn.complete_info({ 'selected' }).selected ~= -1 then
              require('pairs.utils').feedkeys('<c-y>', 'i')
            else
              require('pairs.utils').feedkeys('<c-e>', 'i')
              require('pairs.enter').type()
            end
          else
              require('pairs.enter').type()
          end
        end
        map('i', '<CR>', smartPairsCR, {silent = true})
      end
    }
    use {'junegunn/vim-easy-align',
      keys = {'<leader>a'},
      event = 'ModeChanged',
      cmd = '*:v',
      config = function()
        map('n', '<leader>a' , 'vipga', {remap = true})
        map('x', 'ga'        , '<Plug>(EasyAlign)'    )
      end
    }
    use {'alvan/vim-closetag',
      ft = {'html', 'xml'},
      event = 'InsertEnter'
    }
    use {'norcalli/nvim-colorizer.lua',
      config = function()
        require('colorizer').setup ({
          '*';
        }, { css = true })
      end
    }
    use {'dkarter/bullets.vim',
      event = 'InsertEnter',
      ft = {'markdown', 'text', 'gitcommit', 'scratch'}
    }
    use {'tversteeg/registers.nvim', branch = 'main'}
    use {'Pocco81/AutoSave.nvim',
      config = function()
        require('autosave').setup {
          execution_message = '',
          conditions = {
            filename_is_not = {'picom.conf'}
          }
        }
      end,
    }
    use {'antoinemadec/FixCursorHold.nvim'}
    use {'bryanmylee/vim-colorscheme-icons'}
    use {'nvim-telescope/telescope.nvim',
      requires = {
        'nvim-lua/plenary.nvim',
        'nvim-telescope/telescope-file-browser.nvim',
        {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'},
      },
      keys = {
        'tel',
        '<leader>fl',
        '<leader>fb',
        '<leader>fm',
        '<leader>q',
        'gpr',
        'gpd',
        'gpi'
      },
      event = 'CmdlineEnter',
      config = function()
        local telescope = require('telescope')
        telescope.setup {
          defaults = {
            layout_strategy = 'vertical',
            layout_config = {
              vertical = {width = 0.8}
            },
            mappings = {
              i = {
                ['<ESC>'] = require('telescope.actions').close
              }
            },
            prompt_prefix = " 🔍 ",
            color_devicons = true,
          },
          extensions = {
            fzf = {
              fuzzy = true,
              override_generic_sorter = true,
              override_file_sorter = true,
              case_mode = 'smart_case',
            },
            file_browser = {
              mappings = {
                i = {
                  ['<leader>q'] = require('telescope.actions').close
                }
              }
            }
          }
        }

        telescope.load_extension('file_browser')
        telescope.load_extension('fzf')

        map('n', 'tel'        , ':Telescope '                                  )
        map('n', '<leader>fl' , ':Telescope live_grep grep_open_files=true<CR>')
        map('n', '<leader>fb' , ':Telescope current_buffer_fuzzy_find<CR>'     )
        map('n', '<leader>fm' , ':Telescope man_pages sections=ALL<CR>'        )
        map('n', '<leader>q'  , ':Telescope file_browser<CR>'                  )
      end
    }
    use {'karb94/neoscroll.nvim',
      keys = {'<C-u>', '<C-d>', '<C-f>', '<C-b>', '<C-e>', '<C-y>'},
      config = function() require('neoscroll').setup() end
    }
    use {'max397574/better-escape.nvim',
      event = 'InsertEnter',
      config = function()
        require('better_escape').setup {
          mapping = {'jk'}
        }
      end
    }
    use {'echasnovski/mini.nvim',
      config = function()
        require('mini.surround').setup()
        map('n', 'sa', 'viWsa', {remap  = true})
        local starter = require('mini.starter')
        local f = assert(io.popen('fortune | cowsay', 'r'))
        local s = assert(f:read('*a'))
        f:close()
        starter.setup {
          evaluate_single = true,
          header = s,
          items = {
            starter.sections.builtin_actions(),
            starter.sections.recent_files(10),
          },
          content_hooks = {
            starter.gen_hook.adding_bullet('• '),
            starter.gen_hook.aligning('center', 'center')
          },
          footer = ''
        }
      end
    }
    use {'Vimjas/vim-python-pep8-indent',
      event = 'InsertEnter',
      ft = {'python'}
    }
    use {'alx741/vim-stylishask',
      ft = {'haskell'}
    }
    use {'neovimhaskell/haskell-vim',
      after = 'vim-stylishask',
      config = function() vim.cmd('syntax on') end
    }
  end,
  config = {
    display = {
      open_fn = function()
        return require('packer.util').float {
          border = 'rounded',
        }
      end
      }
  }
}
