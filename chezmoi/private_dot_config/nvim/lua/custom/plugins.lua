local overrides = require("custom.configs.overrides")

---@type NvPluginSpec[]
local plugins = {

  -- Override plugin definition options

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- format & linting
      {
        "jose-elias-alvarez/null-ls.nvim",
        config = function()
          require "custom.configs.null-ls"
        end,
      },
    },
    config = function()
      require "plugins.configs.lspconfig"
      require "custom.configs.lspconfig"
    end, -- Override to setup mason-lspconfig
  },

  {
    "hrsh7th/nvim-cmp",
    opts = function()
      local opts = require "plugins.configs.cmp"
      local cmp = require "cmp"
      table.insert(opts.sources, { name = "codeium"})
      opts.mapping["<CR>"] = cmp.mapping.confirm { select = false, }

      return opts
    end,
  },

  -- override plugin configs
  {
    "williamboman/mason.nvim",
    opts = overrides.mason
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = overrides.treesitter,
  },

  {
    "nvim-tree/nvim-tree.lua",
    opts = overrides.nvimtree,
    lazy = false,
  },

  -- Install a plugin
  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    config = function()
      require("better_escape").setup()
    end,
  },

  {
    "tpope/vim-eunuch",
    lazy = false,
  },

--"Hilight and remove trailing whitespace
  {
    "ntpeters/vim-better-whitespace";
    config = function()
      vim.g.better_whitespace_enabled = 1
      vim.g.strip_whitespace_on_save = 1
      vim.g.strip_whitespace_confirm = 0
    end,
  },

  {
    "dominikduda/vim_current_word",
    lazy = false,
    config = function()
      vim.g["vim_current_word#highlight_current_word"] = 0
    end,
  },
  {
    "tpope/vim-surround",
    lazy = false,
  },
  {
    "sindrets/winshift.nvim",
    lazy = false,
  },

  --Filetype plugins
  {
    "wavded/vim-stylus",
    lazy = false,
  },
  {
    "lepture/vim-jinja",
    lazy = false,
  },
  {
    "kchmck/vim-coffee-script",
    lazy = false,
    config = function()
      vim.cmd.autocmd("BufNewFile,BufRead *.cjsx setfiletype coffee")
    end,
  },

  {
    "jcdickinson/codeium.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "hrsh7th/nvim-cmp",
    },
    config = function()
      require("codeium").setup({
      })
    end,
    lazy = false,
    enabled = false,
  },
  -- To make a plugin not be loaded
  -- {
  --   "NvChad/nvim-colorizer.lua",
  --   enabled = false
  -- },

  -- All NvChad plugins are lazy-loaded by default
  -- For a plugin to be loaded, you will need to set either `ft`, `cmd`, `keys`, `event`, or set `lazy = false`
  -- If you want a plugin to load on startup, add `lazy = false` to a plugin spec, for example
  -- {
  --   "mg979/vim-visual-multi",
  --   lazy = false,
  -- }
}

return plugins
