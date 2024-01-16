---@type ChadrcConfig
local M = {}

vim.opt.relativenumber = true

-- Path to overriding theme and highlights files
local highlights = require "custom.highlights"

M.ui = {
  theme = "one_light",
  theme_toggle = { "one_light", "one_light" },

  hl_override = highlights.override,
  hl_add = highlights.add,
  statusline = {
    overriden_modules = function()
      local st_modules = require "nvchad_ui.statusline.default"
      return {
        fileInfo = function ()
          local filename = (vim.fn.expand "%" == "" and "Empty ") or vim.fn.expand "%:t"
          local info = st_modules.fileInfo()
          local cutoff = string.find(info, filename, 1, false)
          local path = vim.fn.expand('%:p:h')
          return string.sub(info,0,cutoff-1) .. path .. "/" .. string.sub(info,cutoff)
        end,
      }
    end
  }
}

M.plugins = "custom.plugins"

-- check core.mappings for table structure
M.mappings = require "custom.mappings"

return M
