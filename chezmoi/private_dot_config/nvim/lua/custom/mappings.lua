---@type MappingsTable
local M = {}

M.nvimtree = {
  n = {
    ["-"] = { "<cmd> NvimTreeFindFileToggle<Cr>", "Show current file in nvimtree"}
  }
}
M.custom= {
  n = {
    [";"] = { ":", "enter command mode", opts = { nowait = true } },
    ["<leader>q"] = { "<cmd>q<Cr>", "Close window" },
    ["<leader>w"] = { "<cmd>w<Cr>", "Save" },
    ["<leader>y"] = { "+y", "Copy from system keyboard" },
    ["<leader>p"] = { "+p", "Paste from system keyboard" },
    ["<leader>m"] = { "<C-w>100+", "Make current window tall" },
    ["<leader>c"] = { "<cmd>nohlsearch<CR>", "Clear search hilight" },
    ["<C-h>"] = { function () WinMove("h") end, "Test" },
    ["<C-j>"] = { function () WinMove("j") end, "Test" },
    ["<C-k>"] = { function () WinMove("k") end, "Test" },
    ["<C-l>"] = { function () WinMove("l") end, "Test" },
    ["<C-W>m"] = { "<cmd>WinShift<CR>", "WinShift" },
    ["<C-W>S"] = { "<cmd>WinShift swap<CR>", "WinShift Swap" },
    ["<leader>o"] = { "<cmd> Telescope find_files <CR>", "Find files" },
  },
}

function WinMove(key)
  local prevWin = vim.api.nvim_get_current_win()
  vim.cmd("wincmd " .. key)
  local curWin = vim.api.nvim_get_current_win()
  print(prevWin..", "..curWin)

  if (prevWin == curWin) then
    if key == "k" or key == "j" then
      vim.cmd("wincmd s")
    else
      vim.cmd("wincmd v")
    end
    vim.cmd("wincmd " .. key)
  end
end

M.lspconfig = {

  n = {
    ["<F2>"] = {
      function()
        require("nvchad_ui.renamer").open()
      end,
      "LSP rename",
    },

    ["<leader><space>"] = {
      function()
        vim.lsp.buf.code_action()
      end,
      "LSP code action",
    },

    ["<leader>e"] = {
      function()
        vim.diagnostic.goto_next()
      end,
      "Goto next",
    },
  }
}
-- more keybinds!

return M
