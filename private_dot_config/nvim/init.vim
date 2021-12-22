call plug#begin(stdpath('data') . '/plugged')
"For nice color scheme
Plug 'rakr/vim-one'
"Searching tools
Plug 'ctrlpvim/ctrlp.vim'
Plug 'mileszs/ack.vim'
"Filetype helpers
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'jparise/vim-graphql'
Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'wavded/vim-stylus'
Plug 'lepture/vim-jinja'

"Hilight word under cursor
Plug 'dominikduda/vim_current_word'

"Git integration
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

"Automatically close quotes, parenthesis, brackets etc.
Plug 'Raimondi/delimitMate'

"File management commands
Plug 'tpope/vim-eunuch'

"File browsing fixes
Plug 'tpope/vim-vinegar'

"For quickly jumping to errors with leader-e
Plug 'milkypostman/vim-togglelist'

"LSP and typescript
Plug 'nvim-lua/plenary.nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'jose-elias-alvarez/null-ls.nvim'

"Snippets
"You need pynvim and neovim packages for pip3 installed
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'Purrrrrr/vim-1loc-ultisnips', { 'branch' : 'main'}
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
call plug#end()


let g:deoplete#enable_at_startup = 1
" disable autocomplete
" let g:deoplete#disable_auto_complete = 1
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

let g:vim_current_word#highlight_current_word = 0

let g:vim_jsx_pretty_colorful_config = 1 " default 0

set tabstop=8 softtabstop=2 shiftwidth=2 expandtab
set number relativenumber
set ignorecase smartcase
set incsearch
set nofixendofline

"Show relative numbers only on the focused buffer
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

let g:ackprg = 'ag --vimgrep'
let g:netrw_liststyle = 3
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_max_files=0
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

set statusline  =\ %<%F\ %y        "full path and filetype
set statusline +=\ %m              "modified flag
set statusline +=\ %=\ %l/%L\      "Rownumber
set statusline +=\ col:%03v\       "Colnr
set statusline +=0x%04B\           "character under cursor


" ----------------------------------------------------------------------------
" move to the window in the direction shown, or create a new split in that
" direction
" ----------------------------------------------------------------------------
function! s:WinMove(key)
    let t:curwin = winnr()
    exec "wincmd ".a:key
    if (t:curwin == winnr())
        if (match(a:key,'[jk]'))
            wincmd v
        else
            wincmd s
        endif
        exec "wincmd ".a:key
    endif
endfunction

nnoremap <silent> <C-h> :call <SID>WinMove('h')<cr>
nnoremap <silent> <C-j> :call <SID>WinMove('j')<cr>
nnoremap <silent> <C-k> :call <SID>WinMove('k')<cr>
nnoremap <silent> <C-l> :call <SID>WinMove('l')<cr>

" ----------------------------------------------------------------------------
" Setup color scheme "one"
" ----------------------------------------------------------------------------

"Credit joshdick Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
if (empty($TMUX))
  if (has("termguicolors"))
    set termguicolors
  endif
endif

" Hilight extra whitespace at the end of nonempty lines
highlight ExtraWhitespace ctermbg=red guibg=red
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
"Hilight extra whitespace at line ends, but only if it isn't before the cursor
match ExtraWhitespace /\s\+\%#\@<!$/

" Hilight non breaking spaces
highlight NonBreakingSpace ctermbg=red guibg=red
autocmd ColorScheme * highlight NonBreakingSpace ctermbg=red guibg=red
match NonBreakingSpace /\%xA0/

let g:one_allow_italics = 1
colorscheme one
set background=light
call one#highlight('StatusLine', 'ffffff', '444444', 'none')

func! s:ToggleBg()
    if (&background == "dark")
      set background=light
      call one#highlight('StatusLine', 'ffffff', '444444', 'none')
    else
      set background=dark
      call one#highlight('StatusLine', '000000', 'bbbbbb', 'none')
    endif
endfunc

set expandtab

" ----------------------------------------------------------------------------
"LSP config
" ----------------------------------------------------------------------------

lua << EOF
local lsp = require'lspconfig'

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end


  vim.cmd("command! LspDef lua vim.lsp.buf.definition()")
  vim.cmd("command! LspFormatting lua vim.lsp.buf.formatting()")
  vim.cmd("command! LspCodeAction lua vim.lsp.buf.code_action()")
  vim.cmd("command! LspHover lua vim.lsp.buf.hover()")
  vim.cmd("command! LspRename lua vim.lsp.buf.rename()")
  vim.cmd("command! LspRefs lua vim.lsp.buf.references()")
  vim.cmd("command! LspTypeDef lua vim.lsp.buf.type_definition()")
  vim.cmd("command! LspImplementation lua vim.lsp.buf.implementation()")
  vim.cmd("command! LspDiagPrev lua vim.lsp.diagnostic.goto_prev()")
  vim.cmd("command! LspDiagNext lua vim.lsp.diagnostic.goto_next()")
  vim.cmd("command! LspDiagLine lua vim.lsp.diagnostic.show_line_diagnostics()")
  vim.cmd("command! LspSignatureHelp lua vim.lsp.buf.signature_help()")

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
    -- Mappings.
  local opts = { noremap=true, silent=false }

  buf_set_keymap('n', '<C-e>', '<cmd>lua vim.lsp.stop_client(vim.lsp.get_active_clients())<CR>', opts)
  buf_set_keymap('n', '<F2>', '<cmd>LspRename<CR>', opts)
  buf_set_keymap('n', '<Leader>e', '<cmd>LspDiagNext<CR>', opts)
  buf_set_keymap('n', '<Leader>s', '<cmd>split<CR><cmd>LspDef<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>LspDef<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>LspRefs<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>LspImplementation<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>LspHover<CR>', opts)
  buf_set_keymap('n', '<Leader><space>', '<cmd>LspCodeAction<CR>', opts)
--[[ "LSP
"nnoremap <Leader>j :LspImplementation<CR>
"nnoremap <Leader>r :LspReferences<CR>
"nnoremap <Leader>p :LspPeekDefinition<CR>
"nnoremap <Leader>s :split<CR>:LspDefinition<CR>
"nnoremap <Leader>d :LspDefinition<CR>

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  -- buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  -- buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  -- buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
--]]
end

lsp.tsserver.setup {
  on_attach = on_attach
  }
lsp.eslint.setup{}

EOF

" ----------------------------------------------------------------------------
" General keybindings
" ----------------------------------------------------------------------------

let mapleader = "\<Space>"
let maplocalleader = "-"
nnoremap <C-PageUp> :prev<CR>
nnoremap <C-PageDown> :next<CR>
nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>d :call  <SID>ToggleBg()<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>a :Ack<CR>
" " Copy to clipboard
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y
nnoremap  <leader>yy  "+yy

" " Paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P
"Clear search hilight
noremap <Leader>c :nohlsearch<CR>
"Reload vim config
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <leader>vv :split $MYVIMRC<CR>
"Max current window height
noremap <Leader>m <C-w>100+

let g:toggle_list_no_mappings="true"

map  esc
inoremap <S-CR> <Esc>
inoremap <C-CR> <Esc>
