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
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'wavded/vim-stylus'

"Automatically close quotes, parenthesis, brackets etc.
Plug 'Raimondi/delimitMate'

"File management commands
Plug 'tpope/vim-eunuch'

"File browsing fixes
Plug 'tpope/vim-vinegar'

"For quickly jumping to errors with leader-e
Plug 'milkypostman/vim-togglelist'

"LSP and typescript
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'ryanolsonx/vim-lsp-typescript'
call plug#end()

set tabstop=8 softtabstop=2 shiftwidth=2 expandtab
set number relativenumber
set ignorecase
set smartcase

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

set statusline  =\ %<%F\ %y         "full path and filetype
set statusline +=\ %m              "modified flag
set statusline +=\ %=\ %l/%L\      "Rownumber
set statusline +=\ col:%03v\       "Colnr
set statusline +=0x%04B\           "character under cursor

" ----------------------------------------------------------------------------
" General keybindings
" ----------------------------------------------------------------------------

let mapleader = "\<Space>"
let maplocalleader = "-"
nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>d :call ToggleBg()<CR>
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

" ----------------------------------------------------------------------------
"LSP toolset keys
" ----------------------------------------------------------------------------
noremap <F2> :LspRename<CR>
nnoremap <Leader>h :LspHover<CR>
nnoremap <Leader>p :LspPeekDefinition<CR>
nnoremap <Leader>s :split<CR>:LspDefinition<CR>
"Show LSP Hover after 300 milliseconds, but only if no other preview window is
"open right now
set updatetime=500
augroup autohover
  autocmd!
  autocmd CursorHold *.ts,*.tsx call LspHoverWhenNoPreview()
augroup END

function! LspHoverWhenNoPreview()
  if PreviewWindowOpened() == 0
    LspHover
  endif
endfunction


function! PreviewWindowOpened()
  for nr in range(1, winnr('$'))
    if getwinvar(nr, "&pvw") == 1
      " found a preview
      return 1
    endif
  endfor
  return 0
endfunction


" ----------------------------------------------------------------------------
" move to the window in the direction shown, or create a new split in that
" direction
" ----------------------------------------------------------------------------
function! WinMove(key)
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

nnoremap <silent> <C-h> :call WinMove('h')<cr>
nnoremap <silent> <C-j> :call WinMove('j')<cr>
nnoremap <silent> <C-k> :call WinMove('k')<cr>
nnoremap <silent> <C-l> :call WinMove('l')<cr>

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
highlight ExtraWhitespace ctermbg=yellow guibg=yellow
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=yellow guibg=yellow
match ExtraWhitespace /\s\+\%#\@<!$/

let g:one_allow_italics = 1
colorscheme one
set background=light
call one#highlight('StatusLine', 'ffffff', '444444', 'none')

func! ToggleBg()
    if (&background == "dark")
      set background=light
      call one#highlight('StatusLine', 'ffffff', '444444', 'none')
    else
      set background=dark
      call one#highlight('StatusLine', '000000', 'bbbbbb', 'none')
    endif
endfunc

set expandtab
