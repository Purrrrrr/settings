"call pathogen#infect()
"call pathogen#helptags()

call plug#begin(stdpath('data') . '/plugged')
"For nice color scheme
Plug 'rakr/vim-one'
"Searching tools
Plug 'ctrlpvim/ctrlp.vim'
Plug 'mileszs/ack.vim'
"Filetype helpers
Plug 'leafgarland/typescript-vim'
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

"Plug 'neomake/neomake'
"Plug 'luochen1990/rainbow'
"Plug 'ruanyl/vim-sort-imports'
call plug#end()

set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
set number relativenumber
set ignorecase
set smartcase

let g:ackprg = 'ag --vimgrep'
let g:netrw_liststyle = 3
let g:ale_fixers = {
 \ 'javascript': ['eslint_d']
 \ }
let g:ale_fix_on_save = 1
"let g:import_sort_auto = 1
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_max_files=0
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

set statusline  =\ %<%F            "full path
set statusline +=\ %m              "modified flag
set statusline +=\ %=\ %l/%L\      "Rownumber
set statusline +=\ col:%03v\       "Colnr
set statusline +=0x%04B\           "character under cursor

"Credit joshdick
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

" Hilight extra whitespace at the end of nonempty lines
highlight ExtraWhitespace ctermbg=yellow guibg=yellow
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=yellow guibg=yellow
match ExtraWhitespace /\s\+\%#\@<!$/

let g:rainbow_active = 1
let g:one_allow_italics = 1
colorscheme one
set background=light
call one#highlight('StatusLine', '000000', 'bbbbbb', 'none')

func! ToggleBg()
    if (&background == "dark")
      set background=light
      call one#highlight('StatusLine', 'ffffff', '444444', 'none')
    else
      set background=dark
      call one#highlight('StatusLine', '000000', 'bbbbbb', 'none')
    endif
endfu

let mapleader = "\<Space>"
let maplocalleader = "-"
nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>d :call ToggleBg()<CR>

" ----------------------------------------------------------------------------
" move to the window in the direction shown, or create a new split in that
" direction
" ----------------------------------------------------------------------------
func! WinMove(key)
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
endfu

nnoremap <silent> <C-h> :call WinMove('h')<cr>
nnoremap <silent> <C-j> :call WinMove('j')<cr>
nnoremap <silent> <C-k> :call WinMove('k')<cr>
nnoremap <silent> <C-l> :call WinMove('l')<cr>
map <Leader>m <C-w>100+
map <Leader>c :noh<CR>

let g:toggle_list_no_mappings="true"
map <script> <silent> <Leader>e :call ToggleLocationList()<CR>

map  esc
inoremap <S-CR> <Esc>
inoremap <C-CR> <Esc>

call one#highlight('StatusLine', '000000', 'bbbbbb', 'none')

set expandtab
