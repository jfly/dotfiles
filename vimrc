execute pathogen#infect()

let mapleader = ","
" Fast reloading of the .vimrc
map <leader>r :source ~/.vimrc<cr>

" http://vim.wikia.com/wiki/Indenting_source_code
filetype plugin indent on
syntax on

" http://ethanschoonover.com/solarized/vim-colors-solarized
syntax enable

" Tweak because I (jfly) changed the solarized terminal pallete a bit, but
" still want to use the default terminal background color...
let g:solarized_termtrans=1

set background=dark
colorscheme solarized
" Shortcuts to change our colorscheme
noremap <c-F5> :set background=dark<CR>
noremap <c-F6> :set background=light<CR>

" Experimenting with cross file search
" See: http://stackoverflow.com/a/25879734
noremap <leader>f :exec "cope \| :silent Ggrep ".input(">>> ")." \| redraw!"<CR>
noremap ]q :cnext<CR>
noremap [q :cprevious<CR>

" http://stackoverflow.com/a/234578
set smartindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" http://vim.wikia.com/wiki/256_colors_in_vim
set t_Co=256

" Search case insensitive, unless there's a capital letter (smartcase),
" or * or # was used to initiate a search (huge thanks to Justin Jaffray for
" writing the scripts for this last trick!)
set hlsearch
set incsearch
set smartcase
set ignorecase
nnoremap * :call feedkeys("/\\C\\<" . expand("<cword>") . "\\>\r")<CR>
nnoremap # :call feedkeys("?\\C\\<" . expand("<cword>") . "\\>\r")<CR>

" http://usevim.com/2012/10/19/vim101-set-hidden/
set hidden

" Always show status line
set laststatus=2

" Prevent the cursor from changing the current column when jumping to other
" lines within the window?
set nostartofline

" http://vim.wikia.com/wiki/Easier_buffer_switching
set wildchar=<Tab> wildmenu wildmode=full
set wildcharm=<C-Z>

"""""" filetypes
au BufRead,BufNewFile *.md set filetype=markdown
au BufRead,BufNewFile Vagrantfile setfiletype ruby
""""""

"""""" ctrlp configuration
" http://stackoverflow.com/a/22784889
if executable('ag')
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
let g:ctrlp_working_path_mode = 'wra'
let g:ctrlp_max_files = 1000000
let g:ctrlp_max_depth = 40
" https://coderwall.com/p/hk_bwg
" Ignore some folders and files for CtrlP indexing
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|pyc)$',
  \ }
map <leader>b :CtrlPBuffer<CR>
""""""

"""""" airline configuration
" Display all buffers when there's only one tab open
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
""""""

" http://unix.stackexchange.com/a/30757
set tabpagemax=100
