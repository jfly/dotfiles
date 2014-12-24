" http://vim.wikia.com/wiki/256_colors_in_vim
set t_Co=256
execute pathogen#infect()

let mapleader = ","

"Fast reloading of the .vimrc
map <leader>r :source ~/.vimrc<cr>

" Thank you justin jaffray
nnoremap * :call feedkeys("/\\C\\<" . expand("<cword>") . "\\>\r")<cr>
nnoremap # :call feedkeys("?\\C\\<" . expand("<cword>") . "\\>\r")<cr>

set smartcase
set ignorecase

set hidden
set laststatus=2
set nostartofline

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

" Display all buffers when there's only one tab open
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'

" http://vim.wikia.com/wiki/Easier_buffer_switching
set wildchar=<Tab> wildmenu wildmode=full
set wildcharm=<C-Z>

map <leader>b :CtrlPBuffer<CR>

au BufRead,BufNewFile *.md set filetype=markdown

" http://vim.wikia.com/wiki/Indenting_source_code
filetype plugin indent on
