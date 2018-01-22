" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')

" Declare the list of plugins.
Plug '~/.fzf'
Plug 'junegunn/fzf.vim'
Plug 'Valloric/MatchTagAlways'
Plug 'vim-airline/vim-airline'
Plug 'vim-scripts/matchit.zip'
Plug 'tomtom/tcomment_vim'
Plug 'altercation/vim-colors-solarized'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'

Plug 'sheerun/vim-polyglot'
Plug 'editorconfig/editorconfig-vim'

Plug 'janko-m/vim-test'
Plug 'kassio/neoterm'

" List ends here. Plugins become visible to Vim after this call.
call plug#end()

let mapleader = ","
" Fast reloading of .vimrc
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
" Ignore if the solarized colorscheme is not yet installed. This happens when
" we first run vim to run vim plug.
silent! colorscheme solarized
" Shortcuts to change our colorscheme
noremap 1<Backspace> :set background=dark<CR>
noremap 2<Backspace> :set background=light<CR>

" Experimenting with cross file search
" See: http://stackoverflow.com/a/25879734
noremap <leader>f :exec "cope \| :silent Ggrep ".shellescape(input(">>> ", expand("<cword>")))." \| redraw!"<CR>
noremap ]q :cnext<CR>
noremap [q :cprevious<CR>

" http://stackoverflow.com/a/234578
filetype plugin indent on
" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
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
au BufRead,BufNewFile Vagrantfile set filetype=ruby
au BufRead,BufNewFile *.jy set filetype=python
""""""

"""""" FZF configuration
noremap <c-p> :Files<CR>
noremap <leader>b :Buffers<CR>
""""""

"""""" vim-test configuration
function! FatRunnerStrategy(cmd)
    call system("fat-runner run " . shellescape("clear;" . a:cmd))
endfunction

let g:test#custom_strategies = {'fat_runner': function('FatRunnerStrategy')}
let g:test#strategy = "fat_runner"

" vim-test transformation to run nose tests via `make singletest`.
" If a command looks like "nosetests ...", transform it to
" "make singletest NOSEARGS='...'"
function! HonorTransform(cmd) abort
    if a:cmd =~ '^nosetests '
        let a:cmd_sans_nosetests = substitute(a:cmd, '^nosetests ', '', '')
        let a:cmd = 'make singletest NOSEARGS='.shellescape(a:cmd_sans_nosetests)
    else
        let a:new_cmd = a:cmd
    endif
    return a:new_cmd
endfunction

let g:test#custom_transformations = {'honor': function('HonorTransform')}
let g:test#transformation = 'honor'
""""""

nnoremap <leader>ts :w<CR>:TestSuite<CR>
nnoremap <leader>tf :w<CR>:TestFile<CR>
nnoremap <leader>tl :w<CR>:TestLast<CR>
nnoremap <leader>tn :w<CR>:TestNearest<CR>
""""""

"""""" airline configuration
" Display all buffers when there's only one tab open
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
""""""

" http://unix.stackexchange.com/a/30757
set tabpagemax=100

" https://webpack.github.io/docs/webpack-dev-server.html#working-with-editors-ides-supporting-safe-write
set backupcopy=yes
