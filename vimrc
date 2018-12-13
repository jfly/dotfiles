" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')

" Declare the list of plugins.
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
Plug 'xtal8/traces.vim'
Plug 'majutsushi/tagbar'
"<<< Plug 'davidhalter/jedi-vim'
Plug 'w0rp/ale'
" List ends here. Plugins become visible to Vim after this call.
call plug#end()

let mapleader = ","
" Fast reloading of .vimrc
map <leader>r :source ~/.vimrc<cr>

" Easy copying of path to file
nnoremap <Leader>cf :let @+ = expand("%")<CR>

" http://vim.wikia.com/wiki/Indenting_source_code
" http://stackoverflow.com/a/234578
filetype plugin indent on
" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab

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
" Copying from https://github.com/FatBoyXPC/dotfiles/commit/37b6ed80e20415fe181f4cacaa0f16bb37c19503
noremap <Leader>/ "ay:Ag <C-r>a<Space>
nnoremap <Leader>* :Ag<Space><C-R><C-W>
noremap ]q :cnext<CR>
noremap [q :cprevious<CR>

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

"""""" tcomment configuration
call tcomment#type#Define('bash', '#<<< %s')
call tcomment#type#Define('sh', '#<<< %s')
call tcomment#type#Define('conf', '#<<< %s')
call tcomment#type#Define('zsh', '#<<< %s')
call tcomment#type#Define('python', '#<<< %s')
call tcomment#type#Define('ruby', '#<<< %s')
call tcomment#type#Define('vim', '"<<< %s')
call tcomment#type#Define('scss', '//<<< %s')
call tcomment#type#Define('javascript', '//<<< %s')
call tcomment#type#Define('jsx', '{/*<<< %s */}')
""""""

"""""" FZF configuration
noremap <c-p> :Files<CR>
noremap <leader>b :Buffers<CR>
""""""

"""""" vim-test configuration
let g:shtuff_receiver = $SHTUFF_RUNNER
function! ShtuffStrategy(cmd)
    call system("shtuff into " . shellescape(g:shtuff_receiver) . " " . shellescape("clear;" . a:cmd))
endfunction

let g:test#custom_strategies = {'fat_runner': function('ShtuffStrategy')}
let g:test#strategy = "fat_runner"

" vim-test transformation to run nose tests via `make singletest`.
" If a command looks like "nosetests ...", transform it to
" "make singletest NOSEARGS='...'"
function! HonorTransform(cmd) abort
    if a:cmd =~ '^nosetests '
        "let a:cmd_sans_nosetests = "-s --pdb ".substitute(a:cmd, '^nosetests ', '', '')
        let a:cmd_sans_nosetests = "-s ".substitute(a:cmd, '^nosetests ', '', '')
        let a:new_cmd = 'make singletest TEST_PROCESSES=0 TEST_DB_COUNT=1 NOSEARGS='.shellescape(a:cmd_sans_nosetests)
    else
        let a:new_cmd = a:cmd
    endif
    return a:new_cmd
endfunction

" Force use of nosetest over pytest
let test#python#pytest#file_pattern = '\vMATCH_NOTHING_AT_ALL$'
let test#python#nose#file_pattern = '\v(^|[\b_\.-])[Tt]est.*\.py$'

let g:test#custom_transformations = {'honor': function('HonorTransform')}
let g:test#transformation = 'honor'

let test#custom_runners = {'HonorJs': ['HonorRunner']}
""""""

nnoremap <leader>ts :w<CR>:TestSuite<CR>
nnoremap <leader>tf :w<CR>:TestFile<CR>
nnoremap <leader>tn :w<CR>:TestNearest<CR>
nnoremap <leader>tv :w<CR>:TestVisit<CR>
nnoremap <leader>tl :w<CR>:call system("fat-rerunner")<CR>
""""""

"""""" ToggleTest between file and corresponding test file.
function! ToggleTest(path)
    if a:path =~ "_test\.py$"
        let l:other_path = substitute(expand('%'), "_test\.py$", ".py", "")
    elseif a:path =~ "\.py$"
        let l:other_path = substitute(expand('%'), "\.py$", "_test.py", "")
    elseif a:path =~ "\.test\.js$"
        let l:other_path = substitute(expand('%'), "\.test\.js$", ".js", "")
    elseif a:path =~ "\.js$"
        let l:other_path = substitute(expand('%'), "\.js$", ".test.js", "")
    else
        echo "I'm not sure how to toggle " . a:path
        return
    endif
    :execute 'edit' l:other_path
endfunction
nnoremap <leader>tt :call ToggleTest(expand('%'))<CR>
""""""

"""""" airline configuration
" Display all buffers when there's only one tab open
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
""""""

"" Turn off folding
set nofoldenable

" http://unix.stackexchange.com/a/30757
set tabpagemax=100

" https://webpack.github.io/docs/webpack-dev-server.html#working-with-editors-ides-supporting-safe-write
set backupcopy=yes

"""""""
nnoremap <leader>p oimport pdb; pdb.set_trace()#<<<<Esc>
nnoremap <leader>P Oimport pdb; pdb.set_trace()#<<<<Esc>
"""""""
