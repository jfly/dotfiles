" Plugins will be downloaded under the specified directory.
call plug#begin('~/.vim/plugged')

" Declare the list of plugins.
Plug 'junegunn/fzf.vim'
Plug 'Valloric/MatchTagAlways'
Plug 'vim-airline/vim-airline'
Plug 'vim-scripts/matchit.zip'
Plug 'tomtom/tcomment_vim'
Plug 'altercation/vim-colors-solarized'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'sheerun/vim-polyglot'
Plug 'editorconfig/editorconfig-vim'
Plug 'janko-m/vim-test'
Plug 'kassio/neoterm'
Plug 'xtal8/traces.vim'
Plug 'majutsushi/tagbar'
Plug 'dense-analysis/ale'
Plug 'jesseleite/vim-agriculture'
" TODO: figure out how to get only one of coc/ale to run pylint... >>>
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'metakirby5/codi.vim'
Plug 'samoshkin/vim-mergetool'
" List ends here. Plugins become visible to Vim after this call.
call plug#end()

" https://github.com/FatBoyXPC/dotfiles/commit/2f2827d822fe80b9b8d7137e5f193fa0be390255
command! -bang -nargs=+ -complete=dir Ag call fzf#vim#ag_raw(agriculture#smart_quote_input(<q-args>), <bang>0)

let mapleader = " "
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

" vim-gutter tweak
" Copied from
" https://github.com/FatBoyXPC/dotfiles/commit/53815a4009884dcf04b3f009de9d1bf458bed32f
highlight! link SignColumn LineNr

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

"""""" filetypes {{{
au BufRead,BufNewFile *.md set filetype=markdown
au BufRead,BufNewFile Vagrantfile set filetype=ruby
au BufRead,BufNewFile *.jy set filetype=python
"""""" }}}

"""""" mergetool configuration {{{
let g:mergetool_layout = 'mr'
let g:mergetool_prefer_revision = 'local'
" Turn off syntax and spell checking highlighting for all splits, so it doesn't distract me from diff highlighting.
function s:on_mergetool_set_layout(split)
    set syntax=off
    set nospell
endfunction
let g:MergetoolSetLayoutCallback = function('s:on_mergetool_set_layout')
nmap <leader>mt <plug>(MergetoolToggle)
"""""" }}}

"""""" tcomment configuration {{{
call tcomment#type#Define('bash', '#<<< %s')
call tcomment#type#Define('sh', '#<<< %s')
call tcomment#type#Define('conf', '#<<< %s')
call tcomment#type#Define('zsh', '#<<< %s')
call tcomment#type#Define('cpp', '//<<< %s')
call tcomment#type#Define('python', '# <<< %s')
call tcomment#type#Define('ruby', '#<<< %s')
call tcomment#type#Define('vim', '"<<< %s')
call tcomment#type#Define('scss', '//<<< %s')
call tcomment#type#Define('javascript', '//<<< %s')
call tcomment#type#Define('typescript', '//<<< %s')
call tcomment#type#Define('typescriptreact', '//<<< %s')
call tcomment#type#Define('jsx', '{/*<<< %s */}')
call tcomment#type#Define('make', '#<<< %s')
call tcomment#type#Define('dockerfile', '#<<< %s')
call tcomment#type#Define('yaml', '#<<< %s')
"""""" }}}

"""""" FZF configuration {{{
noremap <c-p> :Files<CR>
noremap <leader>b :Buffers<CR>
noremap <leader><leader> <C-^>
let g:fzf_layout = { 'down': '~40%' }
let g:fzf_preview_window = ''
"""""" }}}

"""""" vim-test configuration
function! ShtuffStrategy(cmd)
    call system("shtuff into " . shellescape(getcwd()) . " " . shellescape("clear;" . a:cmd))
endfunction

let g:test#custom_strategies = {'fat_runner': function('ShtuffStrategy')}
let g:test#strategy = "fat_runner"

" vim-test transformation to run nose tests via `make singletest`.
" If a command looks like "nosetests ...", transform it to
" "make singletest NOSEARGS='...'"
function! HonorTransform(cmd) abort
    if a:cmd =~ '.py:'
        " We're probably running a single single test, and therefore we set
        " the amount of parallelism to 1, so pdb and other stuff works well.
        " There might be a better way of detecting if we're inside of
        " `TestFile`...
        let l:parallelism="PARALLELISM=1"
    else
        let l:parallelism=""
    endif
    if a:cmd =~ '^nosetests '
        let l:cmd_sans_nosetests = "-s ".substitute(a:cmd, '^nosetests ', '', '')
        let l:new_cmd = 'make singletest '.l:parallelism.' NOSEARGS='.shellescape(l:cmd_sans_nosetests)
    elseif a:cmd =~ '^pipenv run pytest '
        let l:cmd_sans_nosetests = "-s ".substitute(a:cmd, '^pipenv run pytest ', '', '')
        let l:new_cmd = 'make singletest '.l:parallelism.' PYTESTARGS='.shellescape(l:cmd_sans_nosetests)
    else
        let l:new_cmd = a:cmd
    endif
    return l:new_cmd
endfunction

if executable('nosetests')
    let test#python#runner = 'nose'
else
    let test#python#runner = 'pytest'
endif

let g:test#custom_transformations = {'honor': function('HonorTransform')}
let g:test#transformation = 'honor'

let test#custom_runners = {'HonorJs': ['HonorRunner']}
""""""

nnoremap <leader>ts :w<CR>:TestSuite<CR>
nnoremap <leader>tf :w<CR>:TestFile<CR>
nnoremap <leader>tn :w<CR>:TestNearest<CR>
nnoremap <leader>tv :w<CR>:TestVisit<CR>
nnoremap <Leader>tl :w<CR>:call system("shtuff into " . shellescape(getcwd()) . " \x1BOA")<CR>
""""""

"""""" ToggleTest between file and corresponding test file.
function! Mapped(fn, l)
    """" Copied from https://learnvimscriptthehardway.stevelosh.com/chapters/39.html
    let new_list = deepcopy(a:l)
    call map(new_list, string(a:fn) . '(v:val)')
    return new_list
endfunction
function! Reversed(l)
    """" Copied from https://learnvimscriptthehardway.stevelosh.com/chapters/39.html
    let new_list = deepcopy(a:l)
    call reverse(new_list)
    return new_list
endfunction

function! GetToggleFile(path)
    """" This is a two way mapping of "normal" suffixes to/from test suffixes.
    """" If the given path has and of these suffixes, we'll search for a file
    """" ending with the opposite type of suffix. This should let you easily
    """" toggle to/from test files.
    let l:mappings = [
        \[[".py"], ["_test.py"]],
        \[[".js", ".jsx", ".ts", ".tsx"], [".test.js", ".test.jsx", ".test.ts", ".test.tsx"]],
    \]
    let l:reverse_mappings = Mapped(function("Reversed"), l:mappings)
    let l:unknown_suffix = v:true
    for [l:suffixes, l:other_suffixes] in l:mappings + l:reverse_mappings
        for l:suffix in l:suffixes
            let l:basename = a:path[0:-(len(l:suffix)+1)]
            let l:potential_suffix = a:path[-len(l:suffix):-1]

            """" Does the end of the path match this suffix we're looking at?
            if l:potential_suffix ==? l:suffix
                let l:unknown_suffix = v:false
                """" It does match! Let's try appending every possible
                """" other_suffix and if the file exists, that's the winner.
                for l:other_suffix in l:other_suffixes
                    let l:other_path = l:basename . l:other_suffix
                    if filereadable(l:other_path)
                        return l:other_path
                    endif
                endfor
            endif
        endfor
    endfor

    """" Uh oh, we don't recognize this suffix. Return empty string as an
    """" indicator that we don't know what to do with this path. Does
    """" vimscript support throwing and catching errors...?
    if l:unknown_suffix
        return ""
    endif

    """" This file doesn't exist, but maybe the user wants to write a new
    """" test! Help them out by opening up a new buffer with an appropriate
    """" filename.
    return l:other_path
endfunction

function! ToggleTest(path)
    let l:other_path = GetToggleFile(a:path)
    if len(l:other_path) == 0
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
let g:airline_powerline_fonts = 1
""""""

"" Turn off folding
set nofoldenable

" http://unix.stackexchange.com/a/30757
set tabpagemax=100

" https://webpack.github.io/docs/webpack-dev-server.html#working-with-editors-ides-supporting-safe-write
set backupcopy=yes

""""""" Enable the Prettier fixer
let g:ale_fixers = {
\   'javascript': ['prettier'],
\   'typescript': ['prettier'],
\   'typescriptreact': ['prettier'],
\   'css': ['prettier'],
\   'python': ['black', 'isort'],
\}
" Automatically run ALEFix on save.
let g:ale_fix_on_save = 1
let g:ale_haskell_ghc_options = '-fno-code -v0 -dynamic'
" I haven't fully investigated this, but without this setting, black fails to
" import our generated thrift files.
let g:ale_python_black_change_directory = 0
"""""""

""""""" Easy debugging.
autocmd FileType python nnoremap <leader>d o__import__('pdb').set_trace()#<<<<Esc>
autocmd FileType python nnoremap <leader>D O__import__('pdb').set_trace()#<<<<Esc>
autocmd FileType sh nnoremap <leader>d oecho -n "paused..." && read -r #<<<<Esc>
autocmd FileType sh nnoremap <leader>D oecho -n "paused..." && read -r #<<<<Esc>

autocmd FileType python nnoremap <leader>o o__import__('os').environ['JFLY'] = '1'#<<<<Esc>
autocmd FileType python nnoremap <leader>l oif __import__('os').environ.get('JFLY'): __import__('pdb').set_trace()#<<<<Esc>
"""""""

""""""" Configure coc
"<<< let g:coc_global_extensions = ['coc-tsserver']
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
nmap <silent> <leader>g <Plug>(coc-definition)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-references)
"""""""

"""""""
" Copied from https://github.com/FatBoyXPC/dotfiles/commit/1bae9190f0d4291d08850bf5483986f185bc26fd
command! Mapsn call fzf#vim#maps('n', 0)
command! Mapsx call fzf#vim#maps('x', 0)
command! Mapso call fzf#vim#maps('o', 0)
command! Mapsi call fzf#vim#maps('i', 0)
command! Mapsv call fzf#vim#maps('v', 0)
command! Mapsa call fzf#vim#maps('a', 0)
"""""""

""""""" Misc shortcuts
nnoremap <leader>a ggVG
"""""""

""""""" Make it easier to interface with the system clipboard.
noremap <leader>y "+y
noremap <leader>p "+p
noremap <leader>P "+P
"""""""
