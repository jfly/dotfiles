" Returns true if the given file belongs to your test runner
function! test#honorjs#honorrunner#test_file(file) abort
    return a:file =~ "\.test\.js"
endfunction

" Returns test runner's arguments which will run the current file and/or line
function! test#honorjs#honorrunner#build_position(type, position) abort
    return ["GREP=".expand(substitute(a:position['file'], 'assets/src', '', ''))]
endfunction

" Returns processed args (if you need to do any processing)
function! test#honorjs#honorrunner#build_args(args) abort
    let args = a:args
    return args
endfunction

" Returns the executable of your test runner
function! test#honorjs#honorrunner#executable() abort
    return "make test-js"
endfunction
