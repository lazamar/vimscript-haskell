" Nested functions
function! SayHi ()
    function! SayIt ()
        echo "hi"
    endfunction

    call SayIt()
endfunction

" Recursive
function! Fib(n)
    call SayHi()
    if (a:n < 2)
        return 1
    else
        let l:v = Fib(a:n - 1)
        let l:w = Fib(a:n - 2)
        return  l:w + l:v
endfunction

" Closure
function! SayHiTo (name)
    function! WithGreeting (greeting) closure
        function! Unnecessary (deep) closure
            echo a:deep . " " . a:name . ", ". a:greeting
        endfunction

        call Unnecessary("Hi")
    endfunction

    call WithGreeting ("good morning")
endfunction

echo SayHiTo("my little friend")
