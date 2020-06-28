nix-shell --pure --command \
    "ls src/*.hs test/*.hs | entr  -dsc 'cabal build vimscript-test && cabal run vimscript-test'"
