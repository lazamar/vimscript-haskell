{ sources ? import ./nix/sources.nix,
  compiler ? "ghc882"
}:

let
    pkgs = import sources.nixpkgs {};
    vimscript-haskell = import ./default.nix {};

    # Commands
    commands = {
      test-watch = pkgs.writeShellScriptBin "test-watch" ''
        find src test -name '*.hs' | entr -dsc 'cabal build && cabal run vimscript-test'
        sleep 1
        '';

      run-watch = pkgs.writeShellScriptBin "run-watch" ''
        find src -name '*.hs' | entr -dsc 'cabal build && cabal run vimscript-haskell-exe'
        sleep 1
        '';

      run = pkgs.writeShellScriptBin "run" ''
        cabal build && cabal run vimscript-haskell-exe
        '';
    };
in
pkgs.stdenv.mkDerivation {
    name = "vimscript-shell";
    # withHoogle = true;
    buildInputs =
      vimscript-haskell.env.nativeBuildInputs ++   # Get correct GHC
      [
        pkgs.haskellPackages.cabal-install         # Make sure we are using cabal version 3
        pkgs.haskellPackages.ghcid
        pkgs.haskell.packages."${compiler}".ghcide # We need ghcide compiled with the correct compiler
        pkgs.entr
        pkgs.vim
        commands.test-watch
        commands.run-watch
        commands.run
      ];
}
