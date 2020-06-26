# Little snippet from Niv to get our pinned dependencies
{ sources ? import ./nix/sources.nix }:
with { overlay = _: pkgs: { niv = import sources.niv {}; }; };
let
    pkgs = import sources.nixpkgs {};

    gitignoreSource = (import sources.gitignore { lib = pkgs.lib; }).gitignoreSource;

    vimscript-haskell = pkgs.haskellPackages.callCabal2nix "vimscript-haskell" (gitignoreSource ./.) {};
in
    vimscript-haskell
