# Little snippet from Niv to get our pinned dependencies
{ sources ? import ./nix/sources.nix,
  compiler ? "ghc882"
}:
with { overlay = _: pkgs: { niv = import sources.niv {}; }; };

let
    #gitignoreSource = (import sources.gitignore { lib = pkgs.lib; }).gitignoreSource;

    config = {
        packageOverrides = pkgs: rec {
            haskell = pkgs.haskell // {
                packages = pkgs.haskell.packages // {
                    "${compiler}" = pkgs.haskell.packages."${compiler}".override {
                        overrides = haskellPackagesNew: haskellPackagesOld: rec {
                            vimscript-haskell =
                              haskellPackagesNew.callCabal2nix "vimscript-haskell" (./.) {};
                        };
                    };
                };
            };
        };
    };

    pkgs = import sources.nixpkgs { inherit config; };

in
    pkgs.haskell.packages.${compiler}.vimscript-haskell
