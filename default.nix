let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/nixos-unstable";
  };
  pkgs = import nixpkgs { config = {}; overlays = []; };
  ollama = pkgs.ollama;
  haskellPkgs = pkgs.haskell.packages.ghc984; 
  stack = pkgs.haskellPackages.stack;
in
pkgs.mkShell {
  name = "ollama-haskell-dev";
  buildInputs = [
    pkgs.ollama
    stack
    haskellPkgs.ghc
    pkgs.zlib # Common dependency for Haskell projects
    pkgs.pkg-config # Useful for native dependencies
  ];

  shellHook = ''
    export STACK_YAML=stack.yaml
    export NIX_GHC="${haskellPkgs.ghc}/bin/ghc"
    export NIX_GHCPKG="${haskellPkgs.ghc}/bin/ghc-pkg"
    export NIX_GHC_LIBDIR="${haskellPkgs.ghc}/lib/ghc"
    echo "Development environment for ollama-haskell ready!"
    echo "Run 'stack build' to build the project."
  '';
}
