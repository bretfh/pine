{
  description = "pine: a lisp machine you cannot crash and can distribute";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      each = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      # What pine needs that is not a lisp system. The lisp side comes from
      # ocicl, because several of pine's systems are not in nixpkgs and ocicl
      # resolves them: inside this shell, `make FOREIGN=1 test'.
      #
      # Guix is what pine develops against; this is the other way in, and the
      # one a mac has. Wawona, which is how a mac gets a wayland compositor at
      # all, is itself a flake, so nix is already there.
      devShells = each (pkgs:
        let
          # dlopened by name at runtime through cffi, so the store paths have
          # to be on the loader's path
          libs = with pkgs; [ tree-sitter cairo sqlite libffi libxkbcommon ];
          libPath = pkgs.lib.makeLibraryPath libs;
        in {
          default = pkgs.mkShell {
            packages = with pkgs; [
              sbcl
              pkg-config
              graphviz       # make docs
              tree-sitter    # the generator as well as the library
              nodejs         # what tree-sitter generate runs the grammar with
            ] ++ libs
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux (with pkgs; [
              grim wtype     # the wm harness, bench/wm-shot.sh
            ]);

            # a mac's loader reads DYLD_, everything else LD_; setting both
            # costs nothing and keeps this one line rather than a conditional
            shellHook = ''
              export LD_LIBRARY_PATH="${libPath}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              export DYLD_LIBRARY_PATH="${libPath}''${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
              echo "pine: the lisp systems come from ocicl, the rest is here."
              echo
              echo "  make foreign                 what is still missing"
              echo "  make FOREIGN=1 foreign-deps  ocicl install"
              echo "  make FOREIGN=1 foreign-libs  the pty and the two grammars"
              echo "  make FOREIGN=1 test"
              echo
            '';
          };
        });
    };
}
