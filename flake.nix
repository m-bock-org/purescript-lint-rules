{
  description = "General-purpose lint rules for purescript-lint";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    al-dente.url = "git+ssh://git@github.com/m-bock-org/al-dente";
  };

  outputs = { self, nixpkgs, flake-utils, al-dente, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = al-dente.lib.${system};

        # A wrong hash makes nix report the right one.
        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        toolchain = pkgs.symlinkJoin {
          name = "toolchain";
          paths = [ lib.defaults.purs lib.defaults.spago lib.defaults.purs-tidy ];
        };

        workspace = lib.mkWorkspace {
          src = ./.;
          name = "lint-purs-rules";
        };
      in
      {
        packages.default = workspace.output;

        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        packages.toolchain = toolchain;
        # The compiled test closure - every dependency plus the local
        # packages built with their tests. `just output` copies this so a
        # dev shell never recompiles what al-dente already built once per
        # machine, which is the whole point of building with al-dente.
        packages.testOutput = workspace.testOutput;

        devShells.default = pkgs.mkShell {
          name = "lint-purs-rules";

          # A marker that you are inside the dev shell. `nix develop` used
          # to do this itself and stopped, and the difference matters:
          # outside it, `purs` is whatever is installed globally.
          shellHook = ''
            case $- in *i*) export PS1="(lint-rules) $PS1" ;; esac

            # Point the editor at this exact toolchain, refreshed on every
            # entry so it cannot go stale against the flake. The .vscode
            # wrappers read this symlink and then need no nix at all.
            ln -sfn ${toolchain} .vscode/.toolchain
          '';

          packages = [
            lib.defaults.purs
            lib.defaults.spago
            lib.defaults.purs-tidy
            lib.defaults.nodejs
            pkgs.just
            pkgs.git
          ];
        };
      });
}
