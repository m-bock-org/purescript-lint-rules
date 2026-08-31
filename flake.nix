{
  description = "General-purpose lint rules for purescript-lint";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    al-dente.url = "git+ssh://git@github-al-dente/m-bock/al-dente";
  };

  outputs = { self, nixpkgs, flake-utils, al-dente, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = al-dente.lib.${system};

        # A wrong hash makes nix report the right one.
        workspace = lib.mkWorkspace {
          src = ./.;
          name = "lint-purs-rules";
          gitHashes = {
            encode-decode = "sha256-urIWpgaeo0pimisUTzFzwNAIibAV31zGaHWbu2hEFuw=";
            lint-purs = "sha256-Nzo0C0vZG4ta3OZLUwNleCLATDA01F+aWj/kt89DJoc=";
          };
        };
      in
      {
        packages.default = workspace.output;

        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        packages.toolchain = pkgs.symlinkJoin {
          name = "toolchain";
          paths = [ lib.defaults.purs lib.defaults.spago lib.defaults.purs-tidy ];
        };

        devShells.default = pkgs.mkShell {
          name = "lint-purs-rules";
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
