{
  description = "Weave compiler developer shell.";

  inputs = {
    flake-utils = {
      inputs.systems.follows = "systems";
      url = "github:numtide/flake-utils";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = { flake-utils, nixpkgs, ... }:
  flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        config.allowUnfree = true;
        system = "${system}";
      };

      overrides = (builtins.fromTOML (builtins.readFile ./rust-toolchain.toml));

      libraries = [];
      programs = with pkgs; [
        rustup
      ];

      packages = libraries ++ programs;
    in
    {
      devShells.default = pkgs.mkShell {
        buildInputs = packages;
        name = "weave";
        version = "1.0.0";

        shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath libraries}:$LD_LIBRARY_PATH"
          export RUSTC_VERSION="${overrides.toolchain.channel}"
        '';
      };
    }
  );
}
