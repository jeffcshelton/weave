{
  description = "Weave compiler developer shell.";

  inputs = {
    crane.url = "github:ipetkov/crane";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { crane, flake-utils, nixpkgs, rust-overlay, ... }:
  let
    overlays = [ (import rust-overlay) ];
  in
  flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit overlays system; };
      rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
      craneLib = (crane.mkLib pkgs).overrideToolchain (_: rust);

      buildInputs = with pkgs; [
        llvmPackages_18.libllvm
      ];

      src = craneLib.cleanCargoSource ./.;

      cargoArtifacts = craneLib.buildDepsOnly {
        inherit buildInputs src;
        strictDeps = true;
      };

      weave = craneLib.buildPackage {
        inherit buildInputs cargoArtifacts src;
        strictDeps = true;
      };
    in
    {
      apps.default = {
        type = "app";
        program = "${weave}/bin/weave";
      };

      devShells.default = pkgs.mkShell {
        inherit buildInputs;

        # environment = {
        #   LLVM_SYS_181_PREFIX = 
        # };

        nativeBuildInputs = [ rust ];
        name = "weave";
        version = "1.0.0";

        shellHook = ''
          echo ${pkgs.llvmPackages_18.libllvm}
        '';
      };

      packages.default = weave;
    }
  );
}
