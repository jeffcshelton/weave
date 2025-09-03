{
  description = "Weave compiler developer shell.";

  inputs = {
    crane.url = "github:ipetkov/crane";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
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

      nativeBuildInputs = with pkgs; [
        llvm_18
        rust
      ];

      src = craneLib.cleanCargoSource ./.;

      cargoArtifacts = craneLib.buildDepsOnly {
        inherit nativeBuildInputs src;
        strictDeps = true;
      };

      weave = craneLib.buildPackage {
        inherit nativeBuildInputs cargoArtifacts src;
        strictDeps = true;
      };
    in
    {
      apps.default = {
        type = "app";
        program = "${weave}/bin/weave";
      };

      devShells.default = pkgs.mkShell {
        inherit nativeBuildInputs;
        name = "weave";
        version = "1.0.0";
      };

      packages.default = weave;
    }
  );
}
