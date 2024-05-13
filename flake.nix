{
  description = "foo-bar";
  nixConfig = {
    extra-substituters = "https://foobar.cachix.org";
    extra-trusted-public-keys = "foobar.cachix.org-1:srabhQPgZR0EO+bOppsCWbesHOgk8ABakPL8D1h5wOU=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    home-manager.url = "github:nix-community/home-manager?ref=release-23.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-dependency-graph-nix.url = "github:NorfairKing/haskell-dependency-graph-nix";
    haskell-dependency-graph-nix.inputs.nixpkgs.follows = "nixpkgs";
    haskell-dependency-graph-nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    appendful.url = "github:NorfairKing/appendful";
    appendful.flake = false;
    mergeless.url = "github:NorfairKing/mergeless";
    mergeless.flake = false;
    mergeful.url = "github:NorfairKing/mergeful";
    mergeful.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , pre-commit-hooks
    , haskell-dependency-graph-nix
    , weeder-nix
    , appendful
    , mergeless
    , mergeful
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (appendful + "/nix/overlay.nix"))
          (import (mergeless + "/nix/overlay.nix"))
          (import (mergeful + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
      pkgsMusl = pkgs.pkgsMusl;
      mkNixosModule = import ./nix/nixos-module.nix {
        inherit (pkgsMusl.fooBarReleasePackages) foo-bar-api-server;
      };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.fooBarRelease;
        static = pkgsMusl.fooBarRelease;
      };
      checks.${system} = {
        release = self.packages.${system}.default;
        static = self.packages.${system}.static;
        shell = self.devShells.${system}.default;
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "foo-bar-api"
            "foo-bar-api-server"
            "foo-bar-api-server-data"
            "foo-bar-cli"
            "foo-bar-client"
            "foo-bar-client-data"
            "foo-bar-data"
          ];
          coverage = [
            "foo-bar-api-gen"
            "foo-bar-api-server-data-gen"
            "foo-bar-api-server-gen"
            "foo-bar-data-gen"
          ];
        };
        dependency-graph = haskell-dependency-graph-nix.lib.${system}.makeDependencyGraph {
          packages = builtins.attrNames pkgs.haskellPackages.fooBarPackages;
          inherit (pkgs) haskellPackages;
        };
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = builtins.attrNames pkgs.haskellPackages.fooBarPackages;
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            deadnix.enable = true;
            deadnix.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit (pkgs) nixosTest;
          home-manager = home-manager.nixosModules.home-manager;
          foo-bar-nixos-module-factory = self.nixosModuleFactories.${system}.default;
          foo-bar-home-manager-module = self.homeManagerModules.${system}.default;
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "foo-bar-shell";
        packages = p: builtins.attrValues p.fooBarPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          cabal-install
          pkgs.haskellPackages.hspec-discover
          zlib
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
            deadnix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nixosModules.${system}.default = mkNixosModule { envname = "production"; };
      nixosModuleFactories.${system}.default = mkNixosModule;
      homeManagerModules.${system}.default = import ./nix/home-manager-module.nix { inherit (pkgsMusl.fooBarReleasePackages) foo-bar-cli; };
    };
}
