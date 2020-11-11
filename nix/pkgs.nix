let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay =
    import (
      pkgs.fetchFromGitHub (import ./validity-version.nix) + "/nix/overlay.nix"
    );
  appendful-overlay =
    import (
      pkgs.fetchFromGitHub (import ./appendful-version.nix) + "/nix/overlay.nix"
    );
  mergeless-overlay =
    import (
      pkgs.fetchFromGitHub (import ./mergeless-version.nix) + "/nix/overlay.nix"
    );
  mergeful-overlay =
    import (
      pkgs.fetchFromGitHub (import ./mergeful-version.nix) + "/nix/overlay.nix"
    );
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  hastoryPkgs =
    pkgsv {
      overlays =
        [
          validity-overlay
          appendful-overlay
          mergeless-overlay
          mergeful-overlay
          yamlparse-applicative-overlay
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
hastoryPkgs
