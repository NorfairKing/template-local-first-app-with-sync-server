final: previous:
with final.haskell.lib;

{
  foobarCasts =
    let
      mkCastDerivation = import (
        builtins.fetchGit {
          url = "https://github.com/NorfairKing/autorecorder";
          rev = "da5bf9d61108a4a89addc8203b1579a364ce8c01";
          ref = "master";
        } + "/nix/cast.nix"
      ) { pkgs = final // final.foobarPackages; };
    in
      {
        foobar-basics-cast = mkCastDerivation {
          name = "foobar-basics-cast";
          src = ../casts/basics.yaml;
        };
      };
  foobarPackages =
    let
      foobarPkg =
        name:
          doBenchmark (
            addBuildDepend (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
                )
              )
            ) (final.haskellPackages.autoexporter)
          );
      foobarPkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName (foobarPkg name);
      foobarPkgWithOwnComp = name: foobarPkgWithComp name name;

    in
      {
        "foobar-api" = foobarPkg "foobar-api";
        "foobar-api-gen" = foobarPkg "foobar-api-gen";
        "foobar-api-server" = foobarPkgWithOwnComp "foobar-api-server";
        "foobar-api-server-data" = foobarPkg "foobar-api-server-data";
        "foobar-api-server-data-gen" = foobarPkg "foobar-api-server-data-gen";
        "foobar-api-server-gen" = foobarPkg "foobar-api-server-gen";
        "foobar-cli" = foobarPkgWithComp "foobar" "foobar-cli";
        "foobar-client" = foobarPkg "foobar-client";
        "foobar-client-data" = foobarPkg "foobar-client-data";
        "foobar-data" = foobarPkg "foobar-data";
        "foobar-data-gen" = foobarPkg "foobar-data-gen";
      };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (
              old.overrides or (
                _:
                _:
                  {}
              )
            ) (
              self: super:
                let
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo {}
                    );
                  cursorBrickRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "cursor-brick";
                      rev = "a7b47b03c8c5525234aaccc0c372e49a80134b9d";
                      sha256 = "sha256:1wk2sixf1ld48j6a14zgfadg41si6rl8gwmwdlkn0cqjiw9n7f4p";
                    };
                  cursorBrickPkg = self.callCabal2nix "cursor-brick" (cursorBrickRepo + "/cursor-brick") {};

                in
                  final.foobarPackages // {
                    envparse = envparsePkg;
                    cursor-brick = cursorBrickPkg;
                  }
            );
        }
    );
}
