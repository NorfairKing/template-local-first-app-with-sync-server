final: previous:
with final.haskell.lib;

{
  foo-barCasts =
    let
      mkCastDerivation = import (
        builtins.fetchGit {
          url = "https://github.com/NorfairKing/autorecorder";
          rev = "da5bf9d61108a4a89addc8203b1579a364ce8c01";
          ref = "master";
        } + "/nix/cast.nix"
      ) { pkgs = final // final.foo-barPackages; };
    in
      {
        foo-bar-basics-cast = mkCastDerivation {
          name = "foo-bar-basics-cast";
          src = ../casts/basics.yaml;
        };
      };
  foo-barPackages =
    let
      foo-barPkg =
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
      foo-barPkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName (foo-barPkg name);
      foo-barPkgWithOwnComp = name: foo-barPkgWithComp name name;

    in
      {
        "foo-bar-api" = foo-barPkg "foo-bar-api";
        "foo-bar-api-gen" = foo-barPkg "foo-bar-api-gen";
        "foo-bar-api-server" = foo-barPkgWithOwnComp "foo-bar-api-server";
        "foo-bar-api-server-data" = foo-barPkg "foo-bar-api-server-data";
        "foo-bar-api-server-data-gen" = foo-barPkg "foo-bar-api-server-data-gen";
        "foo-bar-api-server-gen" = foo-barPkg "foo-bar-api-server-gen";
        "foo-bar-cli" = foo-barPkgWithComp "foo-bar" "foo-bar-cli";
        "foo-bar-client" = foo-barPkg "foo-bar-client";
        "foo-bar-client-data" = foo-barPkg "foo-bar-client-data";
        "foo-bar-data" = foo-barPkg "foo-bar-data";
        "foo-bar-data-gen" = foo-barPkg "foo-bar-data-gen";
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
                  final.foo-barPackages // {
                    envparse = envparsePkg;
                    cursor-brick = cursorBrickPkg;
                  }
            );
        }
    );
}
