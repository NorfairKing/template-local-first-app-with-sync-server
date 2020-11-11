final: previous:
with final.haskell.lib;

{
  fooBarPackages =
    let
      fooBarPkg =
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
      fooBarPkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName (fooBarPkg name);
      fooBarPkgWithOwnComp = name: fooBarPkgWithComp name name;

    in
      {
        "foo-bar-api" = fooBarPkg "foo-bar-api";
        "foo-bar-api-gen" = fooBarPkg "foo-bar-api-gen";
        "foo-bar-api-server" = fooBarPkgWithOwnComp "foo-bar-api-server";
        "foo-bar-api-server-gen" = fooBarPkg "foo-bar-api-server-gen";
        "foo-bar-api-server-data" = fooBarPkg "foo-bar-api-server-data";
        "foo-bar-api-server-data-gen" = fooBarPkg "foo-bar-api-server-data-gen";
        "foo-bar-cli" = fooBarPkgWithComp "foo-bar" "foo-bar-cli";
        "foo-bar-client" = fooBarPkg "foo-bar-client";
        "foo-bar-client-data" = fooBarPkg "foo-bar-client-data";
        "foo-bar-data" = fooBarPkg "foo-bar-data";
        "foo-bar-data-gen" = fooBarPkg "foo-bar-data-gen";
      };

  fooBarRelease =
    final.symlinkJoin {
      name = "foo-bar-release";
      paths = final.lib.attrValues final.fooBarPackages;
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
                  base16Repo =
                    final.fetchFromGitHub {
                      owner = "emilypi";
                      repo = "base16";
                      rev = "f340b4a9a496320010930368e503ba6b7907f725";
                      sha256 = "sha256:1c6910h9y3nmj2277d7bif3nilgacp4qafl4g5b3r2c0295hbq7z";
                    };
                  base16Pkg = self.callCabal2nix "base16" base16Repo {};

                in
                  final.fooBarPackages // {
                    envparse = envparsePkg;
                    base16 = base16Pkg;
                  }
            );
        }
    );
}
