final: prev:
with final.lib;
with final.haskell.lib;
{
  fooBarRelease =
    final.symlinkJoin {
      name = "foo-bar-release";
      paths = final.lib.attrValues final.haskellPackages.fooBarPackages;
    };
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
      (
        self: super:
          let
            fooBarPkg = name:
              buildFromSdist (overrideCabal (self.callPackage (../${name}/default.nix) { }) (old: {
                configureFlags = (old.configureFlags or [ ]) ++ [
                  # Optimisations
                  "--ghc-options=-O2"
                  # Extra warnings
                  "--ghc-options=-Wall"
                  "--ghc-options=-Wincomplete-uni-patterns"
                  "--ghc-options=-Wincomplete-record-updates"
                  "--ghc-options=-Wpartial-fields"
                  "--ghc-options=-Widentities"
                  "--ghc-options=-Wredundant-constraints"
                  "--ghc-options=-Wcpp-undef"
                  "--ghc-options=-Werror"
                ];
                doBenchmark = true;
                doHaddock = false;
                doCoverage = false;
                doHoogle = false;
                doCheck = false; # Only for coverage
                hyperlinkSource = false;
                enableLibraryProfiling = false;
                enableExecutableProfiling = false;
                buildDepends = (old.buildDepends or [ ]) ++ (with final; [
                  haskellPackages.autoexporter
                ]);
                # Ugly hack because we can't just add flags to the 'test' invocation.
                # Show test output as we go, instead of all at once afterwards.
                testTarget = (old.testTarget or "") + " --show-details=direct";
              }));
            fooBarPkgWithComp =
              exeName: name:
              self.generateOptparseApplicativeCompletions [ exeName ] (fooBarPkg name);
            fooBarPkgWithOwnComp = name: fooBarPkgWithComp name name;

            fooBarPackages = {
              foo-bar-api = fooBarPkg "foo-bar-api";
              foo-bar-api-gen = fooBarPkg "foo-bar-api-gen";
              foo-bar-api-server = fooBarPkg "foo-bar-api-server";
              foo-bar-api-server-data = fooBarPkg "foo-bar-api-server-data";
              foo-bar-api-server-data-gen = fooBarPkg "foo-bar-api-server-data-gen";
              foo-bar-api-server-gen = fooBarPkg "foo-bar-api-server-gen";
              foo-bar-cli = fooBarPkg "foo-bar-cli";
              foo-bar-client = fooBarPkg "foo-bar-client";
              foo-bar-client-data = fooBarPkg "foo-bar-client-data";
              foo-bar-data = fooBarPkg "foo-bar-data";
              foo-bar-data-gen = fooBarPkg "foo-bar-data-gen";
            };

            servantPkg = name: subdir: self.callCabal2nix name
              ((builtins.fetchGit {
                url = "https://github.com/haskell-servant/servant";
                rev = "552da96ff9a6d81a8553c6429843178d78356054";
              }) + "/${subdir}")
              { };
            servantPackages = {
              "servant" = servantPkg "servant" "servant";
              "servant-client" = servantPkg "servant-client" "servant-client";
              "servant-client-core" = servantPkg "servant-client-core" "servant-client-core";
              "servant-server" = servantPkg "servant-server" "servant-server";
              "servant-auth" = servantPkg "servant-auth-client" "servant-auth/servant-auth";
              "servant-auth-client" = servantPkg "servant-auth-client" "servant-auth/servant-auth-client";
              "servant-auth-server" = servantPkg "servant-auth-server" "servant-auth/servant-auth-server";
            };
          in
          {
            inherit fooBarPackages;
          } // fooBarPackages // servantPackages
      );
  });
}
