# Local-first application with sync server Template

This is a template implementation of command-line local-first application with a synchronisation server to go along with it.
It features complete option parsing, like in [template-optparse](https://github.com/NorfairKing/template-optparse), a command-line tool like in [template-cli](https://github.com/NorfairKing/template-cli), an api server like in [template-api-server-with-auth-and-cli](https://github.com/NorfairKing/template-api-server-with-auth-and-cli) as well as a full synchronisation implementation.

* Haskell code for an api-server
* Haskell code for an accompanying command-line tool
* Haskell code for an implementation of synchronisation between those two.
* Database definitions for both the server and the client
* Per-route integration tests for the API server
* Per-command integration test for the CLI tool
* Option parsing & Option parsing tests for both the server and the CLI tool
* Nix build
* NixOS module for hosting the server
* Nix home manager module for using the client with automated syncing
* CI
  * Stack-based CI
  * Nix-based CI
* Pre-commit hooks
  * ormolu
  * hlint
  * nixpkgs-fmt

## License

This template is **not** free to use.
See https://template.cs-syd.eu/template/NorfairKing/template-local-first-app-with-sync-server for more information.

Copyright (c) 2020 Tom Sydney Kerckhove.

All Rights Reserved.

## Instructions

To use this template in a new project, choose the name for your project, for example `shelter`.
Then use [template-filler](https://github.com/NorfairKing/template-filler) to use the template, like this:

```
template-filler --source /path/to/this/template-cli --destination /path/to/your/shelter --find Foobar --replace Shelter
```

### Template overview

This template contains these haskell packages and notable modules:

- `foobar-api`: The API, as a `servant`-based type definition, and related data types.
  - `Foobar.API.Data`: The API data type definitions
  - `Foobar.API`: The API Type definition
- `foobar-api-gen`: The generators and tests for the API and its data types.
  - `FooBar.API.Data.Gen`: Generators for the API data types
- `foobar-api-server`: The API server that implements this API.
  - `Foobar.API.Server.OptParse`: Option parsing
  - `Foobar.API.Server.Env`: The (read-only) environment and related functions
  - `Foobar.API.Server.Handler.<CommandName>`: One module per command of the CLI.
- `foobar-api-server-gen`: The generators and tests for the API server.
  - `Foobar.API.Server.TestUtils`: Utility functions to write tests that use the API server
  - `Foobar.API.Server.Handler.<CommandName>Spec`: One module per handler containing its tests
- `foobar-client`: The client record of functions to call the API server.
  - The `Foobar.Client.foobarClient` record.
- `foobar-cli`: An example command-line tool to call the API server.
  - `Foobar.CLI.OptParse`: Option parsing
  - `Foobar.CLI.Env`: The (read-only) environment and related functions
  - `Foobar.CLI.Command.<CommandName>`: One module per command of the CLI.

TODO: Information about all the data packages

![Dependency graph](dependencies.png)

### OptParse

The option parsing for both `foobar-cli` and `foobar-api-server` is based on [the option parsing template](https://github.com/NorfairKing/template-optparse).
It is included in this template so you will not need to also buy the option parsing template.

For more information about how to use the option parsing, follow the instructions in `template-cli/src/Foobar/Cli/OptParse.hs`.

### Nix build

If you don't need a nix build, remove these files:

```
rm -rf *.nix nix .github/workflows/nix.yaml
```

In `nix/nixpkgs-version.nix`, we pin a `nixpkgs` commit.
In `nix/pkgs.nix` we define our own 'version' of the `nixpkgs` by adding our own overlays.
The project overlay is defined in `nix/overlay.nix`.

In `nix/module.nix`, we define a nixos module for hosting the sync server.
In `nix/program.nix`, we define a nix home manager module for using the project on nixos with automatic syncing.

See the instructions in `nix/overlay.nix` for more details.

### CI

CI is set up for both a stack build and a nix build.
See `.github/workflows` for more details.

The stack build should "just work".

For the nix build to work, there is a manual step that you need to go through:
First, make a cachix cache at cachix.org.
Put its name in the right places within `.github/workflows/nix.yaml`.
Then put its signing key in the 'Secrets' part of your repository on github.

### Workflow examples

#### Adding an endpoint to the API

1. Add the endpoint in `foobar-api/src/Foobar/API.hs`.
2. Add a handler module in `foobar-api-server/src/Fooba/API/Server/Handler/<RouteName>hs` with a function as follows:

   ```
   handle<RouteName> :: H ()
   ```

   Give it a type according to the endpoint type.
   If it requires authentication, add `AuthCookie` as the first argument.

3. Hook up the handler in the `foobarHandlers` record in `foobar-api-server/src/Foobar/API/Server.hs`.

   If the endpoint requires authentication, use the `protected` combinator.

4. Add tests in `foobar-api-server-gen/test/Foobar/API/Server/Handler/<RouteName>Spec.hs`

#### Adding a command to the CLI tool

1. Add the new command's option parsing in the `Foobar.CLI.OptParse` module according to the instructions within.

2. Add a `Foobar.CLI.Command.<CommandName>` module with a function as follows:

   ```
   commandName :: CommandNameSettings -> C ()
   ```

3. Add a case to the `dispatch` function in `Foobar.CLI`.
4. Add tests in `Foobar.CLI.Command.<CommandName>Spec`.

#### Adding a new table to sync

TODO
