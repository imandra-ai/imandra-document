{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    onix = {
      url = "github:rizo/onix/3740a2beb84bc21860385f8bbb0d5f557842de03";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      onix,
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        onix' = onix.packages.${system}.latest;
        compilerVersion = "5.2.0";
        opamFiles = [
          ./imandra-document.opam
        ];
        onixEnv = onix'.env {
          path = ./.;
          roots = opamFiles;
          lock = ./onix-lock.json;
          deps = {
            "ocaml-system" = compilerVersion;
          };
        };
        onixEnvDev = onix'.env {
          path = ./.;
          roots = opamFiles;
          lock = ./onix-lock-dev.json;
          deps = {
            "ocaml-system" = compilerVersion;
            "ocaml-lsp-server" = "*";
            "utop" = "*";
          };
        };
        onixLock =
          let
            shellHook = pkgs.writeText "onix-lock-shellHook" onixEnv.lock.shellHook;
            lockPatched = pkgs.runCommand "onix-lock-shellHook-patched" { } ''
              sed 's:--lock-file=/nix/store/.*/onix-lock.json:--lock-file=./onix-lock.json:' "${shellHook}" > "$out"
              chmod +x "$out"
            '';
          in
          pkgs.writeShellApplication {
            name = "onix-lock";
            runtimeInputs = [ onix' ];
            text = lockPatched;
          };
        onixLockDev =
          let
            shellHook = pkgs.writeText "onix-lock-dev-shellHook" onixEnvDev.lock.shellHook;
            lockPatched = pkgs.runCommand "onix-lock-dev-shellHook-patched" { } ''
              sed 's:--lock-file=/nix/store/.*/onix-lock-dev.json:--lock-file=./onix-lock-dev.json:' "${shellHook}" > "$out"
              chmod +x "$out"
            '';
          in
          pkgs.writeShellApplication {
            name = "onix-lock-dev";
            runtimeInputs = [ onix' ];
            text = lockPatched;
          };
      in
      rec {
        formatter = pkgs.nixfmt-rfc-style;

        packages.imandra-document = onixEnv.pkgs.imandra-document;
        packages.default = onixEnv.pkgs.imandra-document;

        packages.onix-lock = onixLock;
        packages.onix-lock-dev = onixLockDev;
        devShells.onixLock = pkgs.mkShell {
          buildInputs = [
            onix'
          ];
        };

        devShells.default = onixEnvDev.shell.overrideAttrs (
          final: prev: {
            buildInputs = prev.buildInputs ++ [
              pkgs.ocamlformat_0_22_4
              pkgs.dune_3
              pkgs.ocaml
              onix'
            ];
          }
        );

        # Allows the default devshell to be cached
        packages.devShell-default = devShells.default.inputDerivation;
      }
    );
}
