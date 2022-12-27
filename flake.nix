{
  description = "An IRC bot";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    qeval.url = "github:ncfavier/qeval";
    qeval.inputs.nixpkgs.follows = "nixpkgs";
    qeval.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = inputs@{ self, flake-utils, nixpkgs, qeval }: let
    src = builtins.path {
      name = "bothendieck-source";
      path = self;
      filter = path: type: let
        name = baseNameOf path;
      in name != "flake.nix" && name != "flake.lock";
    };
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = final.haskell.lib.packageSourceOverrides {
          bothendieck = src;
        };
      };
    };
  in flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ overlay ];
    };
    qeval = inputs.qeval.legacyPackages.${system}.override {
      dumbTerminal = true;
    };
    bothendieck = pkgs.callPackage ({ lib, runCommand, haskellPackages, makeWrapper, qeval, translate-shell }:
      runCommand "bothendieck" { nativeBuildInputs = [ makeWrapper ]; } ''
        mkdir -p "$out/bin"
        makeWrapper ${haskellPackages.bothendieck}/bin/bothendieck "$out/bin/bothendieck" \
          --set EVALUATORS ${qeval.all} \
          --prefix PATH : ${lib.makeBinPath [ translate-shell ]}
      ''
    ) { inherit qeval; };
  in {
    packages = {
      inherit bothendieck;
      default = bothendieck;
      evaluators = qeval.all;
    };
    devShells = rec {
      default = pkgs.haskellPackages.shellFor {
        packages = ps: [ ps.bothendieck ];
        buildInputs = with pkgs; [
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          translate-shell
        ];
      };
      withEvaluators = default.overrideAttrs (_: {
        EVALUATORS = qeval.all;
      });
    };
  }) // {
    overlays.default = overlay;
  };
}
