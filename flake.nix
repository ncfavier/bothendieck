{
  description = "An IRC bot";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    qeval.url = "github:ncfavier/qeval";
    qeval.inputs.nixpkgs.follows = "nixpkgs";
    qeval.inputs.flake-utils.follows = "flake-utils";

    html-charset.url = "github:ncfavier/html-charset/maintenance";
    html-charset.flake = false;
  };

  outputs = { self, flake-utils, nixpkgs, html-charset, qeval }: let
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
          inherit html-charset;
        };
      };
    };
  in flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ overlay ];
    };
    evaluators = qeval.legacyPackages.${system}.evaluators.all;
    bothendieck = pkgs.callPackage ({ runCommand, haskellPackages, makeWrapper, evaluators }:
      runCommand "bothendieck" { nativeBuildInputs = [ makeWrapper ]; } ''
        makeWrapper ${haskellPackages.bothendieck}/bin/bothendieck "$out/bin/bothendieck" \
          --set EVALUATORS ${evaluators}
      ''
    ) { inherit evaluators; };
  in {
    packages = {
      inherit bothendieck evaluators;
      default = bothendieck;
    };
    devShells.default = pkgs.haskellPackages.shellFor {
      packages = ps: [ ps.bothendieck ];
      EVALUATORS = evaluators;
    };
  }) // {
    overlays.default = overlay;
  };
}
