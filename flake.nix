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
        packageOverrides = hpkgs: hprev: {
          html-charset = hpkgs.callCabal2nix "html-charset" html-charset {};
          bothendieck = hpkgs.callCabal2nix "bothendieck" src {};
        };
      };
    };
  in flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [ overlay ];
    };
    evaluators = qeval.packages.${system}.all;
    bothendieck = pkgs.runCommand "bothendieck" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
      makeWrapper ${pkgs.haskellPackages.bothendieck}/bin/bothendieck "$out/bin/bothendieck" \
        --set EVALUATORS ${evaluators}
    '';
  in {
    packages = {
      inherit bothendieck evaluators;
      default = bothendieck;
    };
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [ cabal-install pkg-config zlib libidn ];
      inputsFrom = with pkgs; [ haskellPackages.bothendieck ];
      EVALUATORS = evaluators;
    };
  }) // {
    overlays.default = overlay;
  };
}
