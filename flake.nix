{
  description = "An IRC bot";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    html-charset.url = "github:ncfavier/html-charset/maintenance";
    html-charset.flake = false;
    qeval.url = "github:ncfavier/qeval";
    qeval.flake = false;
  };

  outputs = { self, flake-utils, nixpkgs, html-charset, qeval }: let
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hpkgs: hprev: with final.haskell.lib; {
          gnuidn = markUnbroken (doJailbreak hprev.gnuidn);
          html-charset = hpkgs.callCabal2nix "html-charset" html-charset {};

          bothendieck = hpkgs.callCabal2nix "bothendieck" ./. {};
        };
      };
    };
  in flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        overlay
        (_: _: { path = nixpkgs; }) # avoid needless copying
      ];
    };
    all-evaluators = (import qeval { basePkgs = pkgs; }).evaluators.all;
    bothendieck = pkgs.runCommand "bothendieck" { nativeBuildInputs = [ pkgs.makeWrapper ]; } ''
      makeWrapper ${pkgs.haskellPackages.bothendieck}/bin/bothendieck "$out/bin/bothendieck" \
        --set EVALUATORS ${all-evaluators}
    '';
  in {
    packages = {
      inherit all-evaluators bothendieck;
      default = bothendieck;
    };
  }) // {
    overlays.default = overlay;
  };
}
