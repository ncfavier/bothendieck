{ lib
, runCommand
, haskellPackages
, makeWrapper
, qeval ? null
, translate-shell
, typst
, typstPkgs ? (_: [])
, yt-dlp
}:

runCommand "bothendieck" {
  nativeBuildInputs = [ makeWrapper ];
  inherit (haskellPackages.bothendieck) meta;
  passthru = haskellPackages.bothendieck.passthru // {
    inherit qeval;
  };
} ''
  mkdir -p "$out/bin"
  makeWrapper ${haskellPackages.bothendieck}/bin/bothendieck "$out/bin/bothendieck" \
    ${lib.optionalString (qeval != null) "--set EVALUATORS ${qeval.all}"} \
    --prefix PATH : ${lib.makeBinPath [ translate-shell (typst.withPackages typstPkgs) yt-dlp ]}
''
