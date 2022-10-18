A simple IRC bot based on the [`irc-client`](https://hackage.haskell.org/package/irc-client) library.

## Features

- [URL titles](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/URL.hs).
- [Evaluators](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Eval.hs) for a bunch of languages, running in a VM. Uses [qeval](https://github.com/tilpner/qeval). Syntax: `haskell> 42`.

## Usage

With a [flakes-enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes) Nix:

```console
$ nix run github:ncfavier/bothendieck -- -c path/to/config.dhall
```

See [`config.sample.dhall`](https://github.com/ncfavier/bothendieck/blob/main/config.sample.dhall) for inspiration.
