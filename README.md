A simple IRC bot based on the [`irc-client`](https://hackage.haskell.org/package/irc-client) library.

## Features

- [URL titles](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/URL.hs).
- [Evaluators](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Eval.hs) for a bunch of languages, running in a VM. Uses [qeval](https://github.com/tilpner/qeval). Syntax: `haskell> 42`.
- [Wiktionary](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Wiktionary.hs): random pages from [English Wiktionary](https://en.wiktionary.org). Syntax: `.wrandom [CATEGORY|-l LANGUAGE]`.

## Usage

With a [flakes-enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes) Nix:

```console
$ nix run github:ncfavier/bothendieck -- -c path/to/config.toml
```

See [`config.sample.toml`](https://github.com/ncfavier/bothendieck/blob/main/config.sample.toml) for inspiration.
