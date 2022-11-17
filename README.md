A simple IRC bot based on the [`irc-client`](https://hackage.haskell.org/package/irc-client) library.

## Features

- [URL titles](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/URL.hs).
- [Evaluators](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Eval.hs) for a bunch of languages, running in a VM. Uses [qeval](https://github.com/tilpner/qeval). Syntax: `haskell> 42`. Use `↵` (compose + return) to insert newlines.
- [Wikimedia](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Wikimedia.hs):
  - `.wp ARTICLE` prints a summary of the given article on Wikipedia.
  - `.wprandom [CATEGORY]` prints a link to a random Wikipedia article.
  - `.wtrandom [CATEGORY|-l LANGUAGE]` prints a link to a random Wiktionary entry.
- [Merriam-Webster](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/MerriamWebster.hs) dictionary:
  - `.define [-e ENTRY] WORD`
  - `.etymology [-e ENTRY] WORD`
- [Translate](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Translate.hs): uses [translate-shell](https://github.com/soimort/translate-shell) (backed by Google Translate). `.tr[ans[late]] [[source]:[target]] text`
- [Surreal compliments](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Compliment.hs) extracted from http://www.madsci.org/cgi-bin/lynn/jardin/SCG. `.compliment [nick]`

## Usage

With a [flakes-enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes) Nix:

```console
$ nix run github:ncfavier/bothendieck -- -c path/to/config.toml
```

See [`config.sample.toml`](https://github.com/ncfavier/bothendieck/blob/main/config.sample.toml) for inspiration.

You can specify additional configuration files with the `-C` option. This is useful for separating secrets from the main configuration.
