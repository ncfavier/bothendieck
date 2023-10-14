A simple IRC bot based on the [`irc-client`](https://hackage.haskell.org/package/irc-client) library.

## Features

Each module in [`src/Parts`](https://github.com/ncfavier/bothendieck/tree/main/src/Parts) defines a separate set of features: commands and message handlers. In the following, commands start with a `.`, but this prefix can be [configured](#usage).

- [`URL`](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/URL.hs) fetches and posts URL titles.
- [`Eval`](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Eval.hs) provides evaluators for a bunch of languages using [qeval](https://github.com/ncfavier/qeval). Syntax: `LANG> EXPRESSION`. Use `↵` (compose + return) to insert newlines.
- [`Wikimedia`](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Wikimedia.hs) queries information from Wikimedia projects.
  - `.wp ARTICLE` prints a summary of the given article on [Wikipedia](https://en.wikipedia.org), the free encyclopedia.
  - `.wprandom [CATEGORY]` prints a link to a random Wikipedia article.
  - `.wtrandom [CATEGORY|-l LANGUAGE]` prints a link to a random [Wiktionary](https://en.wiktionary.org) entry.
- [`NLab`](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/NLab.hs) queries information from the [nLab](https://ncatlab.org), a wiki for higher mathematics.
  - `.nlab SEARCH` searches for a page and prints a link to the first result.
  - `.nlabrandom [CATEGORY]` prints a random page.
- [`MerriamWebster`](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/MerriamWebster.hs) queries information from the [Merriam-Webster](https://www.merriam-webster.com) dictionary.
  - `.d[ef[ine]] [-e ENTRY] WORD` queries a word's definition.
  - `.etym[ology] [-e ENTRY] WORD` queries a word's etymology.
- [`Translate`](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Translate.hs) provides translations via [Google Translate](https://translate.google.com/).
  - `.tr[ans[late]] [[SOURCE]:[TARGET]] TEXT` translates `TEXT` from the `SOURCE` to the `TARGET` language, specified as [ISO 3166](https://en.wikipedia.org/wiki/ISO_3166) codes. The default source language is auto-detected, and the default target language is English.
- [`WolframAlpha`](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/WolframAlpha.hs) queries [WolframAlpha](https://www.wolframalpha.com) for answers.
  - `.(wa|wolfram[alpha]) QUERY` asks a general question.
  - `.time PLACE` asks for the current time in `PLACE`.
  - `.weather PLACE` asks for the current weather in `PLACE`.
- [`Compliment`](https://github.com/ncfavier/bothendieck/blob/main/src/Parts/Compliment.hs) provides surrealist compliments extracted from the [surrealist compliment generator](http://www.madsci.org/cgi-bin/lynn/jardin/SCG).
  - `.compliment [WHO]`

## Usage

With a [flakes-enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes) Nix:

```console
$ nix run github:ncfavier/bothendieck -- -c path/to/config.toml
```

See [`config.sample.toml`](https://github.com/ncfavier/bothendieck/blob/main/config.sample.toml) for inspiration.

You can specify additional configuration files with the `-C` option. This is useful for separating secrets from the main configuration.
