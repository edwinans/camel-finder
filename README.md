# Camel Finder

A camel-style variant of the [Minesweeper](https://en.wikipedia.org/wiki/Minesweeper_(video_game)) game implemented in OCaml and compiled to JavaScript ([js_of_ocaml](https://github.com/ocsigen/js_of_ocaml)), in a functional paradigm ([Elm](https://guide.elm-lang.org/architecture/)) using [ocaml-vdom](https://github.com/LexiFi/ocaml-vdom). Running on a web browser.

Play on the [demo page](https://edwinans.github.io/camel-finder/):
- Left-click to expand a cell
- Right-click to find (flag) camels

## Install dependencies
```
opam install . --deps-only
```

## Build
```
dune build
```

## Easy local run
```
./run.sh
```
