# misokoban

Sokoban in Miso.

## setup

- install Nix Flakes

- install Cachix

- use miso's cachix:

```sh
cachix use haskell-miso-cachix
```

## build and run (wasm)

```
nix develop .#wasm --command bash -c "make && make serve"
```

or (dev):

```
nix develop .#wasm
make build && make serve
```

## build and run (docker)

```
nix develop .#wasm --command bash -c "make"
nix-build docker.nix
docker load < result
docker run --rm -it -p 3000:3000 misokoban:latest
```

## edit with vscode

```
nix-shell
code .
```

## generate worlds, from assets + screenshots

```
nix-shell
cabal run mean-assets assets/*
cabal run gen-worlds
cp GeneratedWorlds.hs src/
```

## references

- [Wikipedia, Sokoban](https://en.wikipedia.org/wiki/Sokoban)
- [Math is fun, Sokoban](https://www.mathsisfun.com/games/sokoban.html)


