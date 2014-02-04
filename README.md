hs-snakelike
============

A game that's kind of like snake.

## Releases

There's a single build, untested anywhere but on my OSX Mavericks: [http://releases.abesto.net/hs-snakelike/hs-snakelike-osx-mavericks-20140204](http://releases.abesto.net/hs-snakelike/hs-snakelike-osx-mavericks-20140204)

## Building

```sh
$ cabal install --only-dependencies
# Install SDL, SDL-gfx and SDL-ttf friends using the package-manager of your choice. On OSX:
$ brew install sdl_gfx sdl_ttf
$ make
$ ./snake  # To run
```
