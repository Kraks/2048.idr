#2048 in Idris

This project aims to learn dependent type programming in Idris. 

I implemented two version of game 2048:

* `2048.idr` shows how to just using traditional type system to achieve in Idris.

* `2048-dependent.idr` is the version using dependent type. The most important data structure is dependent list `Vect`, and it also utilize built-in proof to insure some pre-condition and post-condition of function behaviors.

### Build

You should have installed Idris (version 0.11 or later), see [Idris Documentation](http://www.idris-lang.org/download/).
* run `make all` to compile both two versions.

* run `make 2048` to compile ordinary version

* `make 2048-dep` to compile the dependent type version.

### How to play

* `w` and press enter for move up,
* `a` for move left, 
* `s` for move down,
* `d` for move right,
* and `q` to quit the game.

Have fun!



