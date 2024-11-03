![dclock workflow](https://github.com/travgm/dclock/actions/workflows/ci.yml/badge.svg)
[![Release](https://img.shields.io/github/v/release/travgm/dclock.svg)](https://github.com/travgm/dclock/releases)

dclock
======

This was mostly created for my own edification. This takes a simple calculation converting system time 
to usable decimal minutes left in the day. I wanted to utilize the machines package to create a nice 
compositional monadic pipeline and learn some new concepts.

Usage
=====

You can type -v or --version to see program information or -e to print extended information which is the current date. If you want to leave it running for a realtime decimal clock use the -w option.

```
$ cabal run
Decimal time: 292
$ 
```
or if you have copied the "dclock" executable to a bin directory

```
$ dclock
Decimal time: 999
$ dclock -v
Decimal time clock that maps your day to 1000 decimal minutes, version 1.0.0 (x86_64-linux)
$ dclock -e
Decimal time: 49 (2024-10-30)
$
```
Building
========

Run `cabal build` and it will download dependencies and build the project. You can install the executable with the command:

```
cabal install --installdir=/usr/local/bin --install-method=copy
```

License
=======
MIT

Author
======
Travis Montoya <trav@hexproof.sh>
