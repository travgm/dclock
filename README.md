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
$ cabal run exe:dclock -- -h
dclock - decimal time clock

Usage: dclock [(-v|--version) | [-e|--extended] [-w|--watch]]

  Decimal time clock that maps your day to 1000 decimal minutes

Available options:
  -v,--version             Show version information
  -e,--extended            Show extended information including date
  -w,--watch               Watch mode, view as a realtime decimal clock (updates
                           every second)
  -h,--help                Show this help text
$ cabal run
Decimal time: 25
$ cabal run exe:dclock -- -e
Decimal time: 25 (2024-11-05)
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
