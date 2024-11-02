dclock
======

This was mostly created for my own edification. This takes a simple calculation converting system time 
to usable decimal minutes left in the day. I wanted to utilize the machines package to create a nice 
compositional monadic pipeline and learn some new concepts.

Here is a blog post on the creation and explanation of it: [dclock in haskell](https://hexproof.sh/2024/10/22/dclock-in-haskell/)

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

License
=======
MIT

Author
======
Travis Montoya <trav@hexproof.sh>
