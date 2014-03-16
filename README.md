# dagd-snap

This is a Haskell rewrite of the http://da.gd/ site (originally PHP).

It is written in the Snap framework and uses snaplet-postgresql-simple for DB
access.

It is licensed under the same Apache-2.0 license as the original site.

Development install should be roughly this:

```
git clone git://github.com/CodeBlock/dagd-snap
cd dagd-snap
cabal sandbox init
cabal install -fdevelopment
./.cabal-sandbox/bin/dagd
```

## Differences from the original

- Endpoints that only ever return text now return `text/plain` always.
