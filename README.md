# dagd-snap

This is a Haskell rewrite of the http://da.gd/ site (originally PHP).

It is written in the Snap framework and uses snaplet-mysql-simple for DB
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
- We now expect that URLs are properly encoded. For example,
  `/headers/http://www.google.com/` now becomes
  `/headers/http%3A%2F%2Fgoogle.com%2F`. You can, however, use
  `/headers/?site=http://google.com` without encoding.
