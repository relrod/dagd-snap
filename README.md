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
- Shorten now passes everything AFTER the shorturl along with the redirect,
  including query strings. i.e., if `/e` redirects to `http://example.com`,
  `/e/` will redirect to `http://example.com/`, `/e/foo/bar` will redirect to
  `http://example.com/foo/bar`, and `/e?foo=1` will redirect to
  `http://example.com?foo=1`.
  - The differences here are:
    - Adding `/` to the shorturl adds a `/` to the redirect.
    - Querystrings get passed along now too, since we don't use them internally
      for routing anymore.
