Open an SVG viewer which watches for file changes (e.g. GNOME Loupe), then run:

```
ghciwatch --command "cabal repl logo.hs" --watch logo.hs --debounce=50ms --test-ghci :main
```
