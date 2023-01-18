Trivial htmx server.

Install [stack](https://docs.haskellstack.org/en/stable/) and run

```sh
stack build
stack run -- --server --log-path /tmp/log --content-path content --http-port 8080 --no-tls 
```

Or install [cabal](https://www.haskell.org/cabal/) and run

```sh
cabal build
cabal run Server -- --server --log-path /tmp/log --content-path content --http-port 8080 --no-tls
```

then browse to [localhost:8080](http://localhost:8080)

