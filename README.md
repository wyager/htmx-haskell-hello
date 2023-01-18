Trivial htmx server.

Install [stack](https://docs.haskellstack.org/en/stable/) and run

```sh
stack run -- --server --log-path /tmp/log --content-path content --http-port 8080 --no-tls 
```

then browse to [localhost:8080](http://localhost:8080)
