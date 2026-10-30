# Install and build

See the [README setup instructions](README.md#first-time-setup) for first-time installation on Windows (WSL), macOS, and Linux, including opam initialization and package installation.

With the project dependencies installed and the correct opam switch active:

```sh
eval $(opam env)
dune build
dune exec bin/main.exe
dune test
```

Use `dune clean` to remove generated build files. Dune 3.22 or newer is required.
