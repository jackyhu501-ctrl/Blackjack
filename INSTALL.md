# Install & Build Instructions

## Packages Used

The game itself (library + executable) depends on **`ANSITerminal`** for
colored output (chip counts and round outcomes). Install it with:

```bash
opam install ANSITerminal
```

The test suite additionally depends on **`ounit2`** (test-only). Install it
with:

```bash
opam install ounit2
```


## Build

```bash
dune build
```

## Run the Game

```bash
dune exec bin/main.exe
```

## Run the Tests

```bash
dune test
```

## Clean Build Artifacts

```bash
dune clean
```

## Troubleshooting

- If `dune build` fails with a missing-package error, re-run
  `opam install . --deps-only --with-test` and make sure your current OPAM
  switch is active (`eval $(opam env)`).
- If a different OS causes issues, both author and grader can use the CS 3110
  virtual machine described in the OCaml Programming textbook appendix.
