# Yorksap

## Usage

```
opam switch create . 5.0.0 --no-install
opam install . --deps-only --with-test
dune runtest
OCAMLRUNPARAM=b DSN="sqlite3://$PWD/yorksap.sqlite3" dune exec bin/main.exe server
```
