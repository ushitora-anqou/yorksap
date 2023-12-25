# yorksap

```
opam switch . 5.0.0 --no-install
opam pin cohttp https://github.com/mirage/ocaml-cohttp.git#v6.0.0_beta1
opam pin cohttp-eio https://github.com/mirage/ocaml-cohttp.git#v6.0.0_beta1
opam pin caqti.2.1.0 https://github.com/paurkedal/ocaml-caqti.git#6a475abf95565b4ad252d210e10b52aec1bffa97
opam pin caqti-eio.2.1.0 https://github.com/paurkedal/ocaml-caqti.git#6a475abf95565b4ad252d210e10b52aec1bffa97
opam pin caqti-driver-sqlite3.2.1.0 https://github.com/paurkedal/ocaml-caqti.git#6a475abf95565b4ad252d210e10b52aec1bffa97
opam install . --deps-only --with-test
dune runtest
```
