FROM ocaml/opam:ubuntu-22.04-ocaml-5.0

ENV DEBIAN_FRONTEND=noninteractive

USER root
#RUN apt-get update && apt-get install -y \
#    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/yorksap
RUN opam-2.1 update

COPY --chown=opam yorksap.opam .
RUN opam-2.1 install . --deps-only
COPY --chown=opam . .
RUN opam-2.1 install . --deps-only

RUN eval $(opam-2.1 env) && dune build bin/main.exe

FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /root/
COPY --from=0 /home/opam/yorksap/_build/default/bin/main.exe ./yorksap
COPY --from=0 /home/opam/yorksap/london.json ./london.json

CMD /root/yorksap server
