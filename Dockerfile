
FROM ocaml/opam2:ubuntu-lts as builder
ENV LANG C.UTF-8

################################################################################
# Install Dependencies
################################################################################

# Install required packages.
RUN sudo apt-get update && \
    sudo apt-get install -y --assume-yes \
      apt-utils \
      pkg-config \
      autoconf \
      libmpfr-dev \
      libgmp-dev \
      subversion \
      libglpk-dev \
      m4 \
      python \
      ocaml

RUN mkdir $HOME/depend

# OCaml packages
################

RUN opam init
RUN opam switch 4.06
RUN opam install camlp4 camlp5 extlib batteries zarith apron.20160125 glpk.0.1.8 z3.4.7.1 -y
RUN opam install cmdliner core fmt logs lwt menhir ppx_compare ppx_deriving ppx_deriving_cmdliner ppx_let ppx_sexp_conv spawn re2


# fpat
######

RUN     git clone https://github.com/hopv/MoCHi $HOME/depend/mochi-for-fpat
WORKDIR $HOME/depend/mochi-for-fpat/csisat
RUN     git checkout 688c094876c8fe121a50f1a1de55eb5e1d3a3e5f
RUN     opam exec -- make lib
WORKDIR $HOME/depend/mochi-for-fpat/fpat
RUN     eval `opam config env` && \
        autoconf && \
        ./configure && \
        make install-lib && \
        make depend && \
        make && \
        make install

################################################################################
# Build & Install
################################################################################

RUN     git clone https://github.com/Hogeyama/hflmc2 $HOME/hflmc2
WORKDIR $HOME/hflmc2
RUN     git pull && git checkout 8b98ff694fa451f13edac3daa01acc78b1f3eb8b
RUN     eval `opam config env` && \
        dune build && \
        dune install && \
        sudo cp `which hflmc2` /bin
COPY    .circleci/horsat2 /bin
RUN     sudo chmod 755 /bin/horsat2

################################################################################
# FOO
################################################################################

FROM ubuntu:16.04
COPY --from=builder /home/opam/.opam/4.06/share/apron/lib/lib* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /home/opam/.opam/4.06/lib/z3/lib* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libglpk.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libmpfr.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgmp.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libamd.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libcolamd.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libltdl.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libgomp.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /usr/lib/x86_64-linux-gnu/libsuitesparseconfig.* /usr/lib/x86_64-linux-gnu/
COPY --from=builder /bin/ /bin/

# WORKDIR $HOME
CMD ["hflmc2"]

