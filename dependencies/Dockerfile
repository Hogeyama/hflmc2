
################################################################################
# Dependencies
################################################################################

FROM ocaml/opam2:4.08 as dependency
ENV LANG C.UTF-8

# apt dependency
################

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

# OCaml dependency
##################

RUN opam update && opam install -y\
 camlp4.4.08+1\
 camlp5.7.12\
 extlib.1.7.7\
 batteries.3.0.0\
 zarith.1.9.1\
 apron.20160125\
 glpk.0.1.8\
 z3.4.7.1\
 cmdliner.1.0.4\
 core.v0.13.0\
 fmt.0.8.8\
 logs.0.7.0\
 lwt.5.3.0\
 menhir.20190924\
 ppx_deriving.4.5\
 ppx_compare.v0.13.0\
 ppx_deriving_cmdliner.0.4.1\
 ppx_let.v0.13.0\
 ppx_sexp_conv.v0.13.0\
 spawn.v0.13.0\
 re2.v0.13.0

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

# ENV
#####

ENV     LD_LIBRARY_PATH /home/opam/.opam/4.08/lib/z3
WORKDIR $HOME

