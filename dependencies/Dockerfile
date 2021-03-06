
################################################################################
# HoICE
################################################################################

FROM    rust:slim-buster as rust
RUN     apt-get update && \
        apt-get install -y \
        git \
        wget \
        unzip
RUN     git clone -b option-no-inlining https://github.com/Hogeyama/hoice /root/hoice
WORKDIR /root/hoice
RUN     cargo install --path .
WORKDIR /root
RUN     wget https://github.com/Z3Prover/z3/releases/download/z3-4.7.1/z3-4.7.1-x64-ubuntu-16.04.zip -O /root/z3.zip \
        && unzip /root/z3.zip \
        && mv z3-4.7.1-x64-ubuntu-16.04/bin/z3 /bin/z3

################################################################################
# Other Dependencies
################################################################################

FROM ocaml/opam2:4.08 as dependency
ENV LANG C.UTF-8

# apt dependency
################

RUN sudo apt-get update && \
    sudo apt-get install -y \
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
ENV LD_LIBRARY_PATH /home/opam/.opam/4.08/lib/z3

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

# horsat2
#########

RUN     git clone https://github.com/hopv/horsat2 $HOME/depend/horsat2
WORKDIR $HOME/depend/horsat2
RUN     git checkout 22c8b3e5d6a5751f28824182d4f27728e3acbf82
RUN     make && sudo cp ./horsat2 /bin

# import HoICE
##############
COPY --from=rust /usr/local/cargo/bin/hoice /bin
COPY --from=rust /bin/z3 /bin

WORKDIR $HOME

