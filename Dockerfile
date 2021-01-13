
################################################################################
# Dependency
################################################################################

# Pushed image of dependencies/Dockerfile
FROM hogeyama/hflmc2:dependency as dependency

################################################################################
# Build
################################################################################

FROM dependency as build
WORKDIR $HOME/hflmc2
RUN     sudo chown opam:opam .
COPY    --chown=opam:opam ./bin           $HOME/hflmc2/bin
COPY    --chown=opam:opam ./input         $HOME/hflmc2/input
COPY    --chown=opam:opam ./lib/          $HOME/hflmc2/lib
COPY    --chown=opam:opam ./test          $HOME/hflmc2/test
COPY    --chown=opam:opam ./hflmc2.opam   $HOME/hflmc2/hflmc2.opam
COPY    --chown=opam:opam ./dune-project  $HOME/hflmc2/dune-project
RUN     eval `opam config env` && \
        dune build && \
        dune install && \
        sudo cp `which hflmc2` /bin
COPY    dependencies/horsat2 /bin
RUN     sudo chmod 755 /bin/horsat2
ENV     LD_LIBRARY_PATH /home/opam/.opam/4.08/lib/z3

################################################################################
# Minimize
################################################################################

FROM ubuntu:20.04
COPY --from=dependency /home/opam/.opam/4.08/share/apron/lib/lib*       /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /home/opam/.opam/4.08/lib/z3/lib*                /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /usr/lib/x86_64-linux-gnu/libglpk.*              /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /usr/lib/x86_64-linux-gnu/libmpfr.*              /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /usr/lib/x86_64-linux-gnu/libgmp.*               /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /usr/lib/x86_64-linux-gnu/libamd.*               /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /usr/lib/x86_64-linux-gnu/libcolamd.*            /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /usr/lib/x86_64-linux-gnu/libltdl.*              /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /usr/lib/x86_64-linux-gnu/libgomp.*              /usr/lib/x86_64-linux-gnu/
COPY --from=dependency /usr/lib/x86_64-linux-gnu/libsuitesparseconfig.* /usr/lib/x86_64-linux-gnu/
COPY --from=build /bin/horsat2 /bin/
COPY --from=build /bin/hflmc2 /bin/

# WORKDIR $HOME
CMD ["hflmc2"]
