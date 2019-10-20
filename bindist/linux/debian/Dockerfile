FROM ubuntu:eoan

ARG PARALLEL=-j8

ENV DEBIAN_FRONTEND noninteractive
ENV PATH /opt/ghc/bin:/opt/cabal/bin:$PATH

ARG DEPS_GHC="ghc ghc-doc ghc-prof cabal-debian cabal-install"
ARG DEPS_BUILD="build-essential dpkg-dev haskell-devscripts debhelper"
ARG DEPS_CLASH="libtinfo-dev libpthread-stubs0-dev"

RUN apt update && apt install $DEPS_GHC $DEPS_BUILD $DEPS_CLASH -y
RUN cabal new-update
