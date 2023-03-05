FROM ubuntu:20.04 AS builder
ENV DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/Europe/Amsterdam /etc/localtime
RUN apt update; apt install -y libsqlite3-dev zlib1g-dev wget pkg-config g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase curl build-essential libgmp10 libncurses-dev libncurses5 libtinfo5 # libffi8ubuntu1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=8.6.5
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=true
ENV BOOTSTRAP_HASKELL_VERBOSE=true
ENV GHCUP_INSTALL_BASE_PREFIX=/opt
ENV BOOTSTRAP_HASKELL_ADJUST_BASHRC=true

ENV SHELL=/bin/bash
ENV TMPDIR=/tmp

RUN useradd -u 1000 -m user
RUN chown user /opt

WORKDIR /home/user
USER user
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
