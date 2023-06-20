#!/bin/sh -x

# Make sure we run noninteractive and skip tzdata config
export DEBIAN_FRONTEND=noninteractive
sudo ln -fs /usr/share/zoneinfo/Europe/Amsterdam /etc/localtime

# Install GHCup dependencies
sudo apt update
sudo apt install -y libsqlite3-dev zlib1g-dev wget pkg-config g++ gcc \
                    libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev \
                    git gnupg netbase curl build-essential libgmp10 \
                    libncurses-dev libncurses5 libtinfo5 sudo

# Configure GHCup
export BOOTSTRAP_HASKELL_GHC_VERSION=8.6.5
export BOOTSTRAP_HASKELL_NONINTERACTIVE=true
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=true

export GHCUP_INSTALL_BASE_PREFIX=/opt/ghcup
sudo mkdir -p $GHCUP_INSTALL_BASE_PREFIX
sudo chown -R $USER:$USER $GHCUP_INSTALL_BASE_PREFIX

# Install GHCup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org |  sh
