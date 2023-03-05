#!/bin/sh -x

# Make sure we run noninteractive and skip tzdata config
export DEBIAN_FRONTEND=noninteractive
ln -fs /usr/share/zoneinfo/Europe/Amsterdam /etc/localtime

# Install GHCup dependencies
apt update
apt install -y libsqlite3-dev zlib1g-dev wget pkg-config g++ gcc \
               libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev \
               git gnupg netbase curl build-essential libgmp10 \
               libncurses-dev libncurses5 libtinfo5 sudo

# Configure GHCup
export BOOTSTRAP_HASKELL_GHC_VERSION=8.6.5
export BOOTSTRAP_HASKELL_NONINTERACTIVE=true
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=true


# Make sure we have an user with uid 100
USER_ID=1000
id $USER_ID
if [ $? -ne 0 ]; then
  useradd -u $USER_ID -m user
fi

# Help sudo -E along a little
export USER="$(id -un $USER_ID)"
export HOME="$(eval echo ~$USER)"
export SHELL=/bin/bash
export TMPDIR=/tmp
cd $HOME

export GHCUP_INSTALL_BASE_PREFIX=/opt/ghcup
mkdir -p $GHCUP_INSTALL_BASE_PREFIX
chown -R $USER:$USER $GHCUP_INSTALL_BASE_PREFIX

# Install GHCup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sudo -Eu $USER sh
