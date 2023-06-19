# Setting up the development environment

Development uses both docker and stack. Stack is used to develop locally, testing and running parts of the code in GHCi. Docker is used for deployment, and for testing the software locally.

## Installing

Installation instructions differ slightly for every platform, but building is supported on all three major operating systems.

### Windows

1. Install Windows Subsystem for Linux (WSL) version 2 or higher, and install a linux distribution (for example ubuntu). Example guide: <https://dev.to/luckierdodge/how-to-install-and-use-docker-in-wsl2-217l>
2. Install docker inside the WSL container. Do not install Docker Desktop for Windows, you want to use the docker installations from the WSL virtual machine. Guide for ubuntu: <https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository>
3. Install GHCup in WSL, a bootstrapping tool that manages ghc, cabal and stack. During installation you will be prompted to install stack, answer `Y`. You will also be asked to configure stack to use GHCup, also answer `Y` here. GHCup can be found here: <https://www.haskell.org/ghcup/install/>
4. Clone the repository using git: `git clone git@github.com:ideas-edu/refactor-tutor.git`. You might need to generate and add a ssh key for this to work from WSL. Run `ssh-keygen`, and add the contents of `~/.ssh/id_rsa.pub` to <https://github.com/settings/keys>.

### Linux

1. Install docker. Guide for ubuntu: <https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository>
2. Install GHCup, a bootstrapping tool that manages ghc, cabal and stack. During installation you will be prompted to install stack, answer `Y`. You will also be asked to configure stack to use GHCup, also answer `Y` here. GHCup can be found here: <https://www.haskell.org/ghcup/install/>
3. Clone the repository using git: `git clone git@github.com:ideas-edu/refactor-tutor.git`.

### MacOS

1. Install docker. Guide for MacOS: <https://docs.docker.com/desktop/install/mac-install/>
2. Install GHCup, a bootstrapping tool that manages ghc, cabal and stack. During installation you will be prompted to install stack, answer `Y`. You will also be asked to configure stack to use GHCup, also answer `Y` here. GHCup can be found here: <https://www.haskell.org/ghcup/install/>
3. Clone the repository using git: `git clone git@github.com:ideas-edu/refactor-tutor.git`.

## Running

### Locally

To build the tutor locally, run

```bash
stack build
```

To clear build files and force a rebuild use

```bash
stack clean
```

To run unit and property tests use

```bash
stack test
```

To run regression tests use

```bash
stack runhaskell RegressionTests.hs -- \
  /path/to/requests-rpt-cleaned-enhanced.db \
  /path/to/base.cgi \
  /path/to/compare.cgi
```

You will have to provide the following:
```
usage: RegressionTests.hs <database> <base> <compare>
  database: path to sqlite3 database with data to compare against
  base:     path to cgi executable of the base system
  compare:  path to cgi executable of the system to compare against
```

### Testing environment (docker)

To run a testing environment that makes use of the docker containers used in production, use the supplied makefile.

To build an executable and start a webserver serving both the frontend and backend, run 

```bash
make start-server
# or for short
make
```

To spawn a development shell with GHCup installed, run

```bash
make develop
```

To only build a docker image for production use, run

```bash
make build-release
```

