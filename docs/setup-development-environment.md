# Setting up the development environment

Development uses both docker and cabal. Cabal is used to develop locally, testing and running parts of the code in GHCi. Docker is used for deployment, and for testing the software locally.

## Installing

Installation instructions differ slightly for every platform, but building is supported on all three major operating systems.

### Windows

1. Install Docker Desktop for Windows. <https://www.docker.com/products/docker-desktop/>
2. Install GHCup, a bootstrapping tool that manages ghc and cabal. GHCup can be found here: <https://www.haskell.org/ghcup/install/>.
3. Run `ghcup tui`, and install ghc version `8.6.5`. Select this version (two checkmarks).
4. Clone the repository using git: `git clone git@github.com:ideas-edu/refactor-tutor.git`. You might need to generate and add a ssh key for this to work from WSL. Run `ssh-keygen`, and add the contents of `~/.ssh/id_rsa.pub` to <https://github.com/settings/keys>.

### Linux

1. Install docker. Guide for ubuntu: <https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository>
2. Install GHCup, a bootstrapping tool that manages ghc and cabal. GHCup can be found here: <https://www.haskell.org/ghcup/install/>.
3. Run `ghcup tui`, and install ghc version `8.6.5`. Select this version (two checkmarks).
4. Clone the repository using git: `git clone git@github.com:ideas-edu/refactor-tutor.git`.

### MacOS

1. Install docker. Guide for MacOS: <https://docs.docker.com/desktop/install/mac-install/>
2. Install GHCup, a bootstrapping tool that manages ghc and cabal. GHCup can be found here: <https://www.haskell.org/ghcup/install/>.
3. Run `ghcup tui`, and install ghc version `8.6.5`. Select this version (two checkmarks).
4. Clone the repository using git: `git clone git@github.com:ideas-edu/refactor-tutor.git`.

## Running

### Locally

To build the tutor locally, run

```bash
cabal build
```

To clear build files and force a rebuild use

```bash
cabal clean
```

To enter a ghci repl, usr

```bash
cabal repl
```

To run unit and property tests, make sure `htfpp` is in the path, then use

```bash
cabal test
```

If you can't install `htfpp` then try stack, it comes with htfpp pre-installed:

```bash
stack test
```

To run regression tests use

```bash
./RegressionTests.hs \
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

