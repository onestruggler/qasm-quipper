# Sets the base image to Ubuntu 20.04
FROM ubuntu:20.04

# Update package manager
RUN apt update
RUN apt-get update

# Cabal setup (with correct version)
RUN apt-get install haskell-platform -y
RUN cabal update
RUN cabal install cabal-install

# Update PATH env variable to find new version of cabal
ENV PATH="/root/.cabal/bin:${PATH}"
RUN cabal update

# Installs Quipper command-line tools
RUN cabal install quipper

# Pulls the translator toolchain from git
RUN apt-get install git -y
RUN git clone https://github.com/onestruggler/qasm-quipper /linguaquanta
WORKDIR /linguaquanta
RUN cabal install
