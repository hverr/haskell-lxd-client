#!/bin/sh

set -ex

LTS=$(awk '$1 == "resolver:" {print $2}' stack.yaml)
GHC=$(stack ghc -- --version | awk '{print $NF}')
PROC=$(uname -p)
PLATFORM=$(dpkg --print-architecture)
VERSION=$(awk '$1 == "version:" {print $2}' lxdfile.cabal)

stack clean
stack build

mkdir -p dist/release/
cp .stack-work/install/${PROC}-linux/${LTS}/${GHC}/bin/lxdfile dist/release/lxdfile_${VERSION}_${PLATFORM}
