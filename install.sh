#!/usr/bin/env zsh

rm $HOME/.multirust/toolchains/stable/cargo/bin/hubris
cargo install --bin hubris --path .
mkdir -p $HOME/.hubris
cp -r lib $HOME/.hubris
