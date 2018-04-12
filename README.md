# About

A small VM built in Rust. It lexes, parses, builds and eventually runs code.

# Usage

## Interpreter

`./lang`

## Build

`./lang build inputfile -o outputfile`

|Option|Effect|
|-|-|
|`--release`|Removes the embedded code to save space and obfuscate.|
|`--optimized`|Removes all meta data regarding position of operations.|

## Run

`./lang run inputfile`

|Option|Effect|
|-|-|
|`--shell`|Stay in shell after program has executed.|

# Installation

With cargo; `cargo build;`