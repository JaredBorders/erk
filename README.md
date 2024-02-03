# erk

## Overview

**erk** provides an OCaml implementation of [Merkle Patricia Trees](https://ethereum.org/en/developers/docs/data-structures-and-encoding/patricia-merkle-trie/) and [Verkle Trees](https://ethereum.org/roadmap/verkle-trees), essential for Ethereum and other blockchain state management.

It utilizes OCaml and libraries from Jane Street and MirageOS.

## Prerequisites

-   **Homebrew**: This guide assumes Homebrew is installed on macOS.
-   **OCaml and OPAM**: Required for managing the OCaml environment and dependencies.
-   **Dune**: The build system for OCaml.

## Installation Steps

### Install OCaml and OPAM

```zsh
brew install ocaml
brew install opam
```

### Initialize OPAM

```zsh
opam init
eval $(opam env)
```

### Install Dune

```zsh
opam install dune
```

### Install Dependencies

Install the required libraries:

-   [Core](https://github.com/janestreet/core): Jane Street's alternative to OCaml's standard library.
-   [Async](https://github.com/janestreet/async): For asynchronous programming.
-   [Digestif](https://github.com/mirage/digestif): A cryptographic library.
-   [PPX_Jane](https://github.com/janestreet/ppx_jane): Standard PPX rewriters.

```sh
opam install core async digestif ppx_jane
```

### Build the Project

Compile with Dune:

```sh
dune build
```

### Execution

Run the application:

```sh
dune exec bin/main.exe
```

## Code Formatting

[OCamlformat](https://github.com/ocaml-ppx/ocamlformat) is used for code formatting. Install OCamlformat via OPAM:

```sh
opam install ocamlformat
```

Format code automatically at build time with Dune:

```sh
dune build @fmt --auto-promote
```

Or format code manually:

```sh
dune fmt
```

## License

The project is licensed under the MIT License.
