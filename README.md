[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![LGPL v2.1 License][license-shield]][license-url]

<h3 align="center">OSDP: Semi-definite programming (SDP) in OCaml</h3>

  <p align="center">
    OSDP is an OCaml frontend library to semi-definite programming (SDP)
solvers.
    <br />
    <a href="https://embedded-sw-vnv.github.io/osdp/doc/"><strong>Explore the docs »</strong></a>
    <br />
    <br />
<!--    <a href="https://github.com/Embedded-SW-VnV/osdp">View Demo</a> 
    · -->
    <a href="https://github.com/Embedded-SW-VnV/osdp/issues">Report Bug</a>
    ·
    <a href="https://github.com/Embedded-SW-VnV/osdp/issues">Request Feature</a>
  </p>
</div>

<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#documentation">Documentation</a></li>
    <li><a href="#troubles">Troubles</a></li>
    <li><a href="#license">License</a></li>
  </ol>
</details>

## About OSDP

OSDP is an OCaml frontend library to semi-definite programming (SDP)
solvers. 

See directory [example](https://github.com/Embedded-SW-VnV/osdp/tree/master/example) for how to use the library.

## Getting Started

### Prerequisites

OSDP relies on external optimization tools
- Csdp
- Mosek 
- SDPA

and on the following opam packages:
```sh
opam install ocamlfind dune conf-autoconf zarith ocplib-simplex.0.5 conf-csdp
```

#### Csdp

Csdp is free software and is available in most Unix systems.
Eg. on Debian-based systems
  ```sh
  apt-get install csdp
  ```
or, on OSX 
  ```sh
  brew install csdp
  ```
  
Csdp binary has to be available in the path.

#### Mosek 

Mosek is a commercial tool available at [www.mosek.com](https://www.mosek.com/).
OSDP is linked against the dynamic library of Mosek.
Make sure that the library is available in the dynamic library environment variable.

Let PATH_TO_MOSEKDIR be path that contains the dynamic library, eg. libmosek64.so on Linux and libmosek64.dylib on OSX.

- On Linux
```sh
 export LD_LIBRARY_PATH = PATH_TO_MOSEKDIR:$LD_LIBRARY_PATH
```

- On OSX
```sh
 export DYLD_LIBRARY_PATH = PATH_TO_MOSEKDIR:$DYLD_LIBRARY_PATH
```

#### SDPA 

Csdp is free software and is available in most Unix systems.
Eg. on Debian-based systems
  ```sh
  apt-get install sdpa
  ```
  
Like CSDP, it has to be available as a binary in the user path.

### Installation

See [INSTALL](https://github.com/Embedded-SW-VnV/osdp/blob/master/INSTALL).

#### Using opam:

```sh
opam install osdp
```

#### Using the source:

1. Clone the repo
   ```sh
   git clone https://github.com/Embedded-SW-VnV/osdp.git
   ```

2. Install dependencies
   ```sh
   opam install ocamlfind dune conf-autoconf zarith ocplib-simplex.0.5 conf-csdp
   ```

3. Compile
   ```sh
   autoconf
   ./configure
   make
   make install
   ```

## Usage

### As library

```ocaml
open Osdp
```
### Ocaml toplevel

To use with the ocaml toplevel
```sh
% rlwrap ocaml
# #use "topfind";;
# #require "osdp";;
```

These last two lines can be added to `~/.ocamlinit` to avoid typing them
again and again.

## Documentation

Type
```sh
% make doc
```
to generate documentation (then see doc/index.html)
or visit [https://embedded-sw-vnv.github.io/osdp/doc/](https://embedded-sw-vnv.github.io/osdp/doc/)

## Troubles

When running with OSX, some issues with the link of moseklib:
- either run the final binary with 
  ```sh
  DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:`dirname \`which mosek\``" my_new_binary
  ```
- or use install_name_tool to provide the absolute path in the dynamic linking lib. It has to be done for every binary using Osdp, even indirectly.

## License

Distributed under the LGPL-v2.1 License. 

[contributors-shield]: https://img.shields.io/github/contributors/Embedded-SW-VnV/osdp.svg?style=for-the-badge
[contributors-url]: https://github.com/Embedded-SW-VnV/osdp/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/Embedded-SW-VnV/osdp.svg?style=for-the-badge
[forks-url]: https://github.com/Embedded-SW-VnV/osdp/network/members
[stars-shield]: https://img.shields.io/github/stars/Embedded-SW-VnV/osdp.svg?style=for-the-badge
[stars-url]: https://github.com/Embedded-SW-VnV/osdp/stargazers
[issues-shield]: https://img.shields.io/github/issues/Embedded-SW-VnV/osdp.svg?style=for-the-badge
[issues-url]: https://github.com/Embedded-SW-VnV/osdp/issues
[license-shield]: https://img.shields.io/github/license/Embedded-SW-VnV/osdp.svg?style=for-the-badge
[license-url]: https://github.com/Embedded-SW-VnV/osdp/blob/master/LICENSE.txt
