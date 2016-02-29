# The Hubris Programming Language 
[![Build Status](https://travis-ci.org/hubris-lang/hubris.svg?branch=master)](https://travis-ci.org/hubris-lang/hubris)

## Overview
Hubris is a brand new dependently typed programming language designed as
a vehicle for experimentation and research. The goal is to understand
what a tool that enables one to leverage both interactive and automated
reasoning to construct software.

Unlike similar tools our intention is to design a language amenable to
large scale verification projects, as well as being executable, effcient
and with a small [trusted computing base](https://en.wikipedia.org/wiki/Trusted_computing_base).

Hurbis is very much a work in progress and there is still lots to do, if
you are interested in contributing please get in contact with @jroesch, join
our Gitter channel or open a pull request or issue.

You can find more in depth documentation in our slowly growing 
[wiki](https://github.com/hubris-lang/hubris/wiki).

## Development

Currently development of Hubris is done using the nightly version of Rust. 
For instructions on how to install it go [here](https://www.rust-lang.org/downloads.html). 
You can also try Brian's great [multirust](https://github.com/brson/multirust) which 
is the recommended way of installing Rust.

If you have `multirust` you can switch to the appropriate version like so:
`multirust override nightly`

You can build Hubris with `cargo` using the standard commands, `build`, `test`,
etc.

In absence of a coding guideline we are currently using
[rust-fmt](https://github.com/rust-lang-nursery/rustfmt)
to keep a consistent style across the project.

You can find more about installation and use on their project page.

## Installing

Currently the language is still under rapid development so there are no supported
installation methods. If you would like to play with Hubris, you can simply 
can clone this repository and build the language yourself. It is currently 
being developed on OS X, so day-to-day Linux and Windows compatibility is an 
unknown.

## License

Hubris is licensed under the MIT license.
