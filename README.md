# The Hubris Programming Language
[![Build Status](https://travis-ci.org/hubris-lang/hubris.svg?branch=master)](https://travis-ci.org/hubris-lang/hubris)
Hubris is a brand new dependently typed programming language designed for
experimentation and research.

It is still very much a work in progress and there is much to do, if
you are interested in developing please email @jroesch, or open a PR.

You can find more in depth documentation in our [wiki](https://github.com/hubris-lang/hubris/wiki).

## Development

Currently development of Hubris is on Rust Nightly. For instructions on how to
install it go [here](https://www.rust-lang.org/downloads.html). You can also try
Brian's great [multirust](https://github.com/brson/multirust) which is my recommended
way of installing Rust.

If you have `multirust` you can switch to the appropriate version like so:
`multirust override nightly`

You can build Hubris with `cargo` using the standard commands, `build`, `test`,
etc.

In absence of a coding guideline we are currently using
[rust-fmt](https://github.com/rust-lang-nursery/rustfmt)
to keep a consistent style across the project.

You can find more about installation and use on their project page.

## Installing

Currently since the language in under rapid development there are no supported
installation methods, you can clone this repository and build the tool yourself,
it is currently being developed on OS X, and Linux and Windows compatibility
in unknown.

## License

Hubris is licensed under the MIT license.
