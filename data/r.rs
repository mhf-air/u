#!/usr/bin/env rustx
//! current Rust version: 1.98.0
//!
//! rust reference
//!     https://doc.rust-lang.org/reference/introduction.html

/*!
    another form of inner doc comment
*/

#![allow(unused_uses)]

extern crate foo as _;
extern crate std;
extern crate std as ruststd;

pub const NAME: &str = "hello";
