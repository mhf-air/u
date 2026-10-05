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

pub const NAME: &str = "name";
pub static VAR: &str = "var";
pub static mut VAR: &str = "var";
pub unsafe static VAR: &str = "var";
pub safe static mut VAR: &str = "var";

#[rustfmt::skip]
pub type Point = i32 where 'a: 'b;
#[rustfmt::skip]
pub type Point = i32 where T: Clone + Copy;
#[rustfmt::skip]
pub type Point where T: Clone = i32;
#[rustfmt::skip]
pub type Point = i32 where
    T: Clone;
#[rustfmt::skip]
pub type Point = i32 where
    T: Clone,
;
#[rustfmt::skip]
pub type Point where
    T: Clone,
    T: Clone,
= i32;
