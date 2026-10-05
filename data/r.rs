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
pub(crate) static mut VAR: &str = "var";
pub unsafe static VAR: &str = "var";
pub safe static mut VAR: &str = "var";

#[rustfmt::skip]
pub mod math {
    #![inner]

    pub type Point<'a, T> = i32 where T: 'a;
    pub type Point<T, const N: usize> = i32 where T: Clone + Copy;
    pub type Point<T: Clone + Default> where T: Clone = i32;
    pub type Point = i32 where
        T: Clone;
    pub type Point = i32 where
        T: Clone,
    ;
    pub type Point<'a, 'b, T> where
        T: Clone,
        'a: 'b,
    = i32;
}
