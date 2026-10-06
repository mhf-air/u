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

use {
    crate::a,
    crate::a::*,
    super::a,
    self::a,
    ::crate_abc::a,

    std,
    std::env,
    std::env::consts as env_consts,
    super::ApiResult as Result,
    std::option::{ Option, Option::Some, Option::None as nil },
    astd::option::{
        Option {
            self,
            Some,
            None as nil,
        },
        env,
    },
};

pub use crate::a;
pub(crate) use crate::a::*;
pub(super) use crate::a;
pub(in crate::abc) use crate::a;
use crate::a::*;
use super::a;
use self::a;
use ::crate_abc::a;

use std;
use std::env;
use std::env::consts as env_consts;
use std::env::consts
    as env_consts;
use super::ApiResult as Result;
use std::option::{ Option, Option::Some, Option::None as nil };

pub struct Point<T> {
    pub x: i32,
    y: i32,
}
pub struct Point<T> {x: T, y: T}
pub struct Point<T>(pub T, T);
struct Point;

pub union Point<T> where T: Clone {
    x: i32,
    y: i32,
}

pub enum Point<T> where T: Clone {
    Z,
    A(String, f64),
    B { name: String, weight: f64 },
    C {
        name: String,
        weight: f64,
    } = 1,
}
