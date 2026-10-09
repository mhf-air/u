#!/usr/bin/env rustx
//! current Rust version: 1.98.0
//!
//! rust reference
//!     https://doc.rust-lang.org/reference/introduction.html

/*!
    another form of inner doc comment
*/

#![allow(unused_uses)]

pub extern crate foo as _;
extern crate self;
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

pub unsafe extern "C" {
    #![inner]

    pub static VAR: &str = "var";
}

pub unsafe trait WithSelf<T>: Super<Self> where Self: Sized {
    #![inner]

    pub const MAX: i32;
    const MAX: i32 = 1;
    type B;
}

impl<T> Person<T> where T: Clone {
    #![inner]

    pub const MAX: i32;
    const MAX: i32 = 1;
    type B;
}
impl<T> Clone for Person<T> where T: Clone {
    #![inner]
}
unsafe impl<T> !Clone for Person<T> where T: Clone {
    #![inner]
}

fn f() {
    let a;
    let a: i32;
    let a = 1;
    let a: i32 = 1;

    let mut a;
    let mut a: i32;
    let mut a = 1;
    let mut a: i32 = 1;

    let a: fn(i32) -> i32;

    let Some(t) = s.t;
    let Some(t) = s.t else {
    };
    let (a, mut b) = {
        (a, b)
    };
    let (a, b): (i32, i32) = parse else {
    };
}

pub const async unsafe extern "C" fn f() {
}

fn f(&self, #[a] mut a: i32, mut b: i32, ...) -> i32 {
    let s = self;
    s.a = a;
}

#[rustfmt::skip]
fn f(
    self: i32,
    #[a]
    a: i32,
    b: i32,
    mut a: i32,
) {}

macro_rules! abc {
    ($l:expr) => { bar!($l); }
}

type Point = Option<i32>;
type Point = Option<(i32, i32)>;

type Point = [i32; 1];
type Point = &[i32];
type Point = fn(i32, i32) -> i32;
type Point = fn(
    i32,
    i32,
) -> i32;
type Point = for<'a> unsafe extern "C" fn(i32, i32) -> i32;
fn call<F>(f: F) where for<'a> F: Fn(&'a i32) {
}

fn f() {
    let func = 1;
    let ret = 1;
    let macro = 1;
    let union = 1;

    // should at \ after union here in u
    let a = union
        + 1;

    let Some(a) = b;

    let a = Person {
        age: 20,
        age: 20,
        ..Default::default()
    };
    let a = Person {
        age: 20,
        ..b
    };
}

fn f() {
    a(|| 1);
    a(|a| 1);
    a(|a: i32| 1);
    a(|a, b| 1);
    a(|a: i32, b: i32| 1);
    a(|a| {
        a.b;
    });

    a(|&a| 1);
    a(|&a: &i32| -> i32 {});

    a = |a| 1;
    a = async |a| 1;
    a = move |a| 1;
    a = async move |a| 1;

    a = async |&a| 1;
    a = async |&a: &i32| 1;
}

fn f() {
    if let Some(inner_opt) = outer_opt
        && let Some(number) = inner_opt
        && number == 1
        && let Person { name } = a
    {
    }

    while let Some(inner_opt) = outer_opt
        && let Some(number) = inner_opt
        && number == 1
        && let Person { name } = a
    {
    }

    loop {}

    for it in list {
    }
    for (it, t) in list {
    }
    for Some(it) in list {
    }
    for Person { name } in list {
    }

    'a: loop {
        break 'a 1;
    }
    'a: while a {
        break 'a;
    }
    'a: for it in list {
        break 'a;
    }
}

fn f() {
    match a {
        Some(xs) if let Person { x } = xs &&
            x < 0 => {}
        _ => {}
    }
}

#[rustfmt::skip]
fn f() {
    vec!(a);
    vec!{a}

    vec![a];
    vec![a, b, c];
    vec![
        a,
        b,
        c,
    ];

    abc!{ a b c }
    abc!( a b c );
    abc![ a b c ];

    stmt!{
        let a = 1;
    }
    stmt!(
        let a = 1;
    );
    stmt![
        let a = 1;
    ];
}
