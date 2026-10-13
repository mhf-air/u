#!/usr/bin/env rustx
//! current Rust version: 1.98.0
//!
//! rust reference
//!     https://doc.rust-lang.org/reference/introduction.html
//!
//! - open *.rs: read *.rs -> rust's ast -> write .*.u
//! - write .*.u: u u-compile .*.u

/*!
    another form of inner doc comment
*/

#![allow(unused_uses)]

// todo
// - goto-definition jump to the correct position in u

pub extern crate self;
extern crate std as ruststd;
extern crate foo as _;

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
use std::option::{ Option::*, Option };
use *;

fn f() {
    use ::crate_abc::Abc;
    type Point = ::crate_abc::Abc;
    ::crate_abc::f();
}

mod include_mod;
mod empty_mod {}

mod vis_self {}
pub(self) mod vis_self {}
pub(super) mod vis_super {}
pub(crate) mod vis_crate {}
pub(crate) mod vis_crate {}
pub(in crate::a::b) mod vis_in_path {}

pub const NAME: &str = "hello";
static mut NAME: &str = "hello";

#[allow(non_camel_case_types)]
pub mod sub {
    pub const NAME: &str = "hello";
    static mut NAME: &str = "hello";
}

#[derive(Serialize, Deserialize)]
pub(crate) struct Param {
    #[serde(default = "default_page")]
    pub(crate) default_page: i32,
    #[serde(default = "api::default_limit")]
    default_limit: i32,

    tag: Option<String>,
}

fn f(mut a: i32, tag: Option<String>) {
    if let Some(tag) = tag {
        t.to_rust();
    }
}

fn f() {
    let func = a;
    let ret = a;
    let macro = a;
    let union = a;

    // ""s to "".to_owned()
    "".to_owned();
    // ""st to "".to_string()
    "".to_string();

    // fast eprintln!()
    eprintln!("{:?}", a);
    eprintln!("{:?}", a.b);

    print!("tag: ");
    eprintln!("{:?}", tag);
}

// this will throw an error, because (a b) is not an expr list
// vec_(a b)
// similarly, {a b} is not a stmt list
// vec_"stmt"{a b}

vec!(1, 2, 3);
vec![1, 2, 3];
call!(1, 2, 3);
call![1, 2, 3];
call!{1, 2, 3}
call!( select a from b );
call![ select a from b ];
call!{ select a from b }
call!(
    let a = 1;
);
call![
    let a = 1;
];
call!{
    let a = 1;
}
call!{
    pub const NAME: &str = "hello";
    -1;

    a::r#match() as i32;
    a::b() as i32;

    let (1, ) = a;

    continue;
    break;
    return;

    a = '😻';
    a = 1_234.0E-18_f64;

    a = 1.0;
    a = a.0;
    a = a.0.0;

    a = 'a';
    a = b'a';

    a = "foo\"";
    a = "foo
bar";
    a = r#"foo"#;
    a = r##"foo#"##;
    a = br##"foo#"##;
    a = b##"foo#"##;
    a = b#"package
name = "foo"
# see more
"#;
}
call!{
    let a = 1.1;
    if a > 0 {
        return;
    }
    #[derive(Debug)]
    pub struct OrderItem {
        name: String,
        age: i32,
    }

    pub fn api_config() -> api::ApiConfig {
        let mut list = vec![1, 2, 3];
        list[0] = 10;

        let func = 1;
        let macro = 1;
        let ret = 1;

        let a = api::ApiConfig {
            ..Default::default()
        };
        a
    }

    fn abc(count: i32, ...);
}

fn f() {
    vec!(1);
}
vec!(1);
trait Clone {
    vec!(1);
}
impl Person {
    vec!(1);
}
extern {
    vec!(1);
}

fn f() {
    Person { name: "", ..b };
    Person {
        name: "",
        ..b
    };
    Person {
        name: "",
        age,
    };

    (a,);
    (a, b);
    (
        a,
        b,
    );

    let a: &[i32; 1] = todo!();
    &[1..2];
    let a: &'a [i32] = todo!();
    let a: [i32; 1] = todo!();
    [0, 1];
    list[0];
    list[1..2];
    list[..];

    fn f(a: [i32; 1]) {
    }
    [0; 1];
    [0, 1];
    [
        0,
        1,
    ];
    [
        0,
        1,
    ];
    [
        a,
    ];

    f(a, b);
    a.b(
        a,
        b,
    );
    f(
        a,
        b,
    );
}

fn f() {
    Person { name: "", ..Default::default() };
    Person { name: "", ..Default::default() };
    Person {
        name: "",
        ..Default::default()
    };

    Person { name: "", ..b };
}

fn identifier() {
    _ = "-";
    a = "a";

    c_ = "c-";
    _c = "-c";
    _A = "-A";
    aa = "aa";
    Aa = "Aa";

    // the following two cases are forbidden
    // aA = "aA"
    // AA = "AA"
    // A--b = ""
    // A-1 = ""

    __ = "--";
    ___ = "---";
    __r = "--r";
    __c = "--c";

    NAME = "name--c";

    Order = "Order";
    Order_ = "Order---r";
    OrderItem = "Order-item";
    Order_item = "Order-item--r";
    a_A = "a-A--r";

    order = "order";
    submit_order = "submit-order";
    mut_ = "mut-";

    c_void = "c-void";
}

// use fn() for function type, not func
type Point<T> = fn(T, T) -> T;
pub type Point<T: Copy + Default> = (T, T);
pub(crate) type Point<T: Copy + Default> = (T, T);
type Point<const N: usize>: Copy + Clone = [i32; N];
type Point<T> = (T, T) where T: Copy;
type Point<T> = fn(T) where T: Iterator;
type Point<'a: 'b + 'c, 'd: 'e> = &'a i32;
type Point<T> = abc::Abc::<T>;
type Point<T> = std::boxed::Box<dyn std::ops::FnOnce(isize) -> isize>;
type Point<T> = impl Iterator<Item = ops::Example<T>>;
type Point<T> = impl Iterator<Item: ops::Example<T>>;
type Point = <S>::f;
type Point = <S as Copy>::f;

async unsafe fn foo<'a, 'b, T>(a: i32, b: i32) -> i32 where
    T: Iterator,
    T::Item: Copy,
    String: PartialEq<T>,
    T: Iterator<Item: Copy>,
    fn(T): Iterator,
    f: Box<dyn FnMut(&mut T) + 'a>,
    'a: 'b,
{}

fn f(
    a: i32,
    b: i32,
) {
    f(
        a,
        b,
    );
    vec!(
        a,
        b,
    );
}

fn f(a: fn(&mut App), b: i32) {
    list.collect::<Vec<_>>();

    id = 1;
    a = &id;
    a = &mut id;
    a = &raw mut id;
    a = &raw const id;

    a = &&id;
    a = &&mut id;
    a = &&raw mut id;
    a = &&raw const id;
    &raw const id;
}

fn const_c_void<T>(r: &T) -> *const c_void {
    return r as fn() as i32;
    return r as *const T as *const c_void;
}

struct Person<'a> where
    T: Clone,
{
    #[serde(rename = "lnum")]
    pub name: &'a str,
    age: i32,
    desc: String,
}
impl Person {
    fn no_s(&self) -> String {
        self.desc;
    }
    fn has_s(&self) -> String {
        let s = self;
        s.desc;
    }
}

impl fmt::Debug for Person<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self;
        f.debug_struct("")
            .field(&s.name, "")
            .field(&s.age, "")
            .finish();
    }
}
impl !Clone for Person {}

struct Pair<T, U>(pub T,pub U);
struct Pair<T, U>(pub(crate) T, U);
struct Pair<T, U>(T, U);

struct Foo<const N: usize = 10>;

#[repr(u8)]
enum Color {
    Red = 1,
    Green,
    Blue,
}
impl Color {
    const RED_2: Color = Color::Red;
}

#[repr(u8)]
enum Foo {
    A(u8),
    B(i8),
    C(bool) = 42,
}

enum Animal<T, E> where
    T: Clone,
{
    Dog(String, f64),
    Cat {
        name: String,
        weight: f64,
    },
}

union MyUnion<T> where
    T: Clone,
{
    pub name: String,
    age: i32,
}

trait WithSelf: Super<Self> where
    Self: Sized,
{}
trait CopyIterator: Iterator<Item: Copy> {}

unsafe trait Seq<T>: Display + Debug where T: Copy {
    const NAME: &str = "hello";
    type Item = T;
    type A;
    type Assoc: Trait2<Assoc2: Copy>;

    fn foo() where Self: Sized {
    }

    fn mov(self);
    fn mov(self: Rc<Self>);

    fn len(&self) -> u32;
    fn elt_at(&self, n: u32) -> T;
    fn iter<F>(&mut self, f: F) where F: Iterator;

    call!();
}

trait Foo {
    type Bar<'a>;

    type Item<'a> where Self: 'a;
    fn next<'a>(&'a mut self) -> Option<Self::Item<'a>>;

    type Pointer<T>: Deref<Target = T>;
    fn new<T>(value: T) -> Self::Pointer<T>;

    type Array<'x, const N: usize> where
        Self: 'x,
    = i32;
    fn borrow_array<'a, const N: usize>(&'a self) -> Self::Array<'a, N>;
}

impl Person {
    fn r(&self, a: &str) {}
    fn rl(&'a self, a: &str) {}
    pub fn rm(&mut self, a: &str) {}
    pub fn rlm(&'a mut self, a: &str) {}

    pub fn sf(self, a: &str) {}
    pub fn m_sf(mut self, a: &str) {}

    pub fn typ(self: crate::Abc, a: &str) {}
    pub fn m_typ(mut self: crate::Abc, a: &str) {}

    pub(self) fn p_self(self, a: &str) {}
    pub(crate) fn p_crate(self, a: &str) {}
    pub(in crate::abc) fn p_path(self, a: &str) {}

    fn f(
        &self,
        a: &str,
        b: &str,
    ) {}
}

impl Point<f32> {
    fn distance(&self) -> f32 {}
}

impl<T> Point<T> where
    T: Clone,
{
}

unsafe impl<T, const N: usize> MyTrait for [T; N] {}
impl<I> ops::Index<I> for str {}
impl fn(i32) -> i32 {}
impl<T> fn(T) -> T {}

impl crate::Person {
    fn abc(&self) {
        let s = self;
        crate::a;
    }
}
impl self::Person {}
impl super::Person {}

fn apply<F>(f: F) where F: FnOnce() { }
fn apply<F: FnOnce()>(f: F) { }

// these are functions
fn capture<'a, T>(x: &'a (), y: T) -> impl Sized + use<'a, T, Self> {}
fn capture<'a, T>(x: &'a (), y: T) -> impl Sized + use<> {}

pub unsafe extern "C" {
    fn abs(input: i32, ...) -> i32;
    safe fn abs(input: i32, ...) -> i32;
    static mut NAME: i32;
    safe static mut NAME: i32;
    call!(1);
    call!{1}
    call![1];
    call!{}
}

#[no_mangle]
extern "C" fn call_from_c() {
}

unsafe fn dangerous() {
}

unsafe extern "C" fn sum(mut args: ...) -> i32 {
}
unsafe extern "C" fn sum(args: ...) -> i32 {
}

fn f() {
    unsafe {
        dangerous();
    }

    unsafe {
        dangerous()
    }

    a = unsafe {
    };

    async {
    }

    a = async move {
    };

    const {
        1
    }

    a = const {
        1
    };
}

fn f() {
    a = match c {
        ..5 => "a",
        5..6 => "b",
        6..=7 => "c",
        8 => "d",
        9.. => "e",
    };
    a = match c {
        ..=6 => "a",
        _ => "b",
    };
    a = match c {
        ..a => "a",
        b..c => "b",
        d..=e => "c",
        f => "d",
        g.. => "e",
    };
    match tuple {
        (1, .., y, z) => {}
        (.., 5) => {}
        (..) => {}
    }
}

fn f() {
    let (a, b) = parse;

    let (a, b): (i32, i32) = parse;

    let (a, b) = parse else {
        return;
    };

    let (a, mut b) = {
        (a, b);
    };

    a * b + c;

    a(|a: i32| -> i32 { 1 });
    a(|mut a: i32| -> i32 { 1 });
    a(|mut a| 1);
    a(move || { 1 });
    a(move |a, b| 1);
    a(|_| { let (a, b) = p; });
    a(|| {
        let (a, b) = {};
    });
    a(async || {
        return 1;
    });
    a(async move || 1);

    let (mut a, mut b) = (0, 1);
    (b, a) = (a, b);
    Point { x: a, y: b } = Point { x: 3, y: 4 };
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

    let a: fn(i32, ...);
}

fn f() {
    if a < 2 {
        -1;
    }

    b = if a < 2 {
        -1;
    } else {
        1;
    };

    if a < 2 {
        -1;
    } else if a > 10 {
        0;
    } else {
        1;
    }

    if let Err(err) = b {
    }

    if let Err(x | y) = b {
    }

    if let Color::Red = b {
    }

    if let a::Person = b{ name } {
    }

    if if true { true } else { false } {
    }
}

fn f() {
    if let E::X(n) | E::Y(n) = v {
    }
    if let Some(b) = a {
    }

    // use , instead of && as separators between chains of conditions,
    // because && can be regarded as part of the expr
    if a == b && c && d && #[some_attr] let Some(inner) = outer &&
            let Some(number) = inner &&
            number == 1 {
    }
    if a == b && let Some(n) = c && n > 0 {
    }

    while let E::X(n) | E::Y(n) = v {
    }
    while let Some(b) = a {
    }
    while a == b && c && d && #[some_attr] let Some(inner) = outer &&
            let Some(number) = inner &&
            number == 1 {
    }
    while a == b && let Some(n) = c && n > 0 {
    }

    // if pattern contains Struct Pattern, have to use : before pattern
    if let Person{ name } = a {
    }
    while let Person{ name } = a {
    }
    for Person{ name } in list {
    }
    match s.p {
        Some(p) if let Person{ name } = p => {}
        _ => {}
    }

    while { true } {
    }

    while let Some(a) = a && (Abc{ n: 1 }.n > 1) {
    }
}

fn f() {
    match message {
        Message::Quit => a,
        Message::WriteString(write) => a,
        Message::Move{ x, y: 0 } => a,
        Message::Move{ .. } => a,
        Message::ChangeColor{
            0: red,
            1: green,
            2: _,
        } => a,
        Message::ChangeColor{
            0: red,
            1: green,
            2: _,
        } =>
            a,
        _ => {}
    }

    match msg {
        Some(x) if x > 0 => {
            a();
        }
        Some(x) if let Ok(y) = compute(x) => {
            a();
        }
        a => break,
        _ => {}
    }
}

fn f() {
    match message {
        Message::Quit => dbg!("Quit"),
        Message::WriteString(write) => dbg!(&write),
        Message::Move{ x, y: 0 } => dbg!("move {} horizontally", x),
        Message::Move{ .. } => dbg!("other move"),
        Message::ChangeColor{ 0: red, 1: green, 2: _ } =>
            dbg!("color change, red: {}, green: {}", red, green),
        _ => {}
    }
}

fn f() {
    loop {}
    'a: loop {} // '

    while a.len() > 0 {}

    while let Some(v @ 1) | Some(v @ 2) = vals.pop() {}

    for item in list {}

    for (i, item) in list.enumerate() {}

    for n in 0.. {}

}

fn f() {
    a = 'x: {
        break 'x 1;
    };
}

// magic cases, only useful for rust-analyzer
/* f func() {
    if a -> a
    match a
    match a {
        -
    }
    for a -> a
} */

/** title
this is `num--c` and `version`

```
a := 1
```

```
b
```
*/
/// a
fn f() {
    let sum: u32 = (0..)
        .map(|n: u32| -> u32 { n * n })
        .take_while(|n: &u32| -> bool { *n < 1000 })
        .filter(|n: &u32| -> bool { *n % 2 == 1 })
        .fold(0, |acc: u32, n: u32| { acc + n } );

    let sum: u32 = (0..)
        .map(|n| n * n)
        .take_while(|n| *n < 1000)
        .filter(|n| *n % 2 == 1)
        .fold(0, |acc, n| acc + n);
}

macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}

// -----------------------------------------------------------------------------
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
    a;
    // add ; before ~a
    -a;
    *a = 1;
    b
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

    token!{ a b c }
    token!( a b c );
    token![ a b c ];

    stmt!{
        let a = 1;
    }
    stmt!(
        let a = 1;
    );
    stmt![
        let a = 1;
    ];

    // special cases
    call!(a);
    call![a];
    call!{a}
}
