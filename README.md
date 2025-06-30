# Compile U to Rust

## Summary
A new syntax that compiles to Rust code

The semantics are the same as that of Rust(like borrow checker, etc.) and it can use all the rust libararies

Basically, it's just Rust with another syntax, but easier to read and write

## Motivation
I like Rust's semantics, but dislike some of its syntax

## Major Changes

- **allow dash(-) in identifiers**

	change snake_case, UpperCamelCase, SCREAMING_SNAKE_CASE to **dash-case** because it's the best way to seperate words

	- Upper-camel-case for UpperCamelCase
	- set-name for set_name
	- name--c for NAME(which is a const)
	- name--g for NAME(which is a static)
	- AnYThInGcUsTom--r for AnYThInGcUsTom

	as for identifiers containing digit, forbid '-' followed by a digit, and consider digits as lower-case letters.

	for example, Point2d, Mat4x4 are fine, but Point-2d is not.

	side effect: all minus related operators have to be changed with ~.

	this needs some adaptation, but the good news is that these operators are not frequently used

```
example

	Order-item, order-name, dir-path--c, name--g

and minus becomes
	~1, ~~
```

- **use [ ] for generics**

	use [T] because [ ] are easier to type(no Shift)

	side effect: have to use {{ }} for array related places to avoid parsing ambiguity.
	It's worth it, because generics are used more often than arrays

	so turbofish is unneccesary, since [ ] are solely used for generics, all the [ ] in expressions can be transformed to turbofish in Rust

```
example

	User struct {
		desc     Option[string]
		tags     Vec[string]
		an-array {{i32: 10}}
	}

and array becomes
	a{{0}} = 1

	{{u32: 1}}

	{{0: 10}}
	{{0, 1}}

vec usages
	vec,,(0, 1, 2)

	vec,,{0; 10}
	// exception: not vec,,{0: 10}

turbofish becomes
	list.iter().collect[Vec[-]]()

```

- **for all declarations, put the symbol name first**

	it fits my thought process. Whenever I think of something, the name is the first thing that pops up, and it's the most important,
	all other stuff are complimentary information

	**use new syntex for Trait implementation**

	Type [Generics]? impl Trait? where? {}

	if Type contains func or Type and [Generics] are ambiguous, wrap Type in parentheses
	(because such cases are rare, so most other cases requires less typing)

```
example

	dir-path--c const &str = "/tmp"
	server--g static Option[Server] = None

	User+['a] struct {
		name+ &'a str
		age   i32
	}

	private-func+ func(id string, count i32) i32 {
		a := 1
		a let i32 = 1
		a let mut = 1
		a let mut i32 = 1
		ret 0
	}

	Point[T, U] [T, U] impl[unsafe] ops..My-trait where {T Copy + Default} {
		Output type = usize

		new func()

		mixup[V, W] (&) func[unsafe, async](other Point[V, W]) Point[T, W]
		mixup[V, W] (&mut) func[unsafe, async](other Point[V, W]) Point[T, W]
		mixup[V, W] (Self) func[unsafe, async](other Point[V, W]) Point[T, W]
		mixup[V, W] (Box[Self]) func[unsafe, async](other Point[V, W]) Point[T, W]
		mixup[V, W] (Pin[Box[Self]]) func[unsafe, async](other Point[V, W]) Point[T, W]
	}
```

- **move method receiver to a different place, and use s as self**

	implementation: add
		let mut s = self;
	at the start of method body

	pros: it's clearer and shorter

```
example

	User impl {
		get-age+ (&) func i32 {
			< s.age
		}

		set-age (&mut) func(new-age i32) {
			s.age = new-age
		}

		move (Self) func() {
			s.age = 0
		}
	}
```

- **drop trailling semicolon**

	use Go's method, automatically insert semicolons where possible(if the last token in a line is identifiers, most operators, ...)

	note: semicolons are also inserted after line-ending . or ..

	note: write semicolons in macro definitions, as in regular Rust code

	side effect: use < Expr as the result expression

```
example

	a++
	a := if b > 0 { < b } else { < -b }
```

- **use .. insteaf of :: for path separator**

	because it's easier to type and distinguishes path segments better

	side effect: have to use `` for Rust's .. operator, etc.

```
example

	import {
		std..io
		std..fs
	}

	compile-file func(filepath &str) io..Result[Parse] {
		data := fs..read-to-string(&filepath)?
		compile-string(data)
	}

	for i in 0``10 {
	}
```

- **forbid leading :: before a path**

	Useless to me. When mod name and external crate name collide, change one of those

- **add another syntax for let, and add mut to all function parameters**

	in my opinion, the bindings should be mutable by default, because that is the most frequent situation.

```
example

	a := 1
	b let = 1
	c let i64 = 1
	d let mut i64 = 1

	abc func(a i32) {
	}

	closure := |a| {
	}

becomes

	let mut a = 1;
	let b = 1;
	let c i64 = 1;
	let d mut i64 = 1;

	fn abc(mut a: i32) {
	}

	let mut closure = |mut a| {
	};
```

- **use string instead of String, and add string literal for String**

	all "string" identifiers are translated to "String", because it's easier to type and strings are so frequently used

	if some library exports "string", then use "string--r" to refer to it

	"foo"s is translated to "foo".to_owned()

- **add u string**

```
example

	#[derive(Serialize, Deserialize)]
	Param struct {
		#[serde(default = u"default-page")]
		page i32
		#[serde(default = u"api..default-limit")]
		limit i32
	}

becomes

	#[derive(Serialize, Deserialize)]
	struct Param {
		#[serde(default = "default_page")]
		page: i32,
		#[serde(default = "api::default_limit")]
		limit: i32,
	}
```

- **#[derive(Debug)] for all structs and enums by default**

	add #[derive(Debug)] for all structs and enums, except for the case that they have outer attributes that is ***#[!derive(Debug)]***

	pros: no need to add that to them, and almost all structs need Debug

	cons: increased compilation time and binary size (but I think this is a good tradeoff)

```
example

	Has-debug struct {
	}

	#[!derive(Debug)]
	No-debug struct {
	}
```

- **use + instead of pub for exporting symbols**

	use + after symbol name. + to pub, no + to pub(crate) (in Rust, Associated items in a pub Trait are public by default;
	Enum variants in a pub enum are also public by default)

```
example

	Num struct(i32)
	Num+ struct(i32)
	Num+(self) struct(i32)
	Num+(super) struct(i32)
	Num+(crate) struct(i32)
	Num+(in crate..util) struct(i32)

	Num+ struct(+ i32)

	User+ struct {
		name+ string
		age   i32
	}
```

- **pub(crate) by default instead of pub(self) by default**

	having to write mod.rs files is very annoying.
	Instead, use Go's method, pub(crate) by default, and automatically generate mod.rs files.
	but you can add a mod.u file to customize visibility for items and write module level documentations

```
unmentioned items are like this

	#[path = "a.rs"]
	mod a___;
	pub use self::a___::*;

or if util is a directory

	pub mod util;
```

- **custom mod.u file**

if you customize the mod.u file, in order to prevent the above processing

just write ordinary u code
```
	a mod
	cfg,,[stmt] {
		a mod
	}
```
and if some mod names are within macros, use u-custom-mod
```
	u-custom-mod,,{a, b, c-d}
	cfg_rt,,{
		mod a;
		mod b;
		mod c-d;
	}
```

- **use unified for syntax instead of loop, while, for**

```
example

	for {
		if can-close {
			break
		}
	}

	for i < 10 {
		do-sth()
	}

	for item in list {
		println,,("{}", item.name)
	}

```

- **use new syntax for destructive binding(including if let, while let), but don't support destructive assignment**

	use Expr -> Pattern instead of let Pattern = Expr

	use if Expr -> Pattern { }, as I always believe that the order should be reversed

	if Expr is too long, you can use
	```
	a := Expr
	a -> Pattern
	```

	destructive assignment is not supported, because using a pattern as lvalue is very confusing,
	and destructive declaration is already sufficent to use

```
example

	get-point() -> (a, b)
	get-point() -> (a, b) let (i32, i32)

	if &token.code -> Token-code..Identifier(identifier) {
		println,,("{:?}", identifier)
	}

	for &token.code -> Token-code..Identifier(identifier) {
		println,,("{:?}", identifier)
	}
```

- **use , instead of && as separators between chains of conditions in if and while statements**
```
example

	// use , instead of && as separators between chains of conditions,
	// because && can be regarded as part of the expr
	if a == b, c && d, #[some-attr] outer -> Some(inner),
			inner -> Some(number),
			number == 1 {
	}
	if a == b, c -> Some(n), n > 0 {
	}

	for v -> E..X(n) | E..Y(n) {
	}
	for a -> Some(b) {
	}
	for a == b, c && d, #[some-attr] outer -> Some(inner),
			inner -> Some(number),
			number == 1 {
	}
	for a == b, c -> Some(n), n > 0 {
	}

```

- **allow semicolon(;) as seperators in addition to comma(,) in multi-line pairs**

```
example

	tuple = (a, b)
	tuple = (
		a
		b
	)
	f func(
		a
		b
	)
	f(
		a
		b
	)
	array := {{
		a
		b
	}}
	vec,,(
		a
		b
	)
	a := Person {
		name: "a"
		age: 20
	}
	match a {
		Person {
			name
			...
		}: {}
	}

	NOTE: tuple type must be
		a func() (i32, i32) {}
	or
		a func() (
			i32,
			i32
		) {}
	not
		a func() (
			i32
			i32
		) {}
```

- **use ,, for macro invocations**

	because it's easier to type(at least in my keyboard layout, where ! is located in "-" in QUERTY keyboard)

```
example

	custom,,()
		anything in () are treated as expressions. like
			vec,,(1, 2, 3)

	custom,,[stmt]{}
		anything in {} are treated as declarations or statements. like
			thread-local,,[stmt]{
				foo--g+ static Ref-cell[u32] = Ref-cell..new()
			}

	custom,,{}
		anything in {} are U tokens. like
			clap..arg,,{ {{name}} "Optional name" }
			clap..arg,,{ ~ c ~ ~ config [file--c] "Sets a file" }
		they become
			clap::arg!{ [name] "Optional name" }
			clap::arg!{ - c - - config < FILE > "Sets a file" }

		dev note(for myself)
			custom,,{} always translates to custom!{}, so there's no need to add a trailling semicolon
			even if it stands alone as a stmt. (as the Rust spec states,
				MacroInvocationSemi:
					SimplePath ! ( TokenTree* ) ;
					SimplePath ! [ TokenTree* ] ;
					SimplePath ! { TokenTree* }
			see https://doc.rust-lang.org/nightly/reference/macros.html#macro-invocation)

```

- **augment struct init expression**

```
example

the following two expressions are the same
	a{ name: "", ...Default..default() }
	a{ name: "", ... }
```

## Other Changes

- **drop colon and -> in declarations**

	exception: if you need to annotate closure expression's return type, then it has to be |n| -> i32 { < 1 }

- **use func, interface, import instead of fn, trait, use**

- **use ret instead of return**

- **use : instead of => in match**

	because it's easier to type

```
example

	match r {
		Ok(a): a
		Err(err): {
			do-sth(err)
		}
	}
```

- **drop leading r in raw string literal**

	use #" "#, or ##" "##, or ###" "###, etc.

- **use ... instead of .. as rest pattern**

- **unit test**

	add a test block. any functions in that block whose name starts with test- automatically
	have a #[test] attribute added
```
example

	test {
		test-add func() {
		}
	}

instead of

	#[cfg(test)]
	test mod {
		#[test]
		add func() {
		}
	}
```

- **patterns in function parameters**

	it's bad to have that in function parameters.
	if you really need that, add a destructuring assignment at the start of the function body

- **use d,,() for dbg!()**

	because it's so frequently used

- **caveat**

```
// special case: wrap the cast type when the type consists of keyword const
const-c-void[T] func(r &T) *const c-void {
	ret r as (*const T) as (*const c-void)
}

```

- **doc comment**

	write doc comment in u's format, and generate the same format doc in rust

	TODO: maybe generating rust doc in the future

- **add u-path string literal**

	adds "../" before the string literal

	because when you run `u run`, under the hood it runs `cd .u; cargo run`, so the static files are one level
	above relative to the rust files

	NOTE: the relative path of those static files referenced in source code must refer all the way to the project root,
	like u-path"../../crates/some-crate/some-file", instead of u-path"../some-file"

```
example

	read-to-str(u-path"some-dir/a")
	include-str,,(u-path"../../some-dir/a")

becomes

	read-to-str("../some-dir/a")
	include-str,,("../../../some-dir/a")

suppose there is a file some-dir/a

$ROOT/
	some-dir/
		a
	crates/
		$ROOT/
			src/
			main.u

```

### for more examples, check [a.u file](./data/a.u)


## Project Structure
```
$ROOT/
	.git/
	.gitignore(./u)

	u.lock
	u.toml

	crates/
		$ROOT/
			benches/
			bin/
			examples/
			src/
				a.u
				b.u
				sub/
					a.u
			tests/
			main.u
			u.toml

		crate-a/

	.u/
		Cargo.lock
		Cargo.toml

		crates/
			$ROOT/
				benches/
				bin/
					multiple/
						src/
							mod.rs
						main.rs
					single.rs
				examples/
				src/
					sub/
						a.rs
							#![allow(unused_mut)]
							use super::*;
							...
						mod.rs
							#![allow(unused_imports)]
							mod a;
							pub use self::a::*;
					a.rs
						#![allow(unused_mut)]
						use super::*;
						...
					b.rs
						#![allow(unused_mut)]
						use super::*;
						...
					mod.rs
						#[allow(unused_imports)]
						pub mod sub;

						mod a;
						pub use self::a::*;
						mod b;
						pub use self::b::*;
				tests/
				lib.rs
					mod src;
					pub use self::src::*;
				main.rs
					#[allow(unused-imports)]
					use ${pkg-name}::*;
				Cargo.toml

			crate-a/
```


## Installation
```
NOTE: only tested on Linux

install u
	git clone https://github.com/mhf-air/u.git

install vim-u

	Plug 'mhf-air/vim-u'

modify dense-analysis/ale
	mkdir ~/.vim/bundle/ale/ale_linters/u
	cp ~/rust/third/u/src/data/u.vim ~/.vim/bundle/ale/ale_linters/u/u.vim

install modified rust-analyzer

	implementation
		cd ../u (u/ and rust-analyzer/ must live in the same directory)
		git clone https://github.com/mhf-air/rust-analyzer.git
		cd rust-analyzer
		git remote add upstream https://github.com/rust-analyzer/rust-analyzer.git
		git checkout u
		cargo install --path crates/rust-analyzer
		(if this fails, try "cargo install --locked --path crates/rust-analyzer")

	sync from upstream
		git checkout master
		git pull upstream master
		git push

		git checkout u
		git merge master
		git push

modify .vimrc
	from my .vimrc, copy the part
		augroup u
		augroup END
	and
		function! Format(arg)
		endfunction
	to your .vimrc for .u file format on save and auto hide import

allow - in identifier for .u files
	edit ~/.vim/bundle/YouCompleteMe/third-party/ycmd/ycmd/identifier_utils.py

	add
		'u': re.compile(r"[a-zA-Z-][a-zA-Z0-9-]*", re.UNICODE),
	after
		'css': re.compile(r"-?[^\W\d][\w-]*", re.UNICODE),

```

## Mechanism
```
for a .u file, output a .rs file, without need to do cross file analysis and type analysis

write to a .u file
	automatically run `u u-compile ${file-path}` to output a .rs file.
	rewrite ./mod.rs (no need to rewrite ../mod.rs, because it's only
	affected when the directory is added, removed or renamed)

write to a u.toml file
	run `u u-sync ${u-toml-path}` to sync the Cargo.toml file
	in vim plugin "mhf-air/vim-u", there are already settings to make this happen automatically
		au BufWritePost */u.toml call u#ToCargoToml()

remove a .u file
add a directory
remove a directory
rename a directory
	might lead to "not found" error
	when that happens, manually edit a sibling .u file, it will rewrite the mod.rs file.
	or cd .u to find the corresponding file and work on it

create a new U project
	u new my-project

init a fresh U project
	cd my-project
	u init

other commands are the same as Cargo's, for example
	u run
	u build
	u install

if something goes wrong, and you don't know how to fix it
	u clean
	u init
	u run
```

## Naming Convention
[Rust naming convention](https://rust-lang.github.io/api-guidelines/naming.html)
|   item        |       U           |   Rust        |
| :-----------: | :---------------: | :-----------: |
| struct        | Order-item        | OrderItem     |
| enum          | Option            | Option        |
| interface     | Copy              | Copy          |
| function      | set-name          | set_name      |
| const         | const-name--c     | CONST_NAME    |
| static        | static-name--g    | STATIC_NAME   |

(the following should be rare, they are only used for third-party code that doesn't conform to Rust's naming conventions)
|   item        |       U           |   Rust        |
| :-----------: | :---------------: | :-----------: |
| other         | anyName--r        | anyName       |
| other         | anyName-tag--r    | anyName_tag   |
| other         | AnyName-tag--r    | AnyName_tag   |
| other         | Order--Item--r    | Order__Item   |
```
note
	reject UpperCamelCase names like Point-2d, but accept UpperCamelCase Point2d and snake_case move-2d

rule
	pub fn id_to_rust(id: &str) -> String {
		let mut p = String::new();
		if id.is_empty() {
			return "".to_string();
		}
		if id == "string" {
			return "String".to_string();
		}

		let chars: Vec<char> = id.chars().collect();
		let len = chars.len();

		if len > 2 {
			if chars[len - 3] == '-' && chars[len - 2] == '-' {
				match chars[len - 1] {
					'c' | 'g' => {
						if len == 3 {
							p.push_str("__");
							if chars[len - 1] == 'c' {
								p.push('c');
							} else {
								p.push('g');
							}
							return p;
						}
						let r = &chars[..len - 3];
						for c in r {
							if *c == '-' {
								p.push('_');
							} else {
								let a = c.to_uppercase().to_string();
								p.push_str(&a);
							}
						}
						return p;
					}
					'r' => {
						if len == 3 {
							return "__r".to_string();
						}
						let r = &chars[..len - 3];
						for c in r {
							if *c == '-' {
								p.push('_');
							} else {
								let a = c.to_string();
								p.push_str(&a);
							}
						}
						return p;
					}
					_ => {}
				}
			}
		}

		if chars[0].is_uppercase() {
			let list = chars.split(|c| *c == '-');
			for word in list {
				for (i, c) in word.iter().enumerate() {
					if i == 0 {
						p.push_str(&c.to_uppercase().to_string());
					} else {
						p.push_str(&c.to_string());
					}
				}
				if word.is_empty() {
					p.push('_');
				}
			}
			return p;
		}

		for c in &chars {
			if *c == '-' {
				p.push('_');
			} else {
				let a = c.to_string();
				p.push_str(&a);
			}
		}
		return p;
	}
```

## Compiler
```
	.u file
->  token list
		identifier containing - to identifier containing _
			- to "_"
			a-b to "a_b"

		~ to "-"
		~= to " -= "
		~~ to " -= 1"
		++ to " += 1"

		[ to "<" (if in expressions, to "::<")
		] to ">"
		{{ to "["
		}} to "]"
		`` to ".."
		``= to "..="

		.. to "::"
		,, to "!"
		.(Type) to "as Type"

		note:
			all these transformations are also done in macro invocations except for "[".
			so in macro invocations, one might need to use "..[" for ::<>
->  U's ast
->  .rs file
```

## Examples
```
declaration
	# --------------------------------------------------
	# visibility
	sub mod
	sub+ mod
	sub+(self) mod
	sub+(super) mod
	sub+(crate) mod
	sub+(in crate..a) mod

	Count struct(i64)
	Count+ struct(+ i64)

	->

	pub(crate) mod sub;
	pub mod sub;
	mod sub;
	pub(super) mod sub;
	pub(crate) mod sub;
	pub(in crate::a) mod sub;

	pub(crate) struct Count(pub(crate) i64)
	pub struct Count(pub i64)

	# --------------------------------------------------
	# built-in macros
	d,,(a)

	->
	dbg!(a)

	# --------------------------------------------------
	# minus
	add(~1, 1)
	num~~

	->

	add(-1, 1)
	num -= 1

	# --------------------------------------------------
	# struct expression
	Person{ name: "", ...b }
	Person{
		name: ""
		...b
	}

	->

	Person { name: "", ..b }
	Person {
		name: "",
		..b
	}

	# --------------------------------------------------
	# mod
	sub mod {
	}

	->

	mod sub {
	}

	# --------------------------------------------------
	# extern crate
	# (as of rust 2024, this is rarely used)
	crate {
		std
		ruststd std
		- foo
	}

	->

	extern crate std;
	extern crate std as ruststd;
	extern crate foo as _;

	# --------------------------------------------------
	# import
	import {
		+ std
		* std..env
		+ * std..env
		env-consts std..env..consts
		std..option..Option{self, Some, nil None}
		std..option{Option, Option..Some, nil Option..None}
	}

	->

	pub use std;
	use std::env::*;
	pub use std::env::*;
	use std::env::consts as env-consts;
	use std::option::Option::{self, Some, None as nil};
	use std::option::{Option, Option::Some, Option::None as nil};

	# --------------------------------------------------
	# mod
	sub+ mod {
	}

	another mod {
	}

	->

	pub mod sub {
	}

	pub(super) mod another {
	}

	# --------------------------------------------------
	# extern
	extern "C" {
		abs func(input i32) i32
	}
	#[no-mangle]
	call-from-c func[extern "C"]() {
	}

	->

	extern "C" {
		fn abs(input: i32) -> i32;
	}
	#[no-mangle]
	extern "C" fn call_from_c() {
	}

	# --------------------------------------------------
	# unsafe
	unsafe {
		dangerous()
	}
	dangerous func[unsafe]() {
	}

	->

	unsafe {
		dangerous();
	}
	unsafe fn dangerous() {
	}

	# --------------------------------------------------
	# func
	foo func[const](a i32, b i32) {
	}
	foo[T] func[async, unsafe]() i32 where {T Clone} {
	}
	foo[T] func() where {T Clone} {
	}

	->

	const fn foo(mut a i32, mut b i32) {
	}
	unsafe async fn foo[T]() i32 where T: Clone {
	}
	fn foo[T]() i32 where T: Clone {
	}

	# --------------------------------------------------
	# type alias

	Point[T] type where {T Clone} = (T, T)

	->

	type Point<T> where T: Clone = (T, T)

	# --------------------------------------------------
	# struct
	Person['a] struct where {} {
		name+   &'a str
		age     i32
		address string
	}

	Pair[T, U] struct(+ T, U) where {}

	->

	struct Person<'a> where {
		pub         name:       &'a str,
		pub(crate)  age:        i32,
		pub(crate)  address:    String,
	}

	struct Point<T, U>(pub T, pub(crate) U) where ;

	# --------------------------------------------------
	# enum
	#[repr(u8)]
	Color enum {
		Red = 1
		Green
		Blue
	}
	Color impl {
		red-2--c const Color = Color..Red
	}

	Animal[T, E] enum where {} {
		Dog(string, f64)
		Cat {
			name    string
			weight  f64
		}
	}

	->

	#[repr(u8)]
	enum Color {
		Red = 1,
		Green,
		Blue,
	}
	impl Color {
		const RED_2: Color = Color::red;
	}

	enum Animal[T, E] where {
		Dog(String, f64),
		Cat {
			name:   String,
			weight: f64,
		},
	}

	# --------------------------------------------------
	# union
	My-union[T] union where {} {
		name+   string
		age     i32
	}

	->

	union MyUnion[T] where {
		pub         name:   String,
		pub(crate)  age:    i32,
	}

	# --------------------------------------------------
	# const, static
	name--c const &str = "hello"
	num--g static mut i32 = 0
	num--g static i32 = 0

	->

	const NAME: &str = "hello";
	static mut NUM: i32 = 0;
	static NUM: i32 = 0;

	# --------------------------------------------------
	# interface
	Seq[T Copy = Self] interface[unsafe] Display + Debug where {} {
		name--c const &str = "hello"
		Item type = ()

		foo func() where { Self Sized } {
		}

		len (&) func() u32
		elt-at (&) func(n u32) T
		iter[F] (&) func(f F) where { F Fn(T) }
	}

	->

	unsafe trait Seq<T: Copy = Self> : Display + Debug where {
		const NAME: &str = "hello";
		type Item = ()

		fn foo() where Self: Sized {
		}

		fn len(&self) -> u32;
		fn elt_at(&self, n: u32) -> T;
		fn iter<F>(&self, f: F) where F: Fn(T);
	}

	# --------------------------------------------------
	# impl(method, associated items, trait)
	Person impl {
		test+ (&mut) func(a &str) {}
	}

	Point[f32] impl {
		distance (&) func() f32 {}
	}

	Person impl Error {}

	(T) [T Copy] impl[unsafe] My-trait {}

	Point[T, U] [T, U] impl[unsafe] ops..My-trait where {} {
		Output type = usize

		new func()

		mixup[V, W] (&) func[unsafe, async](other Point[V, W]) Point[T, W]
		mixup[V, W] (&mut) func[unsafe, async](other Point[V, W]) Point[T, W]
		mixup[V, W] (Self) func[unsafe, async](other Point[V, W]) Point[T, W]
		mixup[V, W] (Box[Self]) func[unsafe, async](other Point[V, W]) Point[T, W]
		mixup[V, W] (Pin[Box[Self]]) func[unsafe, async](other Point[V, W]) Point[T, W]
	}

	->

	impl Person {
		pub fn test(&self, mut a: &str) {}
	}

	impl Point<f32> {
		fn distance(&self) -> f32 {}
	}

	impl Error for Person {}

	unsafe impl<T: Copy> MyTrait for T {}

	unsafe impl<T, U> ops..MyTrait<T> for Point<T, U> where {
		type Output = usize
		fn new() {}
		unsafe async fn mixup<V, W>(self, other: Point<V, W>) -> Point(T, W)
	}

	# rule
	receiver type
		&(&self), &mut(&mut self), &'a(&'a self), &'a mut(&'a mut self)
		Box[Self], Rc[Self], Arc[Self],
		Pin[Box[Self]],

		[Type-a, Type-b, Type-c](self: Type-a[Type-b[Type-c[Self]]])
		Type could be
			Box, Rc, Arc
			Pin

	# --------------------------------------------------
	# where clause

	where { T Copy }
	where {
		T           Iterator
		T..Item     Copy
		string      PartialEq[T]
		(func(T))   Iterator
	}

	->

	where T: Copy
	where
		T:          Iterator,
		T::Item:    Copy,
		String:     PartialEq<T>,
		(fn(T)):    Iterator,

	# --------------------------------------------------
	# unit test, benchmark
	test {
		test-it-works func() {
		}
	}

	->

	#[cfg(test)]
	mod __ {
		use super::*;

		#[test]
		fn test_it_works() {
		}
	}

	# best practice
	for unit test, just put the test block in the same file as the source code.
		pros
			easy to edit test
		cons

	# note
	each file can have only one test block.
	run `cargo test --lib` to only run unit tests, avoiding integration tests.

	# --------------------------------------------------
	# macro
	vec macro {
		( $( $x:expr ),* ) => {
			{
				let mut temp-vec = Vec..new();
				$(
					temp-vec.push($x);
				)*
				temp-vec
			}
		};
	}

	->

	#[macro_export]
	macro-rules! vec {
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

	# --------------------------------------------------
	# doc comment
	/** title
	desc `set-name`

	```
	a := 1
	```
	*/

	->

	/** title
	desc `set_name`

	```u
	a := 1
	```
	*/



statement and expression
	# --------------------------------------------------
	# variable declaration
	a := &point{}
	a let i32 = 1
	a let mut = 1
	a let mut i32 = 1

	get-point() -> (a, b)
	get-point() -> (a, b) let (i32, i32)

	some-var -> Some(a) else {
		ret
	}

	->

	let mut a = &point{};
	let a: i32 = 1;
	let mut a = 1;
	let mut a: i32 = 1;

	let (a, b) = get_point();
	let (a, b): (i32, i32) = get_point();

	let Some(a) = some-var else {
		return;
	};

	# --------------------------------------------------
	# array
	{{i32; 3}}
	list{{0}}
	list{{0``3}}
	list{{0``=3}}
	&{{T}}

	->

	[i32; 3]
	list[0]
	list[0..3]
	list[0..=3]
	&[T]

	# --------------------------------------------------
	# if let, while let
	if &token.code -> Token-code..Identifier(identifier) {
	}

	for &token.code -> Token-code..Identifier(identifier) {
	}

	->

	if let TokenCode::Identifier(identifier) = &token.code {
	}

	while let TokenCode::Identifier(identifier) = &token.code {
	}

	# --------------------------------------------------
	# match
	match message {
		Message..Quit: println,,("Quit")
		Message..Write-string(write): println,,("{}", &write)
		Message..Move{ x, y: 0 }: println,,("move {} horizontally", x)
		Message..Move{ ... }: println,,("other move")
		Message..Change-color{ 0: red, 1: green, 2: - }:
			println,,("color change, red: {}, green: {}", red, green)
		-:
	}

	->

	match message {
		Message::Quit => {
			println!("Quit"),
		}
		Message::WriteString(write) => {
			println!("{}", &write),
		}
		Message::Move{ x, y: 0 } => {
			println!("move {} horizontally", x),
		}
		Message::Move{ .. } => {
			println!("other move"),
		}
		Message::ChangeColor { 0: red, 1: green, 2: _ } => {
			println!("color change, red: {}, green: {}", red, green);
		}
	}
```


## NOTE
```
- some useless clippy lints
	first type "cargo" or "u", then copy the following
		clippy -- \
		-A clippy::field_reassign_with_default \
		-A clippy::needless_range_loop \
		-A clippy::comparison_chain \
		-A clippy::needless_return \
		-A clippy::collapsible_else_if \
		-A clippy::collapsible_if

```


## TODO

- after writing "[Type] impl [Interface] {\n}", automatically query rust-analyzer to
  generate all associated items for the interface
- at present, goto-definition goes to .rs file, maybe I can map it back to .u file if it exists

- Cargo configuration for README.md and other static files

- a new u-analyzer: a wrapper around rust-analyzer, being both a server and a client

- use u-id"default-page" instead of u"default-page"

## Fantasy

- What a Rust with GC is like?
	- prior art: Go

- What an interpreted scripting Rust is like?
	- prior art: Lua, JavaScript
