# Another syntax for Rust

## Summary
A new syntax that maps to Rust's syntax one by one, which is easier to read and write

## Motivation
I like Rust's semantics, but dislike some of its syntax

## Major Changes

- **allow dash(-) in identifiers**

    change snake_case, UpperCamelCase, SCREAMING_SNAKE_CASE to **dash-case** because it's the best way to separate words

    - Upper-camel-case for UpperCamelCase
    - set-name for set_name
    - name--c for NAME(which is a const or static)
    - anYThInGcUsTom--r for anYThInGcUsTom

    as for identifiers containing digit, forbid '-' followed by a digit, and consider digits as lower-case letters.

    for example, Point2d, Mat4x4 are fine, but Point-2d is not.

    side effect: all minus related operators have to be changed with ~.

    possible future conflict: when ~ is used in Rust

    this needs some adaptation, but the good news is that these operators are not frequently used

```
example

    Order-item, order-name, dir-path--c

and minus becomes
    ~1, ~~
```

- **use [ ] for generics**

    use [T] because [ ] are easier to type(no Shift)

    side effect: have to use {{ }} for array related places to avoid parsing ambiguity.
    It's worth it, because generics are used more often than arrays

    so turbofish is unneccesary, since [ ] are solely used for generics, all the [ ] in expressions can be transformed to turbofish in Rust

    possible future conflict: when {{ }} is used in Rust

```
example

    User struct {
        desc       Option[string]
        tags       Vec[string]
        an-array   {{i32: 10}}
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

    Type :[Generics]? impl Trait? where? {}

```
example

    dir-path--c const &str = "/tmp"
    server--c static Option[Server] = None

    User+['a] struct {
        name+   &'a str
        age     i32
    }

    private-func+ func(id string, count i32) i32 {
        ret 0
    }

    Point[T, U] :[T, U] impl[unsafe] ops..My-trait where {T Copy + Default} {
        Output type = usize

        new func()

        mixup[V, W] (&) func[unsafe, async](other Point[V, W]) Point[T, W]
        mixup[V, W] (&mut) func[unsafe, async](other Point[V, W]) Point[T, W]
        mixup[V, W] () func[unsafe, async](other Point[V, W]) Point[T, W]
        mixup[V, W] (Box[Self]) func[unsafe, async](other Point[V, W]) Point[T, W]
        mixup[V, W] (Pin[Box[Self]]) func[unsafe, async](other Point[V, W]) Point[T, W]
    }
```

- **move method receiver to a different place, and use s as self**

```
    implementation: add
        let mut s = self;
    or
        let s = self;
    at the start of method body, except when there's
        $"no-s"
    before method's signature

    pros: it's clearer and shorter

example

    User impl {
        $"no-s"
        get-age+ (&) func i32 {
            < self.age
        }

        get-age+ (&) func i32 {
            < s.age
        }

        set-age (&mut) func(new-age i32) {
            s.age = new-age
        }

        move () func() {
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

    a := some-func()
        .some-method()
        .b
        .c
```

- **use .. instead of :: for path separator**

    because it's easier to type and distinguishes path segments better

    side effect: have to use `` for Rust's .. operator, etc.

    possible future conflict: when `` is used in Rust

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

- **new syntax for leading :: of a path**

    use -..crate-name..abc, which translates to ::crate_name::abc

- **add another syntax for let

    possible future conflict: when := is used in Rust

```
example

    a :=;
    a i32 :=;
    a := 1
    a i32 := 1

    a mut :=;
    a mut i32 :=;
    a mut := 1
    a mut i32 := 1

    a fn(i32, ...) :=;

becomes

    let a;
    let a: i32;
    let a = 1;
    let a: i32 = 1;

    let mut a;
    let mut a: i32;
    let mut a = 1;
    let mut a: i32 = 1;

    let a: fn(i32, ...);

```

- **#[derive(Debug)] for all structs and enums by default**

    add #[derive(Debug)] for all structs and enums,
    except when they have outer mark that is ***$"no-derive-debug"***

    pros: no need to add that to them, and almost all structs need Debug

    cons: increased compilation time and binary size (but I think this is a good trade-off)

```
example

    Has-debug struct {
    }

    $"no-derive-debug"
    No-debug struct {
    }
```

- **use + instead of pub for exporting symbols**

    use + or ^ after symbol name
    + to pub, ^ to pub(crate)

```
example

    Num struct(i32)
    Num+ struct(i32)
    Num+(self) struct(i32)
    Num+(super) struct(i32)
    Num+(crate) struct(i32)
    Num+(in crate..util) struct(i32)

    Num+ struct(+ i32)

    User^ struct {
        name^ string
        age   i32
    }
```

- **use unified for syntax instead of loop, while, for**

    but for struct patterns(because it uses {}), use "for: PATTERN in EXPR {}"

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

    // for struct patterns, use for:
    for: Order { id } in list {}

```

- **use new syntax for destructive binding(including if let, while let)**

    use Expr -> Pattern instead of let Pattern = Expr

    use if Expr -> Pattern { }, as I always believe that the order should be reversed

    possible future conflict: when -> is used in Rust as part of an expression

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

    // if Expr is too long, you can use
    a := Expr
    a -> Pattern
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

- **allow semicolon(;) as separators in addition to comma(,) in multi-line pairs**

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

    possible future conflict: when ,, is used in Rust

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

    exception: if you need to annotate closure expression's return type,
    then it has to be |n| -> i32 { < 1 },
    because you can have |n| n

- **use func, interface, import instead of fn, trait, use**

- **new syntax for bare function type and closure type**

    use : here instead of ->, because Type might appear before :=, which will
    mistake a := statement into a -> statement

```
example

    fn()
    fn(i32, i32)
    fn(i32, i32): T

    dyn Fn-once(): T

    a(|:mut n: i32|: i32 {
        n++
        < n
    })
```

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

- **use ... instead of .. as rest pattern**

- **patterns in function parameters**

```
f func(:Pattern: Type) {}

example

    f func(:mut a: i32, b i32) {}
```

- **doc comment**

    write doc comment in u's format, and generate the same format doc in rust

    TODO: maybe generating rust doc in the future

### caveats
- vim-u offers the following to add something after the first symbol in the current line,
you can use '.' to repeat
    - <leader>+ to add "+"
    - <leader>^ to add "^"
    - <leader>am to add " mut"

### for more examples, check [a.u file](./data/a.u)

## Project Structure (NOTE: this part is deprecated now!)
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
    cd ~/.vim/bundle/ale/ale_linters
    mkdir u
    cp ~/a/rust/third/rust/u/data/u.vim .

install modified rust-analyzer

    implementation
        cd ../u (u/ and rust-analyzer/ must live in the same directory)
        git clone https://github.com/mhf-air/rust-analyzer.git
        cd rust-analyzer
        git remote add upstream https://github.com/rust-analyzer/rust-analyzer.git
        git checkout u
        ./u-install.sh

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

## Naming Convention
[Rust naming convention](https://rust-lang.github.io/api-guidelines/naming.html)
|   item        |       U           |   Rust        |
| :-----------: | :---------------: | :-----------: |
| struct        | Order-item        | OrderItem     |
| enum          | Option            | Option        |
| interface     | Copy              | Copy          |
| function      | set-name          | set_name      |
| const         | const-name--c     | CONST_NAME    |
| static        | static-name--c    | STATIC_NAME   |

(the following should be rare, they are only used for third-party code that doesn't conform to Rust's naming conventions)
|   item        |       U           |   Rust        |
| :-----------: | :---------------: | :-----------: |
| other         | anyName--r        | anyName       |
| other         | anyName-tag--r    | anyName_tag   |
| other         | AnyName-tag--r    | AnyName_tag   |
| other         | Order--Item--r    | Order__Item   |
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

- a new u-analyzer: a wrapper around rust-analyzer, being both a server and a client

## Fantasy

- What a Rust with GC is like?
    - prior art: Go

- What an interpreted scripting Rust is like?
    - prior art: Lua, JavaScript
    - answer: WASM
