# Another syntax for Rust

## Summary
A new syntax that maps to Rust's syntax one by one, which is easier to read and write

## Motivation
I like Rust's semantics, but dislike some of its syntax

## Major Changes

- **allow dash(-) in identifiers**

    change snake_case, UpperCamelCase, SCREAMING_SNAKE_CASE to **dash-case**,
    because it's the best way to separate words

    - Upper-camel-case for UpperCamelCase
    - set-name for set_name
    - name--c for NAME(which is a const or static)
    - anYThInGcUsTom--r for anYThInGcUsTom
    - leading '-' are preserved, so -Order-item will be _OrderItem
    - forbid aA and AA, use aA--r and aa--c instead, but A is allowed
    - forbid A--b and A-1, use A-b and A1 instead

    as for identifiers containing digit, consider digits as lower-case letters.

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

    side effect: have to use [` `] for array related places to avoid parsing ambiguity.
    It's worth it, because generics are used more often than arrays

    so turbofish is unneccesary, since [ ] are solely used for generics,
    all the [ ] in expressions can be transformed to turbofish in Rust

    possible future conflict: when [` `] is used in Rust

```
example

    User struct {
        tags       Vec[String]
        an-array   [`i32; 10`]
    }

and array becomes
    a[`0`] = 1

    [`u32; 1`]

    [`0; 10`]
    [`0, 1`]

turbofish becomes
    list.iter().collect[Vec[-]]()

```

- **for all declarations, put the symbol name first**

    it fits my thought process. Whenever I think of something,
    the name is the first thing that pops up, and it's the most important,
    all other stuff are complimentary information

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

    // have to use ',' after every WhereItem if it's on a separate line
    User[T] struct where
        T: Clone,
    {
    }

    impl[unsafe][T, U] ops..My-trait for Point[T, U] where { T: Copy + Default } {
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
        func[no-s]()
    in method's signature

    pros: it's clearer and shorter

example

    User impl {
        get-age+ (&) func[no-s]() i32 {
            self.age\
        }

        get-age+ (&'a) func() i32 {
            s.age\
        }

        set-age (&mut) func(new-age i32) {
            s.age = new-age
        }

        move () func() {
            s.age = 0
        }
    }
```

- **drop trailling semicolon, colon and ->**

    use Go's method, automatically insert semicolons where possible
    (if the last token in a line is identifiers, most operators, ...)

    - when to manually add semicolon
        - a :=;
        - a [`i32; 3`] := [`1; 3`]
        - vec_[1; 3]

    note: semicolons are also inserted after line-ending . or .. (for auto-completion)

    note: write semicolons in macro definitions, as in regular Rust code

    side effect: use "Expr\" as the result expression.

```
example

    a := if b > 0 { b } else { -b }
    a := if b > 0 {
        _ b
    } else {
        _ ~b
    }

    // if the starting token at the next line is '.' or "as",
    // drop the auto-added ; in the last line
    a := some-func()
        .some-method()
        .b
        .c

    use std..env..consts
        as env-consts

    // allow '{' in a separate line in if, for, match
    if a
    {
    }

    for a
    {
    }

    match a
    {
    }

    // allow this
    a
    ->
    mut a

    a -> ?a
    else {
    }

    if a
    {
    }
    else
    {
    }

    if a
        && b -> c
        && d
    {
    }

    for a
        && b -> c
        && d
    {
    }

    match code {
        T_{+}
        | T_{:}
        | T_{->} : {
        }
        -: {}
    }
```

- **have to use some new syntax because of dropping semicolon**

```
- have to use "_ Expr" as value expression

    f func() i32 {
        if condition { _ true } else { _ false }

        a = condition {
            _ true
        } else {
            _ false
        }

        _ 1
    }

    design decision:
        because I want to use
        {
            c = a
                    && b
        }
        note that "a" is not followed by an auto-added ';', so without this syntax,
        it becomes
            c = a && b

        for the following case, when I do want to have two statements,
        add ';' at the end of the last statement
        {
            c = a;
            && b
            _ c
        }

        there's no cons for this design, because otherwise I have to use
            c\
        anyway

- have to add ';' at the end of the last statement,
    when the starting character of a statement is:

    ~
    &
    |
    &&
    ||

    these operators can both be between two expressions and start an expression.

    because these statments are rare beside as the value expression of a block expression,
    in which case you have to use "_ Expr", so there's no problem.

```
another case is
    &Expr -> Pattern

this may be more commonly used, though I don't.
right now when writing it this way, it will throw an error, then you have to append ';' on
the last statement.
it's not perfect, but I don't a better approach yet.

```

- have to use '*^' as dereference operator

    '*' is also such an operator, but dereference is commonly used.
    I don't want to remember to add ';' before "*a".
    so I change the dereference expression to use a different operator: "*^".
    so now you use
        *^a = b
    instead of
        *a = b

```

- **use .. instead of :: for path separator**

    because it's easier to type and distinguishes path segments better

    side effect: have to use ,, for Rust's .. operator, etc.

    possible future conflict: when ,, is used in Rust

    note: use new syntax for leading :: of a path

    use -..crate-name..abc, which translates to ::crate_name::abc

```
example

    use std..io
    use std..fs

    compile-file func(filepath &str) io..Result[Parse] {
        data := fs..read-to-string(&filepath)?
        compile-string(data)
    }

    for i in 0,,10 {
    }
```

- **add another syntax for let

    possible future conflict: when := is used in Rust

```
example

    a := 1
    a i32 := 1

    a mut := 1
    a mut i32 := 1

    let a
    let a: i32
    let mut a
    let mut a: i32
    let (a, b, c)
    let a: fn(i32, ...)

becomes

    let a = 1;
    let a: i32 = 1;

    let mut a = 1;
    let mut a: i32 = 1;

    let a;
    let a: i32;
    let mut a;
    let mut a: i32;
    let (a, b, c);
    let a: fn(i32, ...);

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
        println_("{:?}", identifier)
    }

    for &token.code -> Token-code..Identifier(identifier) {
        println_("{:?}", identifier)
    }

    // if Expr is too long, you can use
    a := Expr
    a -> Pattern
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

- **use new syntax for Struct Pattern in if, for and match**

```
example

    if a -> b {
    }

    // if pattern contains Struct Pattern, have to use : before pattern.
    // see there's a space between : and pattern, because : is not part of the pattern,
    // it's for the compiler
    if a ->: Person{ name } {
    }
    match s.p {
        ?p if p ->: Person{ name }: {}
        -: {}
    }
    for a ->: Person{ name } {
    }
    for: Person{ name } in list {
    }

    // if condition contains {}, then have to use "for while"
    // but that is very rare
    for while { true } {
    }
```

- **use unified for syntax instead of loop, while, for**

    but for struct patterns in for-in(because it uses {}), use "for: PATTERN in EXPR {}"

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
        println_("{}", item.name)
    }

    // for struct patterns, use for:
    for: Order { id } in list {}

```

- **allow semicolon(;) as separators in addition to comma(,) in multi-line pairs**

    - struct, enum, union declaration

```
example

    use std..option..{
        Option
        Option..Some
        Option..None as nil
    }
    use {
        crate..a
        crate..a..*
        super..a
    }

    Person struct {
        name String
        age i32
        desc String
    }

    Code enum {
        Literal(Literal)
        Identifier {
            id String
            u String
        }
        None
    }

    Code union {
        id String
        u String
    }

```

- **use _ for macro invocations**

    because it's easier to type
    (at least in my keyboard layout, where ! is located in "-" in QUERTY keyboard)

    and easier to read(I find ! very disturbing if not used for exclamation)

    write pure Rust in macro definitions and macro invocations that are random tokens.
    except identifiers and "~" and "~=", and use '_' not '-'.

    note: don't support custom_"stmt"{}, because such case is rare, and it's complicated
    to implement

```
example

    custom_()
    custom_[]
        anything in () or [] are treated as expressions. like
            vec_(1, 2, 3)

        custom_"{}"()
        custom_"{}"[]
            these are for Rust code that have parameters as expressions, but use {} as delimiters

    custom_{}
        anything in {} are U tokens. like
            clap..arg_{ [name] "Optional name" }
            clap..arg_{ ~ c ~ ~ config [file--c] "Sets a file" }
        they become
            clap::arg!{ [name] "Optional name" }
            clap::arg!{ - c - - config < FILE > "Sets a file" }

        custom_"()"{}
        custom_"[]"{}
            these are for Rust code that have parameters as expressions, but use () or [] as delimiters

```

- **use ?Type for Option<Type>**

    and use ?Pat for Some(Pat), because they are so frequently used.
    It's both easier to read and write

    don't add ?Expr for Some(Expr), because it's confusing having something like ?a()?,
    instead of Some(a()?)

```
example

    Person struct {
        name String
        tag ?String
    }
    print-tag func(p &Person) {
        if p.tag -> ?tag {
            println_("{}", tag)
        }
    }
```

- **use another syntax for creating String from string literal**

```
example

    ""s  -> "".to_owned()
    ""st -> "".to_string()
```

- **use another syntax for eprintln!()**

    to facilitate fast debugging by print()

```
example

    // eprintln!("{:?}", a)
    $a

    // eprintln!("{:?}", a.b)
    $a.b

    // eprintln!("{}: {:?}", "a.b is", a.b)
    $["a.b is"]a.b
```

- **augment struct init expression**

```
example

the following two expressions are the same
    a{ name: "", ,,Default..default() }
    a{ name: "", ,, }
```

## Other Changes

- **use func, ret instead of fn, return**

- **new syntax for bare function type and closure type**

    use => instead of ->, because Type might appear before :=, which will
    mistake a := statement into a -> statement

```
example

    fn()
    fn(i32, i32)
    fn(i32, i32): T

    dyn Fn-once(): T

    a(|:mut n: i32|: i32 {
        n++
        _ n
    })
```

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

- **use ,, instead of .. as rest pattern**

- **patterns in function parameters**

```
f func(:Pattern: Type) {}

example

    f func(:mut a: i32, b i32) {}
```

- **add ++ and ~~**

```
example

    a++
translates to
    a += 1;

    a~~
translates to
    a -= 1;
```

### caveats
- vim-u offers the following to add something after the first symbol in the current line,
you can use '.' to repeat
    - <leader>+ to add "+"
    - <leader>^ to add "^"
    - <leader>am to add " mut"

### for more examples, check [a.u file](./data/a.u)

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
| trait         | Copy              | Copy          |
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

- after writing "impl [Trait] for [Type] {\n}", automatically query rust-analyzer to
  generate all associated items for the trait

- a new u-analyzer: a wrapper around rust-analyzer, being both a server and a client

## Fantasy

- What a Rust with GC is like?
    - prior art: Go

- What an interpreted scripting Rust is like?
    - prior art: Lua, JavaScript
    - answer: WASM
