
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
