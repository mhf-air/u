use super::*;
use crate::compile as cc;
use std::collections::HashSet;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::io;
use std::mem;
use std::path::{Path, PathBuf};
use std::process;

macro_rules! io_func {
    ($call:expr, $ctx:expr) => {
        match $call {
            Ok(a) => a,
            Err(err) => {
                let ctx = $ctx();
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("{}: {}. (from {}: {})", err, ctx, file!(), line!()),
                ));
            }
        }
    };
}

// ----------------------------------------------------------------------
pub const U_LOCAL_ROOT: &str = ".u";

pub struct Config {
    cmd: Cmd,
}

impl Config {
    pub fn new(mut args: Vec<String>) -> Config {
        let cmd = args.remove(0);
        if args.is_empty() {
            return Config {
                cmd: Cmd::None(CmdNone { cmd }),
            };
        }

        match args[0].as_str() {
            "u-compile" => {
                args.remove(0);
                Config {
                    cmd: Cmd::Compile(CmdCompile::new(args)),
                }
            }
            "u-sync" => {
                args.remove(0);
                Config {
                    cmd: Cmd::Sync(CmdSync::new(args)),
                }
            }
            "u-fmt" => {
                args.remove(0);
                Config {
                    cmd: Cmd::Fmt(CmdFmt::new(args)),
                }
            }

            _ => {
                let r = CargoCmd::new(args);
                Config { cmd: Cmd::Cargo(r) }
            }
        }
    }

    pub fn run(&self) -> io::Result<()> {
        match &self.cmd {
            Cmd::None(a) => a.run(),
            Cmd::Compile(a) => a.run(),
            Cmd::Sync(a) => a.run(),
            Cmd::Fmt(a) => a.run(),

            Cmd::Cargo(a) => a.run(),
        }
    }
}

enum Cmd {
    None(CmdNone),
    Compile(CmdCompile),
    Sync(CmdSync),
    Fmt(CmdFmt),

    Cargo(CargoCmd),
}

struct CmdNone {
    cmd: String,
}
impl CmdNone {
    fn run(&self) -> io::Result<()> {
        let s = self;
        if s.cmd.as_str() == "target/debug/u" {
            /* CmdCompile {
                src: "/home/mhf/a/rust/wiki/u/abc/main.u".to_string(),
            }
            .run()?;
            Ok(()) */

            // CmdFmt::new(vec!["data/f.u".to_string()]).run()?;

            let filepath = "data/a.u";
            let buf = compile_to_rust(filepath, false).unwrap();
            println!("{}", buf);
            Ok(())
        } else {
            CmdHelp {}.run()
        }
    }
}

// u u-compile /some-path/a.u
pub struct CmdCompile {
    src: String,
}
impl CmdCompile {
    pub fn new(mut args: Vec<String>) -> CmdCompile {
        if args.is_empty() {
            return CmdCompile {
                src: "".to_string(),
            };
        }

        let src = mem::take(&mut args[0]);
        CmdCompile { src }
    }

    fn run(&self) -> io::Result<()> {
        let s = self;
        if s.src.is_empty() {
            println!(
                "usage: u u-compile <file-path>
"
            );
            return Ok(());
        }

        s.to_rust()?;

        Ok(())
    }

    fn to_rust(&self) -> io::Result<()> {
        let s = self;

        let (rs_path, is_main) = s.get_rs_path()?;

        let dir = rs_path.parent().unwrap();
        io_func!(fs::create_dir_all(dir), || dir.to_string_lossy());

        if rs_path.file_name().unwrap() == "mod.rs" {
            // detect lex errors
            let r = compile_to_rust(&s.src, is_main);
            if let Err(err) = r {
                println!("{}", err);
                return Ok(());
            }
            s.set_mod_rs(dir, false)?;
            return Ok(());
        }

        let r = compile_to_rust(&s.src, is_main);
        if let Err(err) = r {
            println!("{}", err);
            return Ok(());
        }
        let buf = r.ok().unwrap();
        io_func!(fs::write(&rs_path, buf), || rs_path.to_string_lossy());

        // must run this after writing the file, otherwise the file may not exist yet
        if !is_main {
            s.set_mod_rs(dir, false)?;
        }

        Ok(())
    }

    /*
       when write to $ROOT/crates/$CRATE/some-path/some-name.u,
       also rewrite $ROOT/.u/crates/$CRATE/some-path/mod.rs
       if some-name.u's ancestors don't have mod.rs files, todo
    */
    fn set_mod_rs(&self, rs_dir: &Path, from_dir: bool) -> io::Result<()> {
        let s = self;

        // find all items in ../${src}
        let mut m = HashSet::<String>::new();
        let p = io_func!(fs::canonicalize(PathBuf::from(&s.src)), || &s.src);
        let p = if from_dir { &p } else { p.parent().unwrap() };
        let u_entries = io_func!(fs::read_dir(p), || p.to_string_lossy());
        for item in u_entries {
            let path = item?.path();
            let mut name = path.file_stem().unwrap().to_string_lossy().to_string();

            if path.is_dir() {
                let is_empty = io_func!(fs::read_dir(&path), || path.to_string_lossy())
                    .next()
                    .is_none();
                if is_empty {
                    continue;
                }
            } else {
                name.push_str(".rs");
            }
            m.insert(name);
        }

        #[derive(Default)]
        struct FileItem {
            name: String,
            is_dir: bool,
            has_hyphen: bool,
            is_custom: bool,
        }

        // collect sibling files in rs_dir
        let mut file_items = Vec::new();
        let rust_entries = io_func!(fs::read_dir(rs_dir), || rs_dir.to_string_lossy());
        for item in rust_entries {
            let path = item?.path();
            let name = path.file_stem().unwrap().to_string_lossy().to_string();
            let full_name = path.file_name().unwrap().to_string_lossy().to_string();

            if name == "mod" {
                continue;
            }
            if !m.contains(&full_name) {
                if path.is_dir() {
                    io_func!(fs::remove_dir_all(&path), || path.to_string_lossy());
                } else {
                    io_func!(fs::remove_file(&path), || path.to_string_lossy());
                }
                continue;
            }

            let mut file_item = FileItem::default();
            if path.is_dir() {
                if path.join("mod.rs").exists() {
                    if name.contains('-') {
                        file_item.has_hyphen = true;
                    }
                    file_item.name = name;
                    file_item.is_dir = true;
                }
            } else {
                if name.contains('-') {
                    file_item.has_hyphen = true;
                }
                file_item.name = name;
                file_item.is_dir = false;
            }
            file_items.push(file_item);
        }

        // check ./mod.u file
        let mod_u = p.join("mod.u");
        let mut buf = if mod_u.is_file() {
            compile_mod_u(mod_u.as_os_str().to_str().unwrap(), &mut file_items)?
        } else {
            String::from("#![allow(unused_imports)]\n")
        };

        for item in file_items.iter_mut() {
            if item.is_custom {
                continue;
            }
            if item.is_dir {
                if item.has_hyphen {
                    buf.push_str(&format!("#[path = \"{}/mod.rs\"]\n", item.name));
                    item.name = item.name.replace("-", "_");
                }
                buf.push_str(&format!("pub mod {};\n", item.name));
            } else {
                buf.push_str(&format!("#[path = \"{}.rs\"]\n", item.name));
                if item.has_hyphen {
                    item.name = item.name.replace("-", "_");
                }
                buf.push_str(&format!("mod {}___;\n", item.name));
                buf.push_str(&format!("pub use self::{}___::*;\n", item.name));
            }
        }

        // write to destination
        let mut mod_rs = rs_dir.to_path_buf();
        mod_rs.push("mod.rs");
        io_func!(fs::write(&mod_rs, buf), || mod_rs.to_string_lossy());

        return Ok(());

        fn compile_mod_u(filepath: &str, file_items: &mut Vec<FileItem>) -> io::Result<String> {
            // parse mod.u file to ast
            let p = cc::compile_file(filepath)?;

            // collect unmentioned mod
            let mut set = HashSet::<&str>::new();
            for item in p.get_pkg_ref().items.iter() {
                match &item.payload {
                    cc::ItemPayload::Mod(a) => {
                        set.insert(&a.name.id);
                    }
                    cc::ItemPayload::MacroCall(a) => match a.body {
                        cc::MacroCallBody::Stmt(ref list) => {
                            for stmt in list {
                                if let cc::Stmt::Item(item) = stmt {
                                    if let cc::ItemPayload::Mod(a) = &item.payload {
                                        set.insert(&a.name.id);
                                    }
                                }
                            }
                        }
                        cc::MacroCallBody::UCustomMod(ref list) => {
                            for a in list {
                                set.insert(&a.id);
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
            for item in file_items.iter_mut() {
                if set.contains(item.name.as_str()) {
                    item.is_custom = true;
                }
            }

            // to rust
            match p.to_rust(true) {
                Err(_) => {
                    return Err(io::Error::new(io::ErrorKind::Other, "to rust error"));
                }
                Ok(f) => return Ok(f.buf()),
            }
        }
    }

    /*
       from $ROOT/crates/some-path/some-name.u,
       to $ROOT/.u/crates/$CRATE/some-path/some-name.rs
    */
    pub fn get_rs_path(&self) -> io::Result<(PathBuf, bool)> {
        let s = self;

        let p = io_func!(fs::canonicalize(PathBuf::from(&s.src)), || &s.src);

        let mut ancestors = p.ancestors();
        let file_name = ancestors.next().unwrap().file_name().unwrap();
        let mut buf = vec![file_name];
        for dir in ancestors {
            let local_root = dir.join(U_LOCAL_ROOT);
            if local_root.is_dir() {
                if buf.len() < 3 {
                    let a: PathBuf = buf.iter().collect();
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("{:?} is not supported", a),
                    ));
                }
                let is_main = if buf.len() == 3 {
                    // crates/$CRATE_ROOT/a.rs
                    true
                } else if file_name == "mod.u" {
                    true
                } else {
                    match buf[2].to_str().unwrap() {
                        "benches" | "bin" | "examples" | "tests" => {
                            // prefix is crates/$CRATE_ROOT
                            // bin/a.rs or bin/a/main.rs -> true
                            // bin/a/src/a.rs -> false
                            buf.len() == 4 || buf.len() == 5
                        }
                        _ => false,
                    }
                };

                buf.insert(0, OsStr::new(U_LOCAL_ROOT));
                buf.insert(0, dir.as_os_str());
                let mut a: PathBuf = buf.iter().collect();
                a.set_extension("rs");
                return Ok((a, is_main));
            }

            if let Some(dir_name) = dir.file_name() {
                buf.insert(0, dir_name);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::Other,
            format!("{} is not in a U project", s.src),
        ))
    }
}

// usage: u u-sync <file-path>
struct CmdSync {
    src: String,
}
impl CmdSync {
    pub fn new(mut args: Vec<String>) -> CmdSync {
        if args.is_empty() {
            return CmdSync {
                src: "".to_string(),
            };
        }

        let src = mem::take(&mut args[0]);
        CmdSync { src }
    }

    fn run(&self) -> io::Result<()> {
        let s = self;

        if s.src.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "usage: u u-sync some-path/u.toml",
            ));
        }

        let f = io_func!(fs::canonicalize(PathBuf::from(&s.src)), || &s.src);
        if f.file_name().unwrap() != "u.toml" {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!("expected u.toml but got {:?}", f),
            ));
        }

        // find .u directory's parent directory, starting from f upward
        let mut buf = Vec::new();
        let mut workspace_root = None;
        let mut ancestors = f.ancestors();
        ancestors.next();
        for dir in ancestors {
            let local_root = dir.join(U_LOCAL_ROOT);
            if local_root.is_dir() {
                workspace_root = Some(local_root);
                break;
            }
            if let Some(dir_name) = dir.file_name() {
                buf.insert(0, dir_name);
            }
        }
        let workspace_root = if let Some(root) = workspace_root {
            root
        } else {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!("{} not found", U_LOCAL_ROOT),
            ));
        };

        env::set_current_dir(&workspace_root)?;
        buf.insert(0, OsStr::new(&workspace_root));

        // in case the new crate is manually created, so there is no lib.rs, src/ and src/mod.rs
        // not $ROOT/u.toml, but $ROOT/crates/$CRATE/u.toml
        if buf.len() > 1 {
            let mut new_buf = buf.clone();

            // check whether $CRATE/lib.rs exists
            new_buf.push(OsStr::new("lib.rs"));
            let new_path: PathBuf = new_buf.iter().collect();
            if !new_path.exists() {
                io_func!(write_default_lib_rs(new_path), || "lib.rs");
            }

            // check whether $CRATE/src/ exists
            new_buf.pop();
            new_buf.push(OsStr::new("src"));
            let mut new_path: PathBuf = new_buf.iter().collect();
            if !new_path.exists() {
                io_func!(fs::create_dir(&new_path), || new_path.to_string_lossy());
            }

            // check whether $CRATE/src/mod.rs exists
            new_path.push(OsStr::new("mod.rs"));
            if !new_path.exists() {
                io_func!(fs::write(&new_path, ""), || new_path.to_string_lossy());
            }
        }

        buf.push(OsStr::new("Cargo.toml"));
        let new_path: PathBuf = buf.iter().collect();
        io_func!(fs::copy(f, &new_path), || new_path.to_string_lossy());

        check_status(process::Command::new("cargo").args(&["build"]).status()?)?;
        io_func!(fs::copy("Cargo.lock", "../u.lock"), || "../u.lock");

        Ok(())
    }
}

struct CmdHelp {}
impl CmdHelp {
    fn run(&self) -> io::Result<()> {
        println!(
            "usage: u <cmd> [<args>]

These are common U commands used in various situations:

u commands:
    new            create a new package at <path>
    init           initilize a U project
    u-compile      compile to target file
    u-sync         sync Cargo.toml and mod.rs files

cargo commands:
    bench                execute all benchmarks of a local package
    build                compile a local package and all of its dependencies
    check                check a local package and all of its dependencies for errors
    clean                remove artifacts that cargo has generated in the past
    clippy               checks a package to catch common mistakes and improve your Rust code
    doc                  build a package's documentation
    fetch                fetch dependencies of a package from the network
    fix                  automatically fix lint warnings reported by rustc
    generate-lockfile    generate the lockfile for a package
    git-checkout         this subcommand has been removed
    install              install a Rust binary. Default location is $HOME/.cargo/bin
    locate-project       print a JSON representation of a Cargo.toml file's location
    login                save an api token from the registry locally. If token is not specified, it will be read from stdin.
    logout               remove an API token from the registry locally
    metadata             output the resolved dependencies of a package, the concrete used versions including overrides, in machine-readable format
    owner                manage the owners of a crate on the registry
    package              assemble the local package into a distributable tarball
    pkgid                print a fully qualified package specification
    publish              upload a package to the registry
    read-manifest        print a JSON representation of a Cargo.toml manifest.
    rustc                compile a package, and pass extra options to the compiler
    rustdoc              build a package's documentation, using specified custom flags.
    search               search packages in crates.io
    test                 execute all unit and integration tests and build examples of a local package
    tree                 display a tree visualization of a dependency graph
    uninstall            remove a Rust binary
    vendor               vendor all dependencies for a project locally
    verify-project       check correctness of crate manifest
    version              show version information
    yank                 remove a pushed crate from the index
"
        );
        Ok(())
    }
}

// --------------------------------------------------------------------------------
// cargo command
struct CargoCmd {
    args: Vec<String>, // check -q
    type_: CargoCmdType,
}
enum CargoCmdType {
    Other,
    New,
    Init,
}
impl CargoCmd {
    fn new(args: Vec<String>) -> CargoCmd {
        let mut r = CargoCmd {
            args,
            type_: CargoCmdType::Other,
        };
        let cmd = r.args[0].as_str();
        match cmd {
            "new" => r.type_ = CargoCmdType::New,
            "init" => r.type_ = CargoCmdType::Init,

            // other
            _ => r.type_ = CargoCmdType::Other,
        }
        r
    }

    fn run(&self) -> io::Result<()> {
        let s = self;
        match s.type_ {
            CargoCmdType::Other => s.r_other(),
            CargoCmdType::New => s.r_new(),
            CargoCmdType::Init => s.r_init(),
        }
    }

    /** u ${cmd} is the same as cargo ${cmd}
        cd .u
        cargo ${args}

        note: in order to avoid include!("../../move/a") errors, I have to filter out
            `No such file or directory` errors in u-cargo-lint
    */
    fn r_other(&self) -> io::Result<()> {
        let s = self;

        env::set_current_dir(U_LOCAL_ROOT)?;
        let run_result = check_status(process::Command::new("cargo").args(&s.args).status()?);

        if let Err(err) = run_result {
            return Err(err);
        }

        Ok(())
    }

    /** u new ${name}
       mkdir ${name}

       cd ${name}
       git init
       mkdir .u
       echo "/.u" > .gitignore
       echo "sth" > u.toml
       mkdir -p crates/${name}

       cd crates/${name}
       mkdir src
       echo "sth" > main.u
       echo "sth" > u.toml

       // ----------------------------------------------------------------------
       // create .rs files
       cd ../../.u
       cp ../u.toml Cargo.toml
       mkdir crates

       cd crates
       cargo new u---u
       mv u---u ${name}

       cd ${name}
       rm -r .git
       rm .gitignore
       mv src/main.rs main.rs
       touch src/mod.rs
       echo "sth" > Cargo.toml
       echo "sth" > lib.rs
    */
    fn r_new(&self) -> io::Result<()> {
        let s = self;

        if s.args.len() == 1 {
            println!(
                "usage:
u new my-project
"
            );
            return Ok(());
        }

        let name = &s.args[1];
        io_func!(fs::create_dir(&name), || &name);

        // at workspace root
        env::set_current_dir(&name)?;
        check_status(process::Command::new("git").args(&["init"]).status()?)?;
        io_func!(fs::create_dir(U_LOCAL_ROOT), || U_LOCAL_ROOT);
        let a = format!("/{}\n", U_LOCAL_ROOT);
        io_func!(fs::write(".gitignore", &a), || a);
        let root_crate = format!("crates/{}", name);
        io_func!(fs::create_dir_all(&root_crate), || root_crate);
        let u_toml = format!(
            r#"[workspace]
members = ["crates/*"]
default-members = ["{}"]
resolver = "2"

[workspace.package]
version = "0.1.0"
edition = "2024"
publish = false

[workspace.dependencies]
"#,
            &root_crate
        );
        io_func!(fs::write("u.toml", &u_toml), || root_crate);

        // at crate root
        env::set_current_dir(&root_crate)?;
        io_func!(fs::create_dir("src"), || "src");
        let main_u = r#"import {
}

main func() {
	d,,("Hello, world!")
}
"#;
        io_func!(fs::write("main.u", main_u), || "main.u");
        let cargo_toml = &format!(
            r#"[package]
name = "{}"
version.workspace = true
edition.workspace = true
publish.workspace = true

default-run = "{}"
autobins = false

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[lib]
path = "lib.rs"

[[bin]]
name = "{}"
path = "main.rs"

[dependencies]
"#,
            name, name, name,
        );
        io_func!(fs::write("u.toml", cargo_toml), || "u.toml");

        // ----------------------------------------------------------------------
        // create rust files
        env::set_current_dir(&format!("../../{}", U_LOCAL_ROOT))?;
        io_func!(fs::copy("../u.toml", "Cargo.toml"), || "../u.toml");
        io_func!(fs::create_dir("crates"), || "crates");

        env::set_current_dir("crates")?;
        let rust_dir = "u---u";
        check_status(
            process::Command::new("cargo")
                .args(&["new", rust_dir])
                .status()?,
        )?;
        io_func!(fs::rename(rust_dir, name), || rust_dir);

        env::set_current_dir(name)?;
        io_func!(fs::remove_dir_all(".git"), || ".git");
        io_func!(fs::remove_file(".gitignore"), || ".gitignore");
        io_func!(fs::rename("src/main.rs", "main.rs"), || "main.rs");
        io_func!(fs::write("src/mod.rs", ""), || "mod.rs");

        io_func!(fs::write("Cargo.toml", cargo_toml), || "Cargo.toml");
        io_func!(write_default_lib_rs("lib.rs"), || "lib.rs");

        Ok(())
    }

    /** u init
        rm -r .u
        mkdir .u

        cd .u
        cp ../u.toml Cargo.toml
        cp ../u.lock Cargo.lock
        mkdir crates

        cd crates
        for dir in dirs {
            cp ${u.toml} Cargo.toml
            cp ${main.u} main.rs
            echo "sth" > lib.rs
            visit_src("${src}", "src")
            visit_other("${other}", "other")
        }
    */
    fn r_init(&self) -> io::Result<()> {
        if Path::new(&format!("./{}", U_LOCAL_ROOT)).exists() {
            io_func!(fs::remove_dir_all(U_LOCAL_ROOT), || U_LOCAL_ROOT);
        }

        // at .u
        io_func!(fs::create_dir(U_LOCAL_ROOT), || U_LOCAL_ROOT);
        env::set_current_dir(U_LOCAL_ROOT)?;
        io_func!(fs::copy("../u.toml", "Cargo.toml"), || "../u.toml");
        let _ = io_func!(fs::copy("../u.lock", "Cargo.lock"), || "../u.lock");
        io_func!(fs::create_dir("crates"), || "crates");

        env::set_current_dir("crates")?;
        for entry in io_func!(fs::read_dir("../../crates"), || "../../crates") {
            let entry = entry?;
            let path = entry.path();
            let name = path.file_stem().unwrap().to_str().unwrap().to_string();

            if path.is_dir() {
                init_crate(&name)?;
            }
        }

        return Ok(());

        // ----------------------------------------------------------------------
        fn init_crate(crate_name: &str) -> io::Result<()> {
            let cwd = env::current_dir()?;

            io_func!(fs::create_dir(crate_name), || crate_name);

            env::set_current_dir(crate_name)?;
            io_func!(
                fs::copy(
                    &format!("../../../crates/{}/u.toml", crate_name),
                    "Cargo.toml",
                ),
                || "Cargo.toml"
            );

            let main_file = format!("../../../crates/{}/main.u", crate_name);
            if Path::new(&main_file).exists() {
                let main = compile_to_rust(&main_file, true).unwrap();
                io_func!(fs::write("main.rs", main), || "main.rs");
            }
            let lib_file = format!("../../../crates/{}/lib.u", crate_name);
            if Path::new(&lib_file).exists() {
                let main = compile_to_rust(&lib_file, true).unwrap();
                io_func!(fs::write("lib.rs", main), || "lib.rs");
            } else {
                io_func!(write_default_lib_rs("lib.rs"), || "lib.rs");
            }

            let u_root_src = format!("../../../crates/{}/src", crate_name);
            if !Path::new(&u_root_src).exists() {
                io_func!(fs::create_dir(&u_root_src), || u_root_src);
            }
            visit_src(&CmdCompile { src: u_root_src }, "src")?;

            let list = ["benches", "bin", "examples", "tests"];
            for item in &list {
                visit_other(
                    &CmdCompile {
                        src: format!("../../../crates/{}/{}", crate_name, item),
                    },
                    &item.to_string(),
                    true,
                )?;
            }

            env::set_current_dir(cwd)?;
            return Ok(());
        }

        fn visit_src(s: &CmdCompile, dir: &str) -> io::Result<()> {
            if !Path::new(&s.src).exists() {
                return Ok(());
            }
            io_func!(fs::create_dir(dir), || dir);
            for entry in io_func!(fs::read_dir(&s.src), || &s.src) {
                let entry = entry?;
                let path = entry.path();
                let path_str = path.to_str().unwrap().to_string();
                let name = path.file_stem().unwrap().to_str().unwrap().to_string();

                if path.is_dir() {
                    visit_src(&CmdCompile { src: path_str }, &format!("{}/{}", dir, name))?;
                } else if name != "mod" {
                    let src = compile_to_rust(&path_str, false).unwrap();
                    io_func!(fs::write(&format!("{}/{}.rs", dir, name), &src), || &src);
                }
            }
            s.set_mod_rs(Path::new(dir), true)?;
            Ok(())
        }

        fn visit_other(s: &CmdCompile, dir: &str, outer: bool) -> io::Result<()> {
            if !Path::new(&s.src).exists() {
                return Ok(());
            }
            io_func!(fs::create_dir(dir), || dir);
            for entry in io_func!(fs::read_dir(&s.src), || &s.src) {
                let entry = entry?;
                let path = entry.path();
                let path_str = path.to_str().unwrap().to_string();
                let name = path.file_stem().unwrap().to_str().unwrap().to_string();

                if path.is_dir() {
                    let dir_name = format!("{}/{}", dir, name);
                    let compile = &CmdCompile { src: path_str };
                    if outer {
                        visit_other(compile, &dir_name, false)?;
                    } else {
                        visit_src(compile, &dir_name)?;
                    }
                } else {
                    let src = compile_to_rust(&path_str, true).unwrap();
                    io_func!(fs::write(&format!("{}/{}.rs", dir, name), &src), || &src);
                }
            }
            Ok(())
        }
    }
}

fn write_default_lib_rs<P: AsRef<Path>>(path: P) -> io::Result<()> {
    fs::write(
        path,
        "mod src;
#[allow(unused_imports)]
pub use self::src::*;
",
    )
}

fn check_status(status: process::ExitStatus) -> io::Result<()> {
    if !status.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "failed to run command",
        ));
    }
    return Ok(());
}

// --------------------------------------------------------------------------------
fn compile_to_rust(filepath: &str, is_main: bool) -> Result<String, String> {
    let data = cc::get_file_content(filepath).unwrap();

    let mut l = cc::Lex::new();
    if let Err(err) = l.lex(data) {
        return Err(serde_json::to_string(&vec![err]).unwrap());
    }

    let mut p = cc::Parse::new(l);
    if let Err(err) = p.parse() {
        return Err(serde_json::to_string(&err).unwrap());
    }

    match p.to_rust(is_main) {
        Err(err) => Err(serde_json::to_string(&err).unwrap()),
        Ok(f) => {
            Ok(f.buf())
            // Ok(String::new())
            /* for item in f.span_pairs() {
                println!("{:?}", item);
            }
            Ok(String::new()) */
        }
    }
}
