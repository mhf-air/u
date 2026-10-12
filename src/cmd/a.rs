use crate::compile as cc;
use crate::rs as cr;
use std::fs;
use std::io;
use std::mem;

// pub const U_TO_RS: bool = true;
pub const U_TO_RS: bool = false;

// -----------------------------------------------------------------------------
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
            "u-r" => {
                args.remove(0);
                Config {
                    cmd: Cmd::ToRs(CmdToRs::new(args)),
                }
            }
            "r-u" => {
                args.remove(0);
                Config {
                    cmd: Cmd::FromRs(CmdFromRs::new(args)),
                }
            }
            _ => {
                return Config {
                    cmd: Cmd::None(CmdNone { cmd }),
                };
            }
        }
    }

    pub fn run(&self) -> io::Result<()> {
        match &self.cmd {
            Cmd::None(a) => a.run(),
            Cmd::ToRs(a) => a.run(),
            Cmd::FromRs(a) => a.run(),
        }
    }
}

enum Cmd {
    None(CmdNone),
    ToRs(CmdToRs),
    FromRs(CmdFromRs),
}

struct CmdNone {
    cmd: String,
}
impl CmdNone {
    fn run(&self) -> io::Result<()> {
        let s = self;
        if s.cmd.as_str() == "target/debug/u" {
            let buf = if U_TO_RS {
                let filepath = "data/a.u";
                compile_to_rs(filepath).unwrap()
            } else {
                let filepath = "data/r.rs";
                compile_from_rs(filepath).unwrap()
            };

            println!("{}", buf);
            Ok(())
        } else {
            CmdHelp {}.run()
        }
    }
}

// u to-rs /some-path/.a.u /some-path/a.rs
#[derive(Debug, Default)]
pub struct CmdToRs {
    src: String,
    dest: String,
}
impl CmdToRs {
    pub fn new(mut args: Vec<String>) -> CmdToRs {
        if args.is_empty() {
            return CmdToRs::default();
        }

        let src = mem::take(&mut args[0]);
        let dest = mem::take(&mut args[1]);
        CmdToRs { src, dest }
    }

    fn run(&self) -> io::Result<()> {
        let s = self;
        if s.src.is_empty() {
            println!(
                "usage: u to-rs <file-path>
"
            );
            return Ok(());
        }

        let s = self;

        let r = compile_to_rs(&s.src);
        if let Err(err) = r {
            println!("{}", err);
            return Ok(());
        }
        let buf = r.ok().unwrap();
        io_func!(fs::write(&s.dest, buf), || "");

        Ok(())
    }
}

// u from-rs /some-path/a.rs /some-path/.a.u
#[derive(Debug, Default)]
pub struct CmdFromRs {
    src: String,
    dest: String,
}
impl CmdFromRs {
    pub fn new(mut args: Vec<String>) -> CmdFromRs {
        if args.is_empty() {
            return CmdFromRs::default();
        }

        let src = mem::take(&mut args[0]);
        let dest = mem::take(&mut args[1]);
        CmdFromRs { src, dest }
    }

    fn run(&self) -> io::Result<()> {
        let s = self;
        if s.src.is_empty() {
            println!(
                "usage: u to-rs <file-path>
"
            );
            return Ok(());
        }

        let s = self;

        let r = compile_from_rs(&s.src);
        if let Err(err) = r {
            println!("{}", err);
            return Ok(());
        }
        let buf = r.ok().unwrap();
        io_func!(fs::write(&s.dest, buf), || "");

        Ok(())
    }
}

struct CmdHelp {}
impl CmdHelp {
    fn run(&self) -> io::Result<()> {
        println!(
            "usage: u <cmd> [<args>]

These are common U commands used in various situations:

commands:
    to-rs       compile .u to .rs
    from-rs     compile .rs to .u
"
        );
        Ok(())
    }
}

// --------------------------------------------------------------------------------
fn compile_to_rs(filepath: &str) -> Result<String, String> {
    let data = fs::read_to_string(&filepath).unwrap();

    let mut l = cc::lex::Lex::new();
    if let Err(err) = l.lex(data) {
        return Err(serde_json::to_string(&vec![err]).unwrap());
    }

    let mut p = cc::parse::Parse::new(l);
    if let Err(err) = p.parse() {
        return Err(serde_json::to_string(&err).unwrap());
    }

    match p.to_rs() {
        Err(err) => Err(serde_json::to_string(&err).unwrap()),
        Ok(f) => {
            Ok(f.buf)
            // Ok(String::new())
            /* for item in f.span_pairs() {
                println!("{:?}", item);
            }
            Ok(String::new()) */
        }
    }
}

fn compile_from_rs(filepath: &str) -> Result<String, String> {
    let data = fs::read_to_string(&filepath).unwrap();

    let mut l = cr::rl::Lex::new();
    if let Err(err) = l.lex(data) {
        return Err(serde_json::to_string(&vec![err]).unwrap());
    }

    let mut p = cr::rp::Parse::new(l);
    if let Err(err) = p.parse() {
        return Err(serde_json::to_string(&err).unwrap());
    }

    match p.to_u() {
        Err(err) => Err(serde_json::to_string(&err).unwrap()),
        Ok(f) => {
            Ok(f.buf)
            // Ok(String::new())
            /* for item in f.span_pairs() {
                println!("{:?}", item);
            }
            Ok(String::new()) */
        }
    }
}
