use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::io;
use std::process;

/**
usage: u-cargo-lint [options]

implementation:
    add an ale linter in ~/.vim/bundle/ale/ale-linters/
        u/
            u.vim

    mkdir ~/.vim/bundle/ale/ale-linters/u
    cp src/data/u.vim ~/.vim/bundle/ale/ale-linters/u/u.vim

    ale will run `cargo check --frozen --message-format=json`,
    filter the output messages to find the (rust-file, rust-byte-start, rust-byte-end),
    find the corresponding u-file,
    compile u-file, output is [{ u: {line, column, width}, rust: {byte_start, width} }],
    iterate over the list to find those items that contains (rust-byte-start, rust-bype-end),
    the first item and last item is the new {line_start, line_end, column_start, column_end}
    update the original message with new {line_start, line_end, column_start, column_end}
    println the message

debug:
    run
        u-cargo-lint check --frozen --message-format=json
    in project root directory

*/
fn main() -> std::io::Result<()> {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);
    u_cargo_lint(args)?;
    Ok(())
}

// cargo check --frozen --message-format=json -q
fn u_cargo_lint(args: Vec<String>) -> io::Result<()> {
    let pwd = env::current_dir()?;
    // from abc/crates/abc/ to abc/.u/crates/abc/
    let a = pwd.file_name().unwrap();
    let b = pwd.parent().unwrap().file_name().unwrap();
    let new_pwd = pwd
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join(u::U_LOCAL_ROOT)
        .join(b)
        .join(a);
    env::set_current_dir(&new_pwd)?;

    let output = process::Command::new("cargo").args(&args).output()?;
    let out = String::from_utf8(output.stdout).unwrap();

    let pwd_str = pwd.to_str().unwrap();
    let mut file_map = HashMap::<String, u::LangFormatter>::new();
    for line in out.lines() {
        if !line.contains(r#""reason":"compiler-message""#) {
            continue;
        }

        // not needed any more, because the addition of u-path"move/a" string literal
        // skip include!("../../raw/a") errors
        /* if line.contains(r#"No such file or directory"#) {
            continue;
        } */

        // eprintln!("{}\n", line);
        let mut manifest: Manifest = serde_json::from_str(&line).unwrap();
        // eprintln!("{:#?}", manifest);

        let spans = &mut manifest.message.spans;
        for span in spans.iter_mut() {
            if let Some(a) = get_real_expansion(&span.expansion) {
                span.file_name = a.span.file_name.clone();
                span.byte_start = a.span.byte_start;
                span.byte_end = a.span.byte_end;
            }

            let list: Vec<_> = span.file_name.split(".u/").collect();
            if !span.file_name.starts_with('/') {
                // relative path
                span.file_name = list[0].replace(".rs", ".u");
            } else {
                // absolute path
                if list[0] != pwd_str {
                    // skip lint messages from other crates
                    continue;
                }
                let relative = list[1].replace(".rs", ".u");
                span.file_name = format!("{}{}", list[0], relative);
            }
            // eprintln!("{}", span.file_name);

            let file = &mut span.file_name;
            if !file_map.contains_key(file) {
                let mut u_file = file.clone();
                // pwd is abc/.u/crates/abc/
                // from abc/.u/crates/abc/main.rs to crates/abc/main.u
                if !file.starts_with('/') {
                    u_file.insert_str(0, "../../../");
                }

                let compile = u::CmdCompile::new(vec![u_file.clone()]);
                let (_, is_main) = compile.get_rs_path()?;

                let a = compile_to_rust_lint(&u_file, is_main)?;
                file_map.insert(file.clone(), a);
            }

            let pairs = file_map.get(file).unwrap().span_pairs();
            /* for item in pairs {
                eprintln!("{:?}", item);
            } */
            let sp = NewSpan::new(span, pairs);
            // eprintln!("{:?}", sp);

            span.line_start = sp.line_start;
            span.line_end = sp.line_end;
            span.column_start = sp.column_start;
            span.column_end = sp.column_end;
        }

        let out = serde_json::to_string(&manifest).unwrap();
        println!("{}", out);
    }

    return Ok(());

    fn get_real_expansion(
        expansion: &Option<ManifestMessageSpanExpansion>,
    ) -> &Option<ManifestMessageSpanExpansion> {
        if let Some(a) = &expansion {
            if a.span.expansion.is_some() {
                return get_real_expansion(&a.span.expansion);
            } else {
                return expansion;
            }
        } else {
            return &None;
        }
    }
}

fn compile_to_rust_lint(filepath: &str, is_main: bool) -> io::Result<u::LangFormatter> {
    let p = u::compile_file(filepath)?;
    match p.to_rust(is_main) {
        Err(_) => Err(io::Error::new(io::ErrorKind::Other, "parse error")),
        Ok(f) => Ok(f),
    }
}

#[derive(Debug, Default)]
struct NewSpan {
    column_end: usize,
    column_start: usize,
    line_end: usize,
    line_start: usize,
}
impl NewSpan {
    fn new(span: &ManifestMessageSpan, pairs: &[u::SpanPair]) -> NewSpan {
        let mut r = NewSpan::default();

        let ManifestMessageSpan {
            byte_start,
            byte_end,
            ..
        } = span;

        let mut has_first = false;
        for pair in pairs {
            let start = pair.rust.byte_start;
            let end = start + pair.rust.width - 1;
            if *byte_start > end {
                continue;
            }
            if *byte_end - 1 < start {
                if !has_first {
                    r.line_start = pair.u.line;
                    r.line_end = pair.u.line;
                    r.column_start = pair.u.column;
                    r.column_end = pair.u.column + byte_end - byte_start;
                }
                break;
            }

            if !has_first {
                r.line_start = pair.u.line;
                r.column_start = pair.u.column;
                has_first = true;
            }
            r.line_end = pair.u.line;
            r.column_end = pair.u.column + pair.u.width;
        }

        r
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Manifest {
    manifest_path: String,
    message: ManifestMessage,
}

#[derive(Debug, Serialize, Deserialize)]
struct ManifestMessage {
    code: Option<ManifestMessageCode>,
    level: String,
    message: String,
    spans: Vec<ManifestMessageSpan>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ManifestMessageCode {
    code: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct ManifestMessageSpan {
    byte_end: usize,
    byte_start: usize,
    column_end: usize,
    column_start: usize,
    file_name: String,
    is_primary: bool,
    label: Option<String>,
    line_end: usize,
    line_start: usize,
    expansion: Option<ManifestMessageSpanExpansion>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ManifestMessageSpanExpansion {
    span: Box<ManifestMessageSpan>,
}
