use std::env;
use std::io;
use u::ToLang;

/**
usage: utags [options] file

options:
    -silent     do not produce any output on error.
    -sort       sort tags. (default true)
*/
fn main() -> std::io::Result<()> {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);
    if args.is_empty() {
        return Err(io::Error::new(io::ErrorKind::Other, "expected file name"));
    }
    utags(args)?;
    Ok(())
}

/*
tag types:
    Macros      r
    Constant    c
    Static      v
    Alias       a
    Type        t
    Interface   n
    Field       w
    Method      m
    Function    f
 */
#[derive(Default)]
struct TagField<'a> {
    access: Option<&'a str>,
    signature: Option<&'a str>,
    type_: Option<&'a str>,
    ctype: Option<&'a str>,
    line: usize,
    ntype: Option<&'a str>,
    language: Option<&'a str>,
    extra_tag: Option<&'a str>,
}

impl<'a> TagField<'a> {
    fn to_string(&self) -> String {
        let s = self;
        let mut p = String::new();

        if let Some(access) = s.access {
            p.push_str("\taccess:");
            p.push_str(access);
        }
        if let Some(signature) = s.signature {
            p.push_str("\tsignature:");
            p.push_str(signature);
        }
        if let Some(type_) = s.type_ {
            p.push_str("\ttype:");
            p.push_str(type_);
        }
        if let Some(ctype) = s.ctype {
            p.push_str("\tctype:");
            p.push_str(ctype);
        }

        p.push_str("\tline:");
        p.push_str(&s.line.to_string());

        if let Some(ntype) = s.ntype {
            p.push_str("\tntype:");
            p.push_str(ntype);
        }
        if let Some(language) = s.language {
            p.push_str("\tlanguage:");
            p.push_str(language);
        }
        if let Some(extra_tag) = s.extra_tag {
            p.push_str("\textraTag:");
            p.push_str(extra_tag);
        }

        p
    }

    fn set_access(&mut self, vis: &u::VisibilityPayload) {
        let s = self;
        match vis {
            u::VisibilityPayload::Pub => {
                s.access = Some("public");
            }
            u::VisibilityPayload::Private => {
                s.access = Some("private");
            }
            u::VisibilityPayload::Super
            | u::VisibilityPayload::Crate
            | u::VisibilityPayload::SimplePath(_) => {
                s.access = Some("");
            }
        }
    }
}

fn utags(args: Vec<String>) -> std::io::Result<()> {
    let mut filepath = None;
    for arg in args {
        if arg.starts_with('-') {
            continue;
        }
        filepath = Some(arg);
    }
    if filepath.is_none() {
        return Err(io::Error::new(io::ErrorKind::Other, "expected file name"));
    }
    let filepath = filepath.unwrap();
    let pkg = u::compile_file(&filepath)?;

    let mut p = String::new();
    p.push_str(
        r#"!_TAG_FILE_FORMAT	2
!_TAG_FILE_SORTED	1	/0=unsorted, 1=sorted/
!_TAG_PROGRAM_AUTHOR	mhf
!_TAG_PROGRAM_NAME	utags
!_TAG_PROGRAM_URL	https://github.com/mhf-air/u
!_TAG_PROGRAM_VERSION	1.0.0
"#,
    );

    parse_items(&mut p, &pkg.get_pkg().items, &filepath, &Vec::new());

    println!("{}", p);

    Ok(())
}

fn parse_items(p: &mut String, items: &[u::Item], filepath: &str, mod_: &Vec<&str>) {
    let t;
    let ctype = if !mod_.is_empty() {
        t = mod_.join("..");
        Some(t.as_str())
    } else {
        None
    };

    for item in items {
        let mut sig = String::new();
        let ng;
        let type_;
        let mut fields = None;
        let mut field = TagField::default();
        field.ctype = ctype;

        match &item.payload {
            u::ItemPayload::Mod(a) => {
                field.type_ = Some("mod");
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char: 't',
                });

                let mut new_mod = mod_.clone();
                new_mod.push(&a.name.id);
                parse_items(p, &a.items, filepath, &new_mod);
            }
            u::ItemPayload::Test(a) => {
                field.type_ = Some("test");
                ng = name_generics(&NameGenerics {
                    name: &u::Identifier {
                        id: "test".to_string(),
                        span: a.span_test,
                        prefix: None,
                    },
                    generics: &None,
                    filepath: &filepath,
                    type_char: 't',
                });

                let mut new_mod = mod_.clone();
                new_mod.push("test");
                parse_items(p, &a.items, filepath, &new_mod);
            }
            u::ItemPayload::Extern(a) => {
                field.type_ = Some("extern");
                ng = name_generics(&NameGenerics {
                    name: &u::Identifier {
                        id: "extern".to_string(),
                        span: u::Span {
                            line: a.span_extern.line,
                            column: 0,
                            width: 6,
                        },
                        prefix: None,
                    },
                    generics: &None,
                    filepath: &filepath,
                    type_char: 't',
                });

                let mut new_mod = mod_.clone();
                new_mod.push("extern");
                extern_items(p, &a, filepath, &new_mod);
            }
            u::ItemPayload::Crate(_) | u::ItemPayload::Import(_) | u::ItemPayload::MacroCall(_) => {
                continue;
            }
            u::ItemPayload::Func(a) => {
                let generics = generics_text(&a.generics);
                if let Some(g) = generics {
                    sig.push_str(&g);
                }
                sig.push_str(&func_sig_text(&a));
                field.signature = Some(&sig);

                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char: 'f',
                });
            }
            u::ItemPayload::TypeAlias(a) => {
                if let Some(t) = &a.type_ {
                    type_ = to_u(t);
                    field.type_ = Some(&type_);
                }
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &a.generics,
                    filepath: &filepath,
                    type_char: 'a',
                });
            }
            u::ItemPayload::Struct(a) => {
                field.type_ = Some("struct");
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &a.generics,
                    filepath: &filepath,
                    type_char: 't',
                });

                fields = Some(struct_fields(&a, &filepath, &mod_));
            }
            u::ItemPayload::Enum(a) => {
                field.type_ = Some("enum");
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &a.generics,
                    filepath: &filepath,
                    type_char: 't',
                });

                fields = Some(enum_fields(&a, &filepath, &mod_));
            }
            u::ItemPayload::Union(a) => {
                field.type_ = Some("union");
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &a.generics,
                    filepath: &filepath,
                    type_char: 't',
                });

                fields = Some(union_fields(&a, &filepath, &mod_));
            }
            u::ItemPayload::Const(a) => {
                type_ = to_u(&a.type_);
                field.type_ = Some(&type_);
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char: 'c',
                });
            }
            u::ItemPayload::Static(a) => {
                type_ = to_u(&a.type_);
                field.type_ = Some(&type_);
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char: 'v',
                });
            }
            u::ItemPayload::Interface(a) => {
                field.type_ = Some("interface");
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &a.generics,
                    filepath: &filepath,
                    type_char: 't',
                });

                fields = Some(interface_fields(&a, &filepath, &mod_));
            }
            u::ItemPayload::Impl(a) => {
                impl_items(p, &a, filepath, &mod_);
                continue;
            }
            u::ItemPayload::Macro(a) => {
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char: 'r',
                });
            }
        }

        let NameGenericsReturn {
            line,
            text,
            generics,
        } = ng;
        field.line = line;
        p.push_str(&text);
        /* if let Some(generics) = &generics {
            field.signature = Some(generics);
        } */

        field.set_access(&item.public.payload);
        p.push_str(&field.to_string());
        p.push('\n');

        if let Some(fields) = fields {
            p.push_str(&fields);
        }
    }
}

fn to_u(a: &impl ToLang) -> String {
    let mut formatter = u::LangFormatter::new(false, Vec::new());
    a.to_u(&mut formatter);
    formatter.buf()
}

struct NameGenerics<'a, 'b> {
    name: &'b u::Identifier,
    generics: &'a Option<u::Generics>,
    filepath: &'a str,
    type_char: char,
}
struct NameGenericsReturn {
    line: usize,
    text: String,
    generics: Option<String>,
}

fn name_generics(a: &NameGenerics) -> NameGenericsReturn {
    let generics = generics_text(&a.generics);
    let b = generics.clone().unwrap_or(String::new());
    NameGenericsReturn {
        line: a.name.span.line,
        text: format!(
            "{}{}\t{}\t{};\"\t{}",
            a.name.id, b, a.filepath, a.name.span.line, a.type_char
        ),
        generics,
    }
}

fn generics_text(a: &Option<u::Generics>) -> Option<String> {
    let mut generics = None;
    if let Some(g) = &a {
        let mut p = String::from("[");
        for (i, item) in g.params.iter().enumerate() {
            if i != 0 {
                p.push_str(", ");
            }
            if matches!(item.payload, u::GenericParamPayload::Lifetime(_)) {
                p.push('\'');
            }
            p.push_str(&item.name.id);
        }
        p.push(']');
        generics = Some(p);
    }
    generics
}
fn func_sig_text(a: &u::Func) -> String {
    let mut p = String::new();

    if let Some(b) = &a.self_param {
        p.push_str(" (");
        match &b.payload {
            u::SelfParamPayload::Type(type_) => {
                p.push_str(&to_u(type_));
            }
            u::SelfParamPayload::Short(c) => {
                p.push('&');
                if let Some(label) = &c.label {
                    p.push_str(&to_u(label));
                    if c.mut_ {
                        p.push_str(" mut")
                    }
                } else {
                    if c.mut_ {
                        p.push_str("mut")
                    }
                }
            }
        }
        p.push_str(") ");
    }

    p.push('(');
    for (i, item) in a.params.iter().enumerate() {
        if i != 0 {
            p.push_str(", ");
        }
        p.push_str(&item.name.id);
        p.push(' ');
        p.push_str(&to_u(&item.type_));
    }
    if a.variadics.is_some() {
        p.push_str(", ...");
    }
    p.push(')');
    if let Some(r) = &a.return_type {
        p.push(' ');
        p.push_str(&to_u(r));
    }
    p
}

fn struct_fields(a: &u::Struct, filepath: &str, mod_: &Vec<&str>) -> String {
    if a.payload.is_none() {
        return String::new();
    }

    let mut p = String::new();
    match a.payload.as_ref().unwrap() {
        u::StructPayload::Struct(f) => {
            for item in f {
                p.push_str(&format!(
                    "{}\t{}\t{};\"\tw",
                    item.name.id, filepath, item.name.span.line
                ));

                let mut field = TagField::default();
                field.line = item.name.span.line;

                let ctype = nested_ctype(mod_, &a.name.id, &a.generics);
                field.ctype = Some(&ctype);

                field.set_access(&item.public.payload);
                let type_ = &to_u(&item.type_);
                field.type_ = Some(type_);
                p.push_str(&field.to_string());
                p.push('\n');
            }
        }
        u::StructPayload::Tuple(f) => {
            for (i, item) in f.iter().enumerate() {
                p.push_str(&format!("{}\t{}\t{};\"\tw", i, filepath, a.name.span.line));

                let mut field = TagField::default();
                field.line = a.name.span.line;

                let ctype = nested_ctype(mod_, &a.name.id, &a.generics);
                field.ctype = Some(&ctype);

                field.set_access(&item.public.payload);
                let type_ = &to_u(&item.type_);
                field.type_ = Some(type_);
                p.push_str(&field.to_string());
                p.push('\n');
            }
        }
    }

    p
}

fn enum_fields(a: &u::Enum, filepath: &str, mod_: &Vec<&str>) -> String {
    let mut p = String::new();
    for item in &a.items {
        p.push_str(&format!(
            "{}\t{}\t{};\"\tw",
            item.name.id, filepath, item.name.span.line
        ));

        let mut field = TagField::default();
        field.line = item.name.span.line;

        let ctype = nested_ctype(mod_, &a.name.id, &a.generics);
        field.ctype = Some(&ctype);

        let mut a = String::new();
        match &item.payload {
            u::EnumItemPayload::Struct(f) => {
                a.push('(');
                for (i, item) in f.iter().enumerate() {
                    if i != 0 {
                        a.push_str(", ");
                    }
                    a.push_str(&to_u(&item.name));
                    a.push(' ');
                    a.push_str(&to_u(&item.type_));
                }
                a.push(')');
            }
            u::EnumItemPayload::Tuple(f) => {
                a.push('(');
                for (i, item) in f.iter().enumerate() {
                    if i != 0 {
                        a.push_str(", ");
                    }
                    a.push_str(&to_u(&item.type_));
                }
                a.push(')');
            }
            _ => {}
        }
        if !a.is_empty() {
            field.signature = Some(&a);
        }

        p.push_str(&field.to_string());
        p.push('\n');
    }

    p
}

fn union_fields(a: &u::Union, filepath: &str, mod_: &Vec<&str>) -> String {
    let mut p = String::new();
    for item in &a.fields {
        p.push_str(&format!(
            "{}\t{}\t{};\"\tw",
            item.name.id, filepath, item.name.span.line
        ));

        let mut field = TagField::default();
        field.line = item.name.span.line;

        let ctype = nested_ctype(mod_, &a.name.id, &a.generics);
        field.ctype = Some(&ctype);

        field.set_access(&item.public.payload);
        let type_ = &to_u(&item.type_);
        field.type_ = Some(type_);
        p.push_str(&field.to_string());
        p.push('\n');
    }

    p
}

fn interface_fields(a: &u::Interface, filepath: &str, mod_: &Vec<&str>) -> String {
    let mut p = String::new();
    for item in &a.items {
        let mut field = TagField::default();

        let ctype = nested_ctype(mod_, &a.name.id, &a.generics);
        field.ctype = Some(&ctype);

        let sig;
        let type_;

        match &item.payload {
            u::AssociatedItemPayload::Func(f) => {
                let tag = if f.self_param.is_some() { 'm' } else { 'f' };
                p.push_str(&format!(
                    "{}\t{}\t{};\"\t{}",
                    f.name.id, filepath, f.name.span.line, tag
                ));
                field.line = f.name.span.line;
                sig = func_sig_text(f);
                field.signature = Some(&sig);
            }
            u::AssociatedItemPayload::Const(f) => {
                p.push_str(&format!(
                    "{}\t{}\t{};\"\tc",
                    f.name.id, filepath, f.name.span.line
                ));
                field.line = f.name.span.line;
                type_ = to_u(&f.type_);
                field.type_ = Some(&type_);
            }
            u::AssociatedItemPayload::TypeAlias(f) => {
                p.push_str(&format!(
                    "{}\t{}\t{};\"\ta",
                    f.name.id, filepath, f.name.span.line
                ));
                field.line = f.name.span.line;
                if let Some(t) = &f.type_ {
                    type_ = to_u(t);
                    field.type_ = Some(&type_);
                }
            }
            u::AssociatedItemPayload::MacroCall(_) => {}
        }

        field.set_access(&item.public.payload);
        p.push_str(&field.to_string());
        p.push('\n');
    }

    p
}

fn extern_items(p: &mut String, a: &u::ExternalBlock, filepath: &str, mod_: &Vec<&str>) {
    let t;
    let ctype = if !mod_.is_empty() {
        t = mod_.join("..");
        Some(t.as_str())
    } else {
        None
    };

    for item in &a.items {
        let mut sig = String::new();
        let ng;
        let type_;
        let mut field = TagField::default();
        field.ctype = ctype;

        match &item.payload {
            u::ExternalBlockItemPayload::Func(a) => {
                let generics = generics_text(&a.generics);
                if let Some(g) = generics {
                    sig.push_str(&g);
                }
                sig.push_str(&func_sig_text(&a));
                field.signature = Some(&sig);

                let type_char = if a.self_param.is_some() { 'm' } else { 'f' };
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char,
                });
            }
            u::ExternalBlockItemPayload::Static(a) => {
                type_ = to_u(&a.type_);
                field.type_ = Some(&type_);
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char: 'v',
                });
            }
            u::ExternalBlockItemPayload::MacroCall(_) => {
                continue;
            }
        }

        let NameGenericsReturn {
            line,
            text,
            generics,
        } = ng;
        field.line = line;
        p.push_str(&text);
        if let Some(generics) = &generics {
            field.signature = Some(generics);
        }

        field.set_access(&item.public.payload);

        p.push_str(&field.to_string());
        p.push('\n');
    }
}

fn impl_items(p: &mut String, a: &u::Impl, filepath: &str, mod_: &Vec<&str>) {
    let mut new_mod = mod_.clone();
    let type_ = to_u(&a.type_);
    new_mod.push(&type_);

    let t;
    let ctype = if !new_mod.is_empty() {
        t = new_mod.join("..");
        Some(t.as_str())
    } else {
        None
    };

    for item in &a.items {
        let mut sig = String::new();
        let ng;
        let type_;
        let mut field = TagField::default();
        field.ctype = ctype;

        match &item.payload {
            u::AssociatedItemPayload::Func(a) => {
                let generics = generics_text(&a.generics);
                if let Some(g) = generics {
                    sig.push_str(&g);
                }
                sig.push_str(&func_sig_text(&a));
                field.signature = Some(&sig);

                let type_char = if a.self_param.is_some() { 'm' } else { 'f' };
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char,
                });
            }
            u::AssociatedItemPayload::Const(a) => {
                type_ = to_u(&a.type_);
                field.type_ = Some(&type_);
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &None,
                    filepath: &filepath,
                    type_char: 'v',
                });
            }
            u::AssociatedItemPayload::TypeAlias(a) => {
                if let Some(t) = &a.type_ {
                    type_ = to_u(t);
                    field.type_ = Some(&type_);
                }
                ng = name_generics(&NameGenerics {
                    name: &a.name,
                    generics: &a.generics,
                    filepath: &filepath,
                    type_char: 'a',
                });
            }
            u::AssociatedItemPayload::MacroCall(_) => {
                continue;
            }
        }

        let NameGenericsReturn {
            line,
            text,
            generics,
        } = ng;
        field.line = line;
        p.push_str(&text);
        if let Some(generics) = &generics {
            field.signature = Some(generics);
        }

        field.set_access(&item.public.payload);

        p.push_str(&field.to_string());
        p.push('\n');
    }
}

fn nested_ctype(mod_: &Vec<&str>, a: &str, gen: &Option<u::Generics>) -> String {
    let mut new_mod = mod_.clone();
    new_mod.push(a);
    let mut r = new_mod.join("..");
    let generics = generics_text(gen);
    if let Some(b) = generics {
        r.push_str(&b);
    }
    r
}
