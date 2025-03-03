use super::lex::*;
use crate::T;
use std::convert::TryFrom;
use std::mem;

/*
HashMap<Span, Comment>
match each comment with the token preceding it, a u span or comment
*/
pub struct LangFormatter {
    buf: String,
    indent: u8,
    is_main: bool,

    span_pairs: Vec<SpanPair>,
    line: usize,
    column: usize,

    in_impl_static: bool,

    comments: Vec<Comment>,
    comment_index: usize,
}
// Clone required by rust-analyzer
#[derive(Debug, Clone)]
pub struct SpanPair {
    pub u: Span,
    pub rust: RustSpan,
}
#[derive(Debug, Clone)]
pub struct RustSpan {
    pub byte_start: usize,
    pub width: usize,
    pub line: usize,
    pub column: usize,

    #[cfg(debug_assertions)]
    pub text: String,
}

impl LangFormatter {
    pub fn new(is_main: bool, comments: Vec<Comment>) -> LangFormatter {
        LangFormatter {
            buf: String::new(),
            indent: 0,
            is_main,

            span_pairs: Vec::new(),
            line: 1,
            column: 1,

            in_impl_static: false,

            comments,
            comment_index: 0,
        }
    }

    pub fn buf(self) -> String {
        let s = self;
        s.buf
    }
    pub fn span_pairs(&self) -> &Vec<SpanPair> {
        let s = self;
        &s.span_pairs
    }
    pub fn span_pairs_move(&mut self) -> Vec<SpanPair> {
        let s = self;
        mem::take(&mut s.span_pairs)
    }

    pub fn push_rust(&mut self, a: &impl ToLang) {
        let s = self;
        a.to_rust(s);
    }
    pub fn push_u(&mut self, a: &impl ToLang) {
        let s = self;
        a.to_u(s);
    }

    /* NOTE
        for { } -> (a, b) expressions and other out of order grammars,
    comments within {} may come before {}
        this seems not to be a problem, as they don't have doc comments
    within and it's not used in macros(where order matters)
    */
    pub fn push_comment(&mut self, span: &Span) {
        let s = self;

        let start = s.comment_index;
        for _ in start..s.comments.len() {
            let comment = &s.comments[s.comment_index];
            // doc comments associated with items are converted to rust in attrs
            // so there is no need to it here
            // NOTE doc comments at the bottom of the file(following no item) will be skipped
            if comment.is_doc {
                s.comment_index += 1;
                continue;
            }
            if comment.span.line < span.line
                || (comment.span.line == span.line && comment.span.column < span.column)
            {
                let item = std::mem::take(&mut s.comments[s.comment_index]);
                item.to_rust(s);
                s.comment_index += 1;
            } else {
                break;
            }
        }
    }

    pub fn push_str(&mut self, a: &str, span: Span) {
        let s = self;

        s.push_comment(&span);
        s.span_pairs.push(SpanPair {
            u: span,
            rust: RustSpan {
                byte_start: s.buf.len(),
                width: a.len(),
                line: s.line,
                column: s.column,
                #[cfg(debug_assertions)]
                text: a.to_string(),
            },
        });

        s.buf.push_str(a);
        for c in a.chars() {
            if c == '\n' {
                s.line += 1;
                s.column = 1;
            } else {
                s.column += 1;
            }
        }
    }

    pub fn push_raw(&mut self, a: &str) {
        let s = self;
        s.buf.push_str(a);
        for c in a.chars() {
            if c == '\n' {
                s.line += 1;
                s.column = 1;
            } else {
                s.column += 1;
            }
        }
    }

    pub fn indent(&mut self) {
        let s = self;
        for _ in 0..s.indent {
            s.buf.push('\t');
        }
        s.column += s.indent as usize;
    }

    pub fn inc_indent(&mut self) {
        let s = self;
        s.indent += 1;
    }
    pub fn dec_indent(&mut self) {
        let s = self;
        s.indent -= 1;
    }
}

pub trait ToLang {
    fn to_rust(&self, p: &mut LangFormatter);
    fn to_u(&self, p: &mut LangFormatter);
}

// --------------------------------------------------------------------------------
#[derive(Debug)]
pub struct Package {
    pub shebang: Option<String>,
    pub inner_attrs: Attrs,
    pub items: Vec<Item>,
}
impl ToLang for Package {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(a) = &s.shebang {
            p.push_raw(a);
            p.push_raw("\n");
        }

        p.push_raw("#![allow(unused_mut)]\n");

        p.push_rust(&s.inner_attrs);
        if !p.is_main {
            p.push_raw("use super::*;\n\n");
        }
        for item in &s.items {
            p.push_rust(item);
        }

        // print remaining comments
        for i in p.comment_index..p.comments.len() {
            let a = mem::take(&mut p.comments[i]);
            p.push_rust(&a);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(a) = &s.shebang {
            p.push_raw(a);
            p.push_raw("\n");
        }

        p.push_u(&s.inner_attrs);
        for item in &s.items {
            p.push_u(item);
        }
    }
}

#[derive(Debug, Default)]
pub struct Visibility {
    pub span: Span,
    pub payload: VisibilityPayload,
    pub is_default: bool, // true for none
}
#[derive(Debug, Default)]
pub enum VisibilityPayload {
    #[default]
    Private,
    Pub,
    Super,
    Crate,
    SimplePath(SimplePath),
}
impl Visibility {
    pub fn new_default(line: usize) -> Visibility {
        Visibility {
            span: Span {
                line,
                column: 0,
                width: 0,
            },
            payload: VisibilityPayload::Crate,
            is_default: true,
        }
    }
}
impl ToLang for Visibility {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match &s.payload {
            VisibilityPayload::Private => {}
            VisibilityPayload::Pub => {
                p.push_str("pub", s.span);
                p.push_raw(" ");
            }
            VisibilityPayload::Super => {
                p.push_str("pub(super)", s.span);
                p.push_raw(" ");
            }
            VisibilityPayload::Crate => {
                p.push_str("pub(crate)", s.span);
                p.push_raw(" ");
            }
            VisibilityPayload::SimplePath(path) => {
                p.push_raw("pub(in ");
                p.push_rust(path);
                p.push_raw(") ");
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match &s.payload {
            VisibilityPayload::Pub => {
                p.push_str("pub", s.span);
            }
            VisibilityPayload::Private => {
                p.push_str("pub(self)", s.span);
            }
            VisibilityPayload::Super => {
                p.push_str("pub(super)", s.span);
            }
            VisibilityPayload::Crate => {
                p.push_str("pub(crate)", s.span);
            }
            VisibilityPayload::SimplePath(path) => {
                p.push_raw("in ");
                p.push_u(path);
            }
        }
        p.push_raw(" ");
    }
}

#[derive(Debug, Default)]
pub struct Item {
    pub outer_attrs: Attrs,
    pub public: Visibility,
    pub payload: ItemPayload,
}
impl ToLang for Item {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match &s.payload {
            // add #[derive(Debug)] for all struct and enum
            ItemPayload::Struct(_) | ItemPayload::Enum(_) => {
                let mut derive = true;
                for attr in &s.outer_attrs.items {
                    if attr.text == "!derive(Debug)" {
                        derive = false;
                        break;
                    }
                }
                if derive {
                    p.push_raw("#[derive(Debug)]\n");
                }
            }
            ItemPayload::Test(_) => {
                p.indent();
                p.push_raw("#[cfg(test)]\n");
            }
            _ => {}
        }
        p.push_rust(&s.outer_attrs);

        p.indent();
        if !matches!(
            &s.payload,
            ItemPayload::Test(_)
                | ItemPayload::Crate(_)
                | ItemPayload::Import(_)
                | ItemPayload::Impl(_)
                | ItemPayload::Extern(_)
                | ItemPayload::Macro(_)
                | ItemPayload::MacroCall(_)
        ) {
            p.push_rust(&s.public);
        }

        match &s.payload {
            ItemPayload::Mod(a) => p.push_rust(a),
            ItemPayload::Test(a) => p.push_rust(a),
            ItemPayload::Crate(a) => p.push_rust(a),
            ItemPayload::Extern(a) => p.push_rust(a),
            ItemPayload::Import(a) => p.push_rust(a),
            ItemPayload::Func(a) => p.push_rust(a.as_ref()),
            ItemPayload::TypeAlias(a) => p.push_rust(a),
            ItemPayload::Struct(a) => p.push_rust(a),
            ItemPayload::Enum(a) => p.push_rust(a),
            ItemPayload::Union(a) => p.push_rust(a),
            ItemPayload::Const(a) => p.push_rust(a),
            ItemPayload::Static(a) => p.push_rust(a),
            ItemPayload::Interface(a) => p.push_rust(a),
            ItemPayload::Impl(a) => p.push_rust(a),
            ItemPayload::Macro(a) => p.push_rust(a),
            ItemPayload::MacroCall(a) => {
                p.push_rust(a);
                // for macro calls in item
                if matches!(a.body, MacroCallBody::Expr(_)) {
                    p.push_raw(";");
                }
            }
        }
        p.push_raw("\n");
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.outer_attrs);

        p.indent();
        if !matches!(
            &s.payload,
            ItemPayload::Test(_)
                | ItemPayload::Crate(_)
                | ItemPayload::Import(_)
                | ItemPayload::Impl(_)
                | ItemPayload::Macro(_)
                | ItemPayload::MacroCall(_)
        ) {
            p.push_u(&s.public);
        }

        match &s.payload {
            ItemPayload::Mod(a) => p.push_u(a),
            ItemPayload::Test(a) => p.push_u(a),
            ItemPayload::Crate(a) => p.push_u(a),
            ItemPayload::Extern(a) => p.push_u(a),
            ItemPayload::Import(a) => p.push_u(a),
            ItemPayload::Func(a) => p.push_u(a.as_ref()),
            ItemPayload::TypeAlias(a) => p.push_u(a),
            ItemPayload::Struct(a) => p.push_u(a),
            ItemPayload::Enum(a) => p.push_u(a),
            ItemPayload::Union(a) => p.push_u(a),
            ItemPayload::Const(a) => p.push_u(a),
            ItemPayload::Static(a) => p.push_u(a),
            ItemPayload::Interface(a) => p.push_u(a),
            ItemPayload::Impl(a) => p.push_u(a),
            ItemPayload::Macro(a) => p.push_u(a),
            ItemPayload::MacroCall(a) => p.push_u(a),
        }
        p.push_raw("\n");
    }
}

#[derive(Debug)]
pub enum ItemPayload {
    Mod(Mod),
    Test(Test),
    Crate(Crate),
    Extern(ExternalBlock),
    Import(Import),
    Func(Box<Func>),
    TypeAlias(TypeAlias),
    Struct(Struct),
    Enum(Enum),
    Union(Union),
    Const(Const),
    Static(Static),
    Interface(Interface),
    Impl(Impl),
    Macro(Macro),
    MacroCall(MacroCall),
}
impl Default for ItemPayload {
    fn default() -> Self {
        ItemPayload::Crate(Crate::default())
    }
}

#[derive(Debug, Default)]
pub struct Mod {
    pub name: Identifier,
    pub inner_attrs: Attrs,
    pub items: Vec<Item>,

    pub span_mod: Span,
    pub span_brace_open: Option<Span>,
    pub span_brace_close: Option<Span>,
}

impl ToLang for Mod {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("mod", s.span_mod);
        p.push_raw(" ");
        p.push_rust(&s.name);
        if s.items.is_empty() {
            p.push_raw(";");
            return;
        }

        p.push_raw(" ");
        p.push_str("{", s.span_brace_open.unwrap());
        p.push_raw("\n");

        p.inc_indent();
        p.push_rust(&s.inner_attrs);
        for item in &s.items {
            p.push_rust(item);
        }
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close.unwrap());
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.name);
        p.push_raw(" mod {\n");

        p.inc_indent();
        p.push_u(&s.inner_attrs);
        for item in &s.items {
            p.push_u(item);
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}

#[derive(Debug, Default)]
pub struct Test {
    pub inner_attrs: Attrs,
    pub items: Vec<Item>,

    pub span_test: Span,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for Test {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.indent();
        p.push_str("mod __ ", s.span_test);
        p.push_str("{", s.span_brace_open);
        p.push_raw("\n");

        p.inc_indent();
        p.push_rust(&s.inner_attrs);

        p.indent();
        p.push_raw("use super::*;\n\n");

        for item in &s.items {
            p.push_rust(item);
        }
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.indent();
        p.push_raw("test {\n");

        p.inc_indent();
        p.push_u(&s.inner_attrs);

        for item in &s.items {
            p.push_u(item);
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}

#[derive(Debug, Default)]
pub struct Crate {
    pub items: Vec<CrateItem>, // items.len() > 0
}
impl ToLang for Crate {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for item in &s.items {
            p.indent();
            p.push_raw("extern crate ");
            p.push_rust(&item.name);
            if let Some(alias) = &item.alias {
                p.push_raw(" as ");
                p.push_rust(alias);
            }
            p.push_raw(";\n");
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("crate {\n");
        p.inc_indent();
        for item in &s.items {
            p.indent();
            if let Some(alias) = &item.alias {
                p.push_u(alias);
                p.push_raw(" ");
            }
            p.push_u(&item.name);
            p.push_raw("\n");
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}
#[derive(Debug)]
pub struct CrateItem {
    pub alias: Option<Identifier>,
    pub name: Identifier,
}

#[derive(Debug, Default)]
pub struct Import {
    pub items: Vec<ImportItem>, // items.len() > 0
}
impl ToLang for Import {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for item in &s.items {
            if matches!(item.public.payload, VisibilityPayload::Pub) {
                p.push_raw("pub ");
            }
            p.push_raw("use ");
            p.push_rust(item);
            p.push_raw(";\n");
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("import {\n");
        p.inc_indent();
        for item in &s.items {
            p.indent();
            p.push_u(item);
            p.push_raw("\n");
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}
#[derive(Debug, Default)]
pub struct ImportItem {
    pub public: Visibility,
    pub alias: Option<Identifier>,
    pub paths: Vec<Identifier>, // path.len() > 0
    pub sub: Vec<ImportItem>,

    pub leading_sep: Option<Span>,
}
impl ToLang for ImportItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.leading_sep.is_some() {
            p.push_raw("::");
        }

        p.push_rust(&s.paths[0]);
        for path in &s.paths[1..] {
            p.push_raw("::");
            p.push_rust(path);
        }
        if let Some(alias) = &s.alias {
            p.push_raw(" as ");
            p.push_rust(alias);
        }
        if !s.sub.is_empty() {
            p.push_raw("::{");
            for (i, sub) in s.sub.iter().enumerate() {
                if i != 0 {
                    p.push_raw(", ");
                }
                p.push_rust(sub);
            }
            p.push_raw("}");
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(alias) = &s.alias {
            p.push_u(alias);
            p.push_raw(" ");
        }
        p.push_u(&s.paths[0]);
        for path in &s.paths[1..] {
            p.push_raw("/");
            p.push_u(path);
        }
        if !s.sub.is_empty() {
            p.push_raw("{");
            for (i, sub) in s.sub.iter().enumerate() {
                if i != 0 {
                    p.push_raw(", ");
                }
                p.push_u(sub);
            }
            p.push_raw("}");
        }
    }
}

#[derive(Debug, Default)]
pub struct Func {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub qualifier: FuncQualifier,

    pub self_param: Option<SelfParam>,
    pub params: Vec<FuncParam>,
    pub variadics: Option<FuncVariadics>, // only functions in external blocks might have this
    pub return_type: Option<Type>,
    pub body: Option<BlockExpr>,

    pub span_func: Span,
    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for Func {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.qualifier.const_ {
            p.push_str("const", s.qualifier.span_const.unwrap());
            p.push_raw(" ");
        }
        if s.qualifier.async_ {
            p.push_str("async", s.qualifier.span_async.unwrap());
            p.push_raw(" ");
        }
        if s.qualifier.unsafe_ {
            p.push_str("unsafe", s.qualifier.span_unsafe.unwrap());
            p.push_raw(" ");
        }
        if s.qualifier.safe {
            p.push_str("safe", s.qualifier.span_safe.unwrap());
            p.push_raw(" ");
        }
        if let Some(extern_) = &s.qualifier.extern_ {
            p.push_str("extern", s.qualifier.span_extern.unwrap());
            p.push_raw(" ");
            if let Some(abi) = extern_ {
                abi.push_span(p, s.qualifier.span_abi.unwrap());
                p.push_raw(" ");
            }
        }

        p.push_str("fn", s.span_func);
        p.push_raw(" ");
        p.push_rust(&s.name);

        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }

        p.push_str("(", s.span_paren_open);
        if let Some(self_param) = &s.self_param {
            p.push_rust(self_param);
            if !s.params.is_empty() {
                p.push_raw(", ");
            }
        }
        for (i, param) in s.params.iter().enumerate() {
            p.push_rust(&param.outer_attrs);
            if s.body.is_some() && param.name.id != "-" {
                p.push_raw("mut ");
            }
            p.push_rust(&param.name);
            p.push_raw(": ");
            p.push_rust(&param.type_);
            if i != s.params.len() - 1 {
                p.push_raw(", ");
            }
        }
        if let Some(v) = &s.variadics {
            p.push_raw(", ");
            p.push_rust(&v.outer_attrs);
            p.push_str("...", v.span);
        }
        p.push_str(")", s.span_paren_close);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_rust(return_type);
        }

        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }

        // body
        if let Some(body) = &s.body {
            p.push_raw(" ");
            p.push_str("{", body.span_brace_open);
            p.push_raw("\n");
            p.inc_indent();
            p.push_rust(&body.inner_attrs);

            if s.self_param.is_some() && !body.stmts.is_empty() {
                p.indent();
                p.push_raw("#[allow(unused_variables)]\n");
                p.indent();
                p.push_raw("let mut s = self;\n");
            }

            for item in &body.stmts {
                p.push_rust(item);
            }
            p.dec_indent();
            p.indent();
            p.push_str("}", body.span_brace_close);
        } else {
            p.push_raw(";");
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if s.qualifier.const_ {
            p.push_raw("const ");
        }
        if s.qualifier.async_ {
            p.push_raw("async ");
        }
        if s.qualifier.unsafe_ {
            p.push_raw("unsafe ");
        }
        if s.qualifier.safe {
            p.push_raw("safe ");
        }
        if let Some(extern_) = &s.qualifier.extern_ {
            p.push_raw("extern ");
            if let Some(abi) = extern_ {
                p.push_u(abi);
            }
        }

        p.push_raw("fn ");
        p.push_u(&s.name);

        if let Some(generics) = &s.generics {
            p.push_u(generics);
        }

        p.push_raw("(");
        if let Some(self_param) = &s.self_param {
            p.push_u(self_param);
            if !s.params.is_empty() {
                p.push_raw(", ");
            }
        }
        for (i, param) in s.params.iter().enumerate() {
            p.push_u(&param.outer_attrs);
            if s.body.is_some() && param.name.id != "-" {
                p.push_raw("mut ");
            }
            p.push_u(&param.name);
            p.push_raw(": ");
            p.push_u(&param.type_);
            if i != s.params.len() - 1 {
                p.push_raw(", ");
            }
        }
        p.push_raw(")");

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_u(return_type);
        }

        if let Some(where_) = &s.where_ {
            p.push_u(where_);
        }

        // body
        if let Some(body) = &s.body {
            p.push_raw(" {\n");
            p.inc_indent();
            p.push_u(&body.inner_attrs);

            if s.self_param.is_some() && !body.stmts.is_empty() {
                p.indent();
                p.push_raw("#[allow(unused_variables)]\n");
                p.indent();
                p.push_raw("let mut s = self;\n");
            }

            for item in &body.stmts {
                p.push_u(item);
            }
            p.dec_indent();
            p.indent();
            p.push_raw("}");
        } else {
            p.push_raw(";");
        }
    }
}
#[derive(Debug, Default)]
pub struct FuncQualifier {
    pub const_: bool,
    pub async_: bool,
    pub unsafe_: bool,
    pub safe: bool,
    pub extern_: Option<Option<LiteralString>>,

    pub span_const: Option<Span>,
    pub span_async: Option<Span>,
    pub span_unsafe: Option<Span>,
    pub span_safe: Option<Span>,
    pub span_extern: Option<Span>,
    pub span_abi: Option<Span>,
}
#[derive(Debug, Default)]
pub struct SelfParam {
    pub outer_attrs: Attrs,
    pub payload: SelfParamPayload,
}
impl ToLang for SelfParam {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        match &s.payload {
            SelfParamPayload::Type(a) => {
                p.push_raw("mut self: ");
                p.push_rust(a);
            }
            SelfParamPayload::Short(a) => {
                p.push_str("&", a.span_ref);
                if let Some(label) = &a.label {
                    p.push_raw("'");
                    p.push_rust(label);
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_str("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                }
                p.push_raw("self");
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.outer_attrs);
        match &s.payload {
            SelfParamPayload::Type(a) => {
                p.push_raw("mut self: ");
                p.push_u(a);
            }
            SelfParamPayload::Short(a) => {
                p.push_raw("&");
                if let Some(label) = &a.label {
                    p.push_raw("'");
                    p.push_u(label);
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_raw("mut ");
                }
                p.push_raw("self");
            }
        }
    }
}
#[derive(Debug)]
pub enum SelfParamPayload {
    Type(Type),
    Short(SelfParamShort),
}
impl Default for SelfParamPayload {
    fn default() -> Self {
        SelfParamPayload::Short(SelfParamShort::default())
    }
}
#[derive(Debug, Default)]
pub struct SelfParamShort {
    pub label: Option<Identifier>,
    pub mut_: bool,

    pub span_ref: Span,
    pub span_mut: Option<Span>,
}
#[derive(Debug, Default)]
pub struct FuncParam {
    pub outer_attrs: Attrs,
    // pub pattern: Pattern, // I don't think using patterns for function params is neccessary
    pub name: Identifier,
    pub type_: Type,
}
#[derive(Debug)]
pub struct FuncVariadics {
    pub outer_attrs: Attrs,

    pub span: Span,
}

#[derive(Debug, Default)]
pub struct TypeAlias {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub bounds: TypeParamBounds,
    pub where_: Option<Where>,
    pub type_: Option<Type>,

    pub span_type: Span,
    pub span_eq: Option<Span>,
}
impl ToLang for TypeAlias {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("type", s.span_type);
        p.push_raw(" ");
        p.push_rust(&s.name);
        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }
        if !s.bounds.items.is_empty() {
            p.push_raw(": ");
            p.push_rust(&s.bounds);
        }
        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }
        if let Some(type_) = &s.type_ {
            p.push_raw(" ");
            p.push_str("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(type_);
        }
        p.push_raw(";");
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("type ");
        p.push_u(&s.name);
        if let Some(generics) = &s.generics {
            p.push_u(generics);
        }
        if !s.bounds.items.is_empty() {
            p.push_raw(": ");
            p.push_u(&s.bounds);
        }
        if let Some(where_) = &s.where_ {
            p.push_u(where_);
        }
        if let Some(type_) = &s.type_ {
            p.push_raw(" = ");
            p.push_u(type_);
        }
        p.push_raw(";");
    }
}

#[derive(Debug, Default)]
pub struct Struct {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub payload: Option<StructPayload>,

    pub span_struct: Span,
    pub span_brace_open: Option<Span>,
    pub span_brace_close: Option<Span>,
    pub span_paren_open: Option<Span>,
    pub span_paren_close: Option<Span>,
}
impl ToLang for Struct {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("struct", s.span_struct);
        p.push_raw(" ");
        p.push_rust(&s.name);
        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }

        if let Some(payload) = &s.payload {
            match payload {
                StructPayload::Struct(a) => {
                    if let Some(where_) = &s.where_ {
                        p.push_rust(where_);
                    }
                    p.push_raw(" ");
                    p.push_str("{", s.span_brace_open.unwrap());
                    p.push_raw("\n");

                    p.inc_indent();
                    for item in a {
                        p.push_rust(&item.outer_attrs);
                        p.indent();
                        p.push_rust(&item.public);
                        p.push_rust(&item.name);
                        p.push_raw(": ");
                        p.push_rust(&item.type_);
                        p.push_raw(",\n");
                    }
                    p.dec_indent();
                    p.indent();
                    p.push_str("}", s.span_brace_close.unwrap());
                }
                StructPayload::Tuple(a) => {
                    p.push_str("(", s.span_paren_open.unwrap());
                    for (i, item) in a.iter().enumerate() {
                        if i != 0 {
                            p.push_raw(", ");
                        }
                        p.push_rust(&item.outer_attrs);
                        p.push_rust(&item.public);
                        p.push_rust(&item.type_);
                    }
                    p.push_str(")", s.span_paren_close.unwrap());
                    if let Some(where_) = &s.where_ {
                        p.push_rust(where_);
                    }
                    p.push_raw(";");
                }
            }
        } else {
            p.push_raw(";");
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("struct ");
        p.push_u(&s.name);
        if let Some(generics) = &s.generics {
            p.push_u(generics);
        }

        if let Some(payload) = &s.payload {
            match payload {
                StructPayload::Struct(a) => {
                    if let Some(where_) = &s.where_ {
                        p.push_u(where_);
                    }
                    p.push_raw(" {\n");

                    p.inc_indent();
                    for item in a {
                        p.push_u(&item.outer_attrs);
                        p.indent();
                        p.push_u(&item.public);
                        p.push_u(&item.name);
                        p.push_raw(": ");
                        p.push_u(&item.type_);
                        p.push_raw(",\n");
                    }
                    p.dec_indent();
                    p.indent();
                    p.push_raw("}");
                }
                StructPayload::Tuple(a) => {
                    p.push_raw("(");
                    for (i, item) in a.iter().enumerate() {
                        if i != 0 {
                            p.push_raw(", ");
                        }
                        p.push_u(&item.outer_attrs);
                        p.push_u(&item.public);
                        p.push_u(&item.type_);
                    }
                    p.push_raw(")");
                    if let Some(where_) = &s.where_ {
                        p.push_u(where_);
                    }
                    p.push_raw(";");
                }
            }
        } else {
            p.push_raw(";");
        }
    }
}

#[derive(Debug)]
pub enum StructPayload {
    Struct(Vec<StructField>),
    Tuple(Vec<TupleField>),
}
#[derive(Debug, Default)]
pub struct StructField {
    pub outer_attrs: Attrs,
    pub name: Identifier,
    pub public: Visibility,
    pub type_: Type,
}
#[derive(Debug, Default)]
pub struct TupleField {
    pub outer_attrs: Attrs,
    pub public: Visibility,
    pub type_: Type,
}

#[derive(Debug, Default)]
pub struct Enum {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub items: Vec<EnumItem>,

    pub span_enum: Span,
    pub span_brace_open: Option<Span>,
    pub span_brace_close: Option<Span>,
}
impl ToLang for Enum {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("enum", s.span_enum);
        p.push_raw(" ");
        p.push_rust(&s.name);
        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }
        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }
        p.push_raw(" ");
        p.push_str("{", s.span_brace_open.unwrap());
        p.push_raw("\n");

        p.inc_indent();
        for item in &s.items {
            p.push_rust(item);
            p.push_raw(",\n");
        }
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close.unwrap());
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("enum ");
        p.push_u(&s.name);
        if let Some(generics) = &s.generics {
            p.push_u(generics);
        }
        if let Some(where_) = &s.where_ {
            p.push_u(where_);
        }
        p.push_raw(" {\n");

        p.inc_indent();
        for item in &s.items {
            p.push_u(item);
            p.push_raw(",\n");
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}
#[derive(Debug, Default)]
pub struct EnumItem {
    pub outer_attrs: Attrs,
    pub name: Identifier,
    // pub public: Visibility, // it's not needed
    pub payload: EnumItemPayload,
    pub discriminant: Option<Expr>,

    pub span_brace_open: Option<Span>,
    pub span_brace_close: Option<Span>,
    pub span_paren_open: Option<Span>,
    pub span_paren_close: Option<Span>,
    pub span_eq: Option<Span>,
}
impl ToLang for EnumItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        p.indent();
        // p.push_rust(&s.public);
        p.push_rust(&s.name);

        match &s.payload {
            EnumItemPayload::Struct(a) => {
                p.push_raw(" ");
                p.push_str("{", s.span_brace_open.unwrap());
                p.push_raw("\n");
                p.inc_indent();
                for item in a {
                    p.indent();
                    // p.push_rust(&item.public);
                    p.push_rust(&item.name);
                    p.push_raw(": ");
                    p.push_rust(&item.type_);
                    p.push_raw(",\n");
                }
                p.dec_indent();
                p.indent();
                p.push_str("}", s.span_brace_close.unwrap());
            }
            EnumItemPayload::Tuple(a) => {
                p.push_str("(", s.span_paren_open.unwrap());
                for (i, item) in a.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(&item.outer_attrs);
                    // p.push_rust(&item.public);
                    p.push_rust(&item.type_);
                }
                p.push_str(")", s.span_paren_close.unwrap());
            }
            EnumItemPayload::None => {}
        }
        if let Some(a) = &s.discriminant {
            p.push_raw(" ");
            p.push_str("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(a);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        p.indent();
        // p.push_rust(&s.public);
        p.push_rust(&s.name);

        match &s.payload {
            EnumItemPayload::Struct(a) => {
                p.push_raw(" ");
                p.push_str("{", s.span_brace_open.unwrap());
                p.push_raw("\n");
                p.inc_indent();
                for item in a {
                    p.indent();
                    // p.push_rust(&item.public);
                    p.push_rust(&item.name);
                    p.push_raw(": ");
                    p.push_rust(&item.type_);
                    p.push_raw(",\n");
                }
                p.dec_indent();
                p.indent();
                p.push_str("}", s.span_brace_close.unwrap());
            }
            EnumItemPayload::Tuple(a) => {
                p.push_str("(", s.span_paren_open.unwrap());
                for (i, item) in a.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(&item.outer_attrs);
                    // p.push_rust(&item.public);
                    p.push_rust(&item.type_);
                }
                p.push_str(")", s.span_paren_close.unwrap());
            }
            EnumItemPayload::None => {}
        }
        if let Some(a) = &s.discriminant {
            p.push_raw(" ");
            p.push_str("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(a);
        }
    }
}
#[derive(Debug, Default)]
pub enum EnumItemPayload {
    #[default]
    None,
    Struct(Vec<StructField>),
    Tuple(Vec<TupleField>),
}

#[derive(Debug, Default)]
pub struct Union {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub fields: Vec<StructField>,

    pub span_union: Span,
    pub span_brace_open: Option<Span>,
    pub span_brace_close: Option<Span>,
}
impl ToLang for Union {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("union", s.span_union);
        p.push_raw(" ");
        p.push_rust(&s.name);
        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }

        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }
        p.push_raw(" ");
        p.push_str("{", s.span_brace_open.unwrap());
        p.push_raw("\n");

        p.inc_indent();
        for item in &s.fields {
            p.push_rust(&item.outer_attrs);
            p.indent();
            p.push_rust(&item.public);
            p.push_rust(&item.name);
            p.push_raw(": ");
            p.push_rust(&item.type_);
            p.push_raw(",\n");
        }
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close.unwrap());
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("union ");
        p.push_u(&s.name);
        if let Some(generics) = &s.generics {
            p.push_u(generics);
        }

        if let Some(where_) = &s.where_ {
            p.push_u(where_);
        }
        p.push_raw(" {\n");

        p.inc_indent();
        for item in &s.fields {
            p.push_u(&item.outer_attrs);
            p.indent();
            p.push_u(&item.public);
            p.push_u(&item.name);
            p.push_raw(": ");
            p.push_u(&item.type_);
            p.push_raw(",\n");
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}

#[derive(Debug, Default)]
pub struct Const {
    pub name: Identifier,
    pub type_: Type,
    pub expr: Option<Expr>,

    pub span_const: Span,
    pub span_eq: Option<Span>,
}
impl ToLang for Const {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("const", s.span_const);
        p.push_raw(" ");
        p.push_rust(&s.name);
        p.push_raw(": ");
        p.push_rust(&s.type_);
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_str("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(expr);
        }
        p.push_raw(";");
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.name);
        p.push_raw(" const ");
        p.push_u(&s.type_);
        if let Some(expr) = &s.expr {
            p.push_raw(" = ");
            p.push_u(expr);
        }
    }
}

#[derive(Debug, Default)]
pub struct Static {
    pub name: Identifier,
    pub safe: bool,
    pub mut_: bool,
    pub type_: Type,
    pub expr: Option<Expr>,

    pub span_static: Span,
    pub span_safe: Option<Span>,
    pub span_mut: Option<Span>,
    pub span_eq: Option<Span>,
}
impl ToLang for Static {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.safe {
            p.push_str("safe", s.span_safe.unwrap());
            p.push_raw(" ");
        }
        p.push_str("static", s.span_static);
        p.push_raw(" ");
        if s.mut_ {
            p.push_str("mut", s.span_mut.unwrap());
            p.push_raw(" ");
        }
        p.push_rust(&s.name);
        p.push_raw(": ");
        p.push_rust(&s.type_);
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_str("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(expr);
        }
        p.push_raw(";");
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if s.safe {
            p.push_raw("safe");
        }
        p.push_raw(" static ");
        if s.mut_ {
            p.push_raw("mut ");
        }
        p.push_u(&s.name);
        p.push_raw(": ");
        p.push_u(&s.type_);
        if let Some(expr) = &s.expr {
            p.push_raw(" = ");
            p.push_u(expr);
        }
    }
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Interface {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,

    pub unsafe_: bool,
    pub bounds: TypeParamBounds,
    pub inner_attrs: Attrs,
    pub items: Vec<AssociatedItem>,

    pub span_interface: Span,
    pub span_unsafe: Option<Span>,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for Interface {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.unsafe_ {
            p.push_str("unsafe", s.span_unsafe.unwrap());
            p.push_raw(" ");
        }
        p.push_str("trait", s.span_interface);
        p.push_raw(" ");
        p.push_rust(&s.name);
        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }
        if !s.bounds.items.is_empty() {
            p.push_raw(": ");
            p.push_rust(&s.bounds);
        }
        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }

        p.push_raw(" ");
        p.push_str("{", s.span_brace_open);
        p.push_raw("\n");
        p.inc_indent();
        p.push_rust(&s.inner_attrs);
        for item in &s.items {
            p.push_rust(item);
        }
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if s.unsafe_ {
            p.push_raw("unsafe ");
        }
        p.push_raw("trait ");
        p.push_u(&s.name);
        if let Some(generics) = &s.generics {
            p.push_u(generics);
        }
        if !s.bounds.items.is_empty() {
            p.push_raw(": ");
            p.push_u(&s.bounds);
        }
        if let Some(where_) = &s.where_ {
            p.push_u(where_);
        }

        p.push_raw("{\n");
        p.inc_indent();
        p.push_u(&s.inner_attrs);
        for item in &s.items {
            p.push_u(item);
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}

#[derive(Debug)]
pub struct AssociatedItem {
    pub outer_attrs: Attrs,
    pub public: Visibility,
    pub payload: AssociatedItemPayload,
}
impl ToLang for AssociatedItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.indent();
        p.push_rust(&s.outer_attrs);
        // public only needed here
        if p.in_impl_static {
            if !matches!(&s.payload, AssociatedItemPayload::MacroCall(_)) {
                p.push_rust(&s.public);
            }
        }
        match &s.payload {
            AssociatedItemPayload::Func(a) => p.push_rust(a),
            AssociatedItemPayload::Const(a) => p.push_rust(a),
            AssociatedItemPayload::TypeAlias(a) => p.push_rust(a),
            AssociatedItemPayload::MacroCall(a) => {
                p.push_rust(a);
                if !matches!(a.body, MacroCallBody::Stmt(_)) {
                    p.push_raw(";");
                }
            }
        }
        p.push_raw("\n");
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.indent();
        p.push_u(&s.outer_attrs);
        match &s.payload {
            AssociatedItemPayload::Func(a) => p.push_u(a),
            AssociatedItemPayload::Const(a) => p.push_u(a),
            AssociatedItemPayload::TypeAlias(a) => p.push_u(a),
            AssociatedItemPayload::MacroCall(a) => {
                p.push_u(a);
                if !matches!(a.body, MacroCallBody::Stmt(_)) {
                    p.push_raw(";");
                }
            }
        }
        p.push_raw("\n");
    }
}

#[derive(Debug)]
pub enum AssociatedItemPayload {
    Func(Func),
    Const(Const),
    TypeAlias(TypeAlias),
    MacroCall(MacroCall),
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Impl {
    pub type_: Type,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub inner_attrs: Attrs,
    pub items: Vec<AssociatedItem>,
    pub payload: ImplPayload,

    pub span_impl: Span,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for Impl {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let ImplPayload::Interface(a) = &s.payload {
            if a.unsafe_ {
                p.push_str("unsafe", a.span_unsafe);
                p.push_raw(" ");
            }
        }
        p.push_str("impl", s.span_impl);

        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }

        p.push_raw(" ");
        match &s.payload {
            ImplPayload::Static => {
                p.push_rust(&s.type_);
            }
            ImplPayload::Interface(a) => {
                p.push_rust(&a.type_path);
                p.push_raw(" for ");
                p.push_rust(&s.type_);
            }
        }

        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }

        p.push_raw(" ");
        p.push_str("{", s.span_brace_open);
        p.push_raw("\n");
        p.inc_indent();
        p.push_rust(&s.inner_attrs);
        if matches!(&s.payload, ImplPayload::Static) {
            p.in_impl_static = true;
        }
        for item in &s.items {
            p.push_rust(item);
        }
        if p.in_impl_static {
            p.in_impl_static = false;
        }
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if let ImplPayload::Interface(a) = &s.payload {
            if a.unsafe_ {
                p.push_raw("unsafe ");
            }
        }
        p.push_raw("impl");

        if let Some(generics) = &s.generics {
            p.push_u(generics);
        }

        p.push_raw(" ");
        match &s.payload {
            ImplPayload::Static => {
                p.push_u(&s.type_);
            }
            ImplPayload::Interface(a) => {
                p.push_u(&a.type_path);
                p.push_raw(" for ");
                p.push_u(&s.type_);
            }
        }

        if let Some(where_) = &s.where_ {
            p.push_u(where_);
        }

        p.push_raw(" {\n");
        p.inc_indent();
        p.push_u(&s.inner_attrs);
        for item in &s.items {
            p.push_u(item);
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}

#[derive(Debug, Default)]
pub enum ImplPayload {
    #[default]
    Static,
    Interface(ImplInterface),
}

#[derive(Debug, Default)]
pub struct ImplInterface {
    pub unsafe_: bool,
    pub type_path: TypePath,

    pub span_unsafe: Span,
}

// --------------------------------------------------------------------------------
// as of now(rustc 1.50.0), no VisItem can appear before macro items,
// so don't generate them
#[derive(Debug, Default)]
pub struct Macro {
    pub name: Identifier,
    pub tokens: Vec<Token>,

    pub span_macro: Span,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for Macro {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("macro_rules!", s.span_macro);
        p.push_raw(" ");
        p.push_rust(&s.name);

        p.push_raw(" ");
        p.push_str("{", s.span_brace_open);
        p.push_raw("\n");
        p.inc_indent();
        p.indent();

        tokens_to_rust(&s.tokens, p);

        p.push_raw("\n");
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("macro_rules! ");
        p.push_u(&s.name);

        p.push_raw(" {\n");
        p.inc_indent();
        p.indent();

        tokens_to_rust(&s.tokens, p);

        p.push_raw("\n");
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}

pub fn tokens_to_rust(list: &[Token], p: &mut LangFormatter) {
    if list.is_empty() {
        return;
    }

    let mut last_line = list[0].span.line;
    let mut need_inc_indent = false;
    let mut square_count = 0;
    for item in list {
        if item.span.line > last_line {
            p.push_raw("\n");
            if need_inc_indent {
                p.inc_indent();
            }
            if matches!(item.code, T![")"] | T!["]"] | T!["}"] | T!["}}"]) {
                p.dec_indent();
            }
            p.indent();
        }
        need_inc_indent = matches!(item.code, T!["("] | T!["["] | T!["{"] | T!["{{"]);
        last_line = item.span.line;

        // for attrs in macro and macro call, #[ ]
        match &item.code {
            T!["#["] | T!["#!["] => {
                square_count += 1;
                p.push_rust(item);
            }
            T!["]"] => {
                if square_count == 0 {
                    p.push_rust(item);
                } else if square_count == 1 {
                    square_count -= 1;
                    p.push_rust(&Token {
                        span: item.span,
                        code: T!["}}"],
                    });
                }
            }
            _ => {
                p.push_rust(item);
            }
        }

        // next space
        p.push_raw(" ");
    }
}

#[derive(Debug, Default)]
pub struct MacroCall {
    pub path: SimplePath,
    pub body: MacroCallBody,

    pub span_macro_call: Span,
    pub span_paren_open: Option<Span>,
    pub span_paren_close: Option<Span>,
    pub span_brace_open: Option<Span>,
    pub span_brace_close: Option<Span>,
}
impl MacroCall {
    fn special_to_rust(&self, p: &mut LangFormatter) -> bool {
        let s = self;

        if s.path.items.len() == 1 {
            if let SimplePathSegment::Identifier(identifier) = &s.path.items[0] {
                match identifier.id.as_str() {
                    "d" => {
                        s.p_to_rust(p, identifier.span);
                        true
                    }
                    "u-custom-mod" => {
                        p.push_raw("\n");
                        true
                    }
                    _ => false,
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    fn p_to_rust(&self, p: &mut LangFormatter, span: Span) {
        let s = self;

        let id = Identifier::new("dbg".to_string(), span.line, span.column);
        p.push_rust(&id);
        p.push_str("!", s.span_macro_call);
        p.push_rust(&s.body);
    }
}
impl ToLang for MacroCall {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.special_to_rust(p) {
            return;
        }

        p.push_rust(&s.path);
        p.push_str("!", s.span_macro_call);
        p.push_rust(&s.body);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.path);
        p.push_raw("!");
        p.push_u(&s.body);
    }
}
#[derive(Debug)]
pub enum MacroCallBody {
    Token(MacroCallBodyToken),   // ,,{}
    Expr(Vec<Expr>),             // ,,()
    Stmt(Vec<Stmt>),             // ,,[stmt]{}
    UCustomMod(Vec<Identifier>), // u-custom-mod,,{}
}
impl Default for MacroCallBody {
    fn default() -> Self {
        MacroCallBody::Token(MacroCallBodyToken { list: Vec::new() })
    }
}
impl ToLang for MacroCallBody {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            MacroCallBody::Token(a) => {
                p.push_raw("{\n");
                p.inc_indent();
                p.indent();
                tokens_to_rust(&a.list, p);
                p.dec_indent();
                p.push_raw("\n");
                p.indent();
                p.push_raw("}");
            }
            MacroCallBody::Expr(list) => {
                p.push_raw("(");
                for (i, item) in list.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                p.push_raw(")");
            }
            MacroCallBody::Stmt(list) => {
                p.push_raw("{\n");
                p.inc_indent();
                for item in list {
                    p.push_rust(item);
                }
                p.dec_indent();
                p.indent();
                p.push_raw("}");
            }
            MacroCallBody::UCustomMod(_) => {}
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            MacroCallBody::Token(a) => {
                p.push_raw("[");
                for (i, item) in a.list.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(" ");
                    }
                    p.push_u(item);
                }
                p.push_raw("]");
            }
            MacroCallBody::Expr(list) => {
                p.push_raw("(");
                for (i, item) in list.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_u(item);
                }
                p.push_raw(")");
            }
            MacroCallBody::Stmt(list) => {
                p.push_raw("{\n");
                p.inc_indent();
                for item in list {
                    p.push_u(item);
                }
                p.dec_indent();
                p.indent();
                p.push_raw("}");
            }
            MacroCallBody::UCustomMod(_) => {}
        }
    }
}
#[derive(Debug)]
pub struct MacroCallBodyToken {
    pub list: Vec<Token>,
}
#[derive(Debug)]
pub struct MapEntry {
    pub key: Expr,
    pub value: Expr,
}

/* #[derive(Debug)]
pub struct DelimTokenTree {
    pub delim_type: DelimType,
    pub token_tree: Vec<TokenTree>,
}
#[derive(Debug)]
pub enum DelimType {
    Paren,
    Square,
    Brace,
}
#[derive(Debug)]
pub enum TokenTree {
    TokenExceptDelim(Token),
    DelimTokenTree(DelimTokenTree),
} */

// --------------------------------------------------------------------------------
/*
extern[unsafe?] Abi? {
    foo func(x i32, ...)
    identifier static mut? Type
}
*/
#[derive(Debug, Default)]
pub struct ExternalBlock {
    pub unsafe_: bool,
    pub extern_: Option<LiteralString>,
    pub inner_attrs: Attrs,
    pub items: Vec<ExternalBlockItem>,

    pub span_extern: Span,
    pub span_unsafe: Option<Span>,
    pub span_abi: Option<Span>,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for ExternalBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.indent();
        if s.unsafe_ {
            p.push_str("unsafe", s.span_unsafe.unwrap());
            p.push_raw(" ");
        }
        p.push_str("extern", s.span_extern);
        p.push_raw(" ");
        if let Some(abi) = &s.extern_ {
            abi.push_span(p, s.span_abi.unwrap());
            p.push_raw(" ");
        }
        p.push_str("{", s.span_brace_open);
        p.push_raw("\n");

        p.inc_indent();
        for item in &s.items {
            p.push_rust(item);
            p.push_raw("\n");
        }
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.indent();
        if s.unsafe_ {
            p.push_raw("unsafe ");
        }
        p.push_raw("extern ");
        if let Some(abi) = &s.extern_ {
            p.push_u(abi);
        }
        p.push_raw(" {\n");

        p.inc_indent();
        for item in &s.items {
            p.push_u(item);
            p.push_raw("\n");
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}
#[derive(Debug)]
pub struct ExternalBlockItem {
    pub outer_attrs: Attrs,
    pub public: Visibility,
    pub payload: ExternalBlockItemPayload,
}
impl ToLang for ExternalBlockItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        p.indent();
        p.push_rust(&s.public);
        match &s.payload {
            ExternalBlockItemPayload::Func(a) => p.push_rust(a),
            ExternalBlockItemPayload::Static(a) => p.push_rust(a),
            ExternalBlockItemPayload::MacroCall(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.outer_attrs);
        p.indent();
        p.push_rust(&s.public);
        match &s.payload {
            ExternalBlockItemPayload::Func(a) => p.push_u(a),
            ExternalBlockItemPayload::Static(a) => p.push_u(a),
            ExternalBlockItemPayload::MacroCall(a) => p.push_u(a),
        }
    }
}
#[derive(Debug)]
pub enum ExternalBlockItemPayload {
    Func(Func),
    Static(Static),
    MacroCall(MacroCall),
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Attrs {
    pub items: Vec<Attr>,
}
impl ToLang for Attrs {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for item in &s.items {
            p.indent();
            p.push_rust(item);
            p.push_raw("\n");
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        for item in &s.items {
            p.indent();
            p.push_u(item);
            p.push_raw("\n");
        }
    }
}

#[derive(Debug, Default)]
pub struct Attr {
    pub tokens: Vec<Token>,
    pub text: String,

    pub comment: Option<Comment>,

    pub span_start: Span,
    pub span_end: Span,
}
impl ToLang for Attr {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(doc) = &s.comment {
            p.push_comment(&doc.span);
            p.push_rust(doc);
            return;
        }

        if s.tokens.is_empty() {
            return;
        }

        if s.text.starts_with('!') {
            return;
        }

        if s.span_start.width == 3 {
            p.push_str("#![", s.span_start);
        } else {
            p.push_str("#[", s.span_start);
        }
        for token in &s.tokens {
            p.push_rust(token);
        }
        p.push_str("]", s.span_end);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;
        for token in &s.tokens {
            p.push_u(token);
        }
    }
}

pub fn attr_to_text(tokens: &[Token]) -> String {
    let mut p = LangFormatter::new(true, Vec::new());
    for token in tokens {
        token.to_rust(&mut p)
    }
    p.buf()
}

/*
#[derive(Debug, Default)]
pub struct Attr {
    pub path: SimplePath,
    pub input: Option<AttrInput>,
}
#[derive(Debug)]
pub enum AttrInput {
    DelimTokenTree(DelimTokenTree),
    ExprLiteral(ExprLiteral),
}
 */

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Generics {
    pub params: Vec<GenericParam>,

    pub span_square_open: Span,
    pub span_square_close: Span,
}
impl ToLang for Generics {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("<", s.span_square_open);
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_str(">", s.span_square_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("<");
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        p.push_raw(">");
    }
}
#[derive(Debug, Default)]
pub struct GenericParam {
    pub outer_attrs: Attrs,
    pub name: Identifier,
    pub payload: GenericParamPayload,
}
impl ToLang for GenericParam {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        match &s.payload {
            GenericParamPayload::Lifetime(a) => {
                let span = s.name.span;
                p.push_str(
                    "'",
                    Span {
                        line: span.line,
                        column: span.column - 1,
                        width: 1,
                    },
                );
                p.push_rust(&s.name);
                if !a.lifetime_bounds.is_empty() {
                    p.push_raw(": ");
                    for (i, item) in a.lifetime_bounds.iter().enumerate() {
                        if i != 0 {
                            p.push_raw(" + ");
                        }
                        p.push_rust(item);
                    }
                }
            }
            GenericParamPayload::Type(a) => {
                p.push_rust(&s.name);
                if !a.bounds.items.is_empty() {
                    p.push_raw(": ");
                    p.push_rust(&a.bounds);
                }
                if let Some(type_) = &a.type_ {
                    p.push_raw(" = ");
                    p.push_rust(type_);
                }
            }
            GenericParamPayload::Const(a) => {
                p.push_str("const", a.span_const);
                p.push_raw(" ");
                p.push_rust(&s.name);
                p.push_raw(": ");
                p.push_rust(&a.type_);
                if let Some(expr) = &a.expr {
                    p.push_raw(" = ");
                    p.push_rust(expr);
                }
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.outer_attrs);
        match &s.payload {
            GenericParamPayload::Lifetime(a) => {
                p.push_raw("'");
                p.push_u(&s.name);
                if !a.lifetime_bounds.is_empty() {
                    p.push_raw(": ");
                    for (i, item) in a.lifetime_bounds.iter().enumerate() {
                        if i != 0 {
                            p.push_raw(" + ");
                        }
                        p.push_u(&item.name);
                    }
                }
            }
            GenericParamPayload::Type(a) => {
                p.push_u(&s.name);
                if !a.bounds.items.is_empty() {
                    p.push_raw(": ");
                    p.push_u(&a.bounds);
                }
                if let Some(type_) = &a.type_ {
                    p.push_raw(" = ");
                    p.push_u(type_);
                }
            }
            GenericParamPayload::Const(a) => {
                p.push_str("const", a.span_const);
                p.push_raw(" ");
                p.push_u(&s.name);
                p.push_raw(": ");
                p.push_u(&a.type_);
                if let Some(expr) = &a.expr {
                    p.push_raw(" = ");
                    p.push_u(expr);
                }
            }
        }
    }
}
#[derive(Debug)]
pub enum GenericParamPayload {
    Lifetime(LifetimeParam),
    Type(TypeParam),
    Const(ConstParam),
}
impl Default for GenericParamPayload {
    fn default() -> Self {
        GenericParamPayload::Lifetime(LifetimeParam::default())
    }
}

#[derive(Debug, Default)]
pub struct LifetimeParam {
    pub lifetime_bounds: Vec<Lifetime>,
}
#[derive(Debug, Default)]
pub struct Lifetime {
    pub name: Identifier,
}
impl ToLang for Lifetime {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str(
            "'",
            Span {
                line: s.name.span.line,
                column: s.name.span.column - 1,
                width: 1,
            },
        );
        p.push_rust(&s.name);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("'");
        p.push_u(&s.name);
    }
}
#[derive(Debug, Default)]
pub struct TypeParam {
    pub bounds: TypeParamBounds,
    pub type_: Option<Type>,
}
// Identifier const Type
#[derive(Debug, Default)]
pub struct ConstParam {
    pub type_: Type,
    pub expr: Option<Expr>,

    pub span_const: Span,
}

#[derive(Debug, Default)]
pub struct Where {
    pub items: Vec<WhereItem>,

    pub span_where: Span,
}
impl ToLang for Where {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw(" ");
        p.push_str("where", s.span_where);
        p.push_raw(" ");
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw(" where ");
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
    }
}
#[derive(Debug)]
pub enum WhereItem {
    Lifetime(WhereItemLifetime),
    TypeBound(WhereItemTypeBound),
}
impl Default for WhereItem {
    fn default() -> Self {
        WhereItem::Lifetime(WhereItemLifetime::default())
    }
}
impl ToLang for WhereItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match &s {
            WhereItem::Lifetime(a) => {
                p.push_rust(&a.lifetime);
                p.push_raw(": ");
                for (i, item) in a.lifetime_bounds.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(" + ");
                    }
                    p.push_rust(item);
                }
            }
            WhereItem::TypeBound(a) => {
                if let Some(for_) = &a.for_lifetimes {
                    p.push_rust(for_);
                }
                p.push_rust(&a.type_);
                p.push_raw(": ");
                p.push_rust(&a.bounds);
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match &s {
            WhereItem::Lifetime(a) => {
                p.push_u(&a.lifetime);
                p.push_raw(": ");
                for (i, item) in a.lifetime_bounds.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(" + ");
                    }
                    p.push_u(item);
                }
            }
            WhereItem::TypeBound(a) => {
                if let Some(for_) = &a.for_lifetimes {
                    p.push_u(for_);
                }
                p.push_u(&a.type_);
                p.push_raw(": ");
                p.push_u(&a.bounds);
            }
        }
    }
}
#[derive(Debug, Default)]
pub struct WhereItemLifetime {
    pub lifetime: Lifetime,
    pub lifetime_bounds: Vec<Lifetime>,
}
#[derive(Debug, Default)]
pub struct WhereItemTypeBound {
    pub for_lifetimes: Option<ForLifetimes>,
    pub type_: Type,
    pub bounds: TypeParamBounds,
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct TypeParamBounds {
    pub items: Vec<TypeParamBound>,
}
impl ToLang for TypeParamBounds {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(" + ");
            }
            p.push_rust(item);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(" + ");
            }
            p.push_u(item);
        }
    }
}
#[derive(Debug)]
pub enum TypeParamBound {
    Lifetime(Lifetime),
    InterfaceBound(InterfaceBound),
    UseBound(UseBound),
}
impl ToLang for TypeParamBound {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            TypeParamBound::Lifetime(a) => p.push_rust(a),
            TypeParamBound::InterfaceBound(a) => p.push_rust(a),
            TypeParamBound::UseBound(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            TypeParamBound::Lifetime(a) => p.push_u(a),
            TypeParamBound::InterfaceBound(a) => p.push_u(a),
            TypeParamBound::UseBound(a) => p.push_u(a),
        }
    }
}

#[derive(Debug, Default)]
pub struct UseBound {
    pub items: Vec<UseBoundGenericArg>,

    pub span_use: Span,
    pub span_square_open: Span,
    pub span_square_close: Span,
}
#[derive(Debug)]
pub enum UseBoundGenericArg {
    Lifetime(Lifetime),
    Id(Identifier),
    Self_(Span),
}
impl ToLang for UseBound {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("use", s.span_use);
        p.push_str("<", s.span_square_open);
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_str(">", s.span_square_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("use", s.span_use);
        p.push_str("<", s.span_square_open);
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        p.push_str(">", s.span_square_close);
    }
}
impl ToLang for UseBoundGenericArg {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            UseBoundGenericArg::Lifetime(a) => p.push_rust(a),
            UseBoundGenericArg::Id(a) => p.push_rust(a),
            UseBoundGenericArg::Self_(a) => p.push_str("Self", *a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            UseBoundGenericArg::Lifetime(a) => p.push_rust(a),
            UseBoundGenericArg::Id(a) => p.push_rust(a),
            UseBoundGenericArg::Self_(a) => p.push_str("Self", *a),
        }
    }
}

#[derive(Debug, Default)]
pub struct InterfaceBound {
    pub unsized_: bool,
    pub for_lifetimes: Option<ForLifetimes>,
    pub type_path: TypePath,

    pub span_unsized: Option<Span>,
}
impl ToLang for InterfaceBound {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.unsized_ {
            p.push_str("?", s.span_unsized.unwrap());
        }
        if let Some(for_) = &s.for_lifetimes {
            p.push_rust(for_);
        }
        p.push_rust(&s.type_path);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if s.unsized_ {
            p.push_raw("?");
        }
        if let Some(for_) = &s.for_lifetimes {
            p.push_u(for_);
        }
        p.push_u(&s.type_path);
    }
}

#[derive(Debug, Default)]
pub struct ForLifetimes {
    pub for_: Generics,

    pub span_for: Span,
}
impl ToLang for ForLifetimes {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("for", s.span_for);
        p.push_raw(" ");
        p.push_rust(&s.for_);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("for ");
        p.push_u(&s.for_);
    }
}

// --------------------------------------------------------------------------------
// path
#[derive(Debug, Default)]
pub struct SimplePath {
    pub items: Vec<SimplePathSegment>, // items.len() > 0
}
impl ToLang for SimplePath {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw("::");
            }
            p.push_rust(item);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw("::");
            }
            p.push_u(item);
        }
    }
}
#[derive(Debug)]
pub enum SimplePathSegment {
    Identifier(Identifier),
    Super(Span),
    SelfValue(Span),
    Crate(Span),
}
impl ToLang for SimplePathSegment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            SimplePathSegment::Identifier(a) => p.push_rust(a),
            SimplePathSegment::Super(span) => p.push_str("super", *span),
            SimplePathSegment::SelfValue(span) => p.push_str("self", *span),
            SimplePathSegment::Crate(span) => p.push_str("crate", *span),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            SimplePathSegment::Identifier(a) => p.push_u(a),
            SimplePathSegment::Super(_) => p.push_raw("super"),
            SimplePathSegment::SelfValue(_) => p.push_raw("self"),
            SimplePathSegment::Crate(_) => p.push_raw("crate"),
        }
    }
}

impl TryFrom<PathIdentSegment> for SimplePathSegment {
    type Error = &'static str;

    fn try_from(p: PathIdentSegment) -> Result<Self, Self::Error> {
        match p {
            PathIdentSegment::Identifier(identifier) => {
                Ok(SimplePathSegment::Identifier(identifier))
            }
            PathIdentSegment::Super(span) => Ok(SimplePathSegment::Super(span)),
            PathIdentSegment::SelfValue(span) => Ok(SimplePathSegment::SelfValue(span)),
            PathIdentSegment::Crate(span) => Ok(SimplePathSegment::Crate(span)),
            PathIdentSegment::SelfType(_) => Err("illegal simple path segment"),
        }
    }
}

#[derive(Debug)]
pub enum PathIdentSegment {
    Identifier(Identifier),
    Super(Span),
    SelfValue(Span),
    SelfType(Span),
    Crate(Span),
}
impl Default for PathIdentSegment {
    fn default() -> Self {
        PathIdentSegment::Super(Span::default())
    }
}
impl ToLang for PathIdentSegment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PathIdentSegment::Identifier(a) => p.push_rust(a),
            PathIdentSegment::Super(span) => p.push_str("super", *span),
            PathIdentSegment::SelfValue(span) => p.push_str("self", *span),
            PathIdentSegment::SelfType(span) => p.push_str("Self", *span),
            PathIdentSegment::Crate(span) => p.push_str("crate", *span),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PathIdentSegment::Identifier(a) => p.push_u(a),
            PathIdentSegment::Super(_) => p.push_raw("super"),
            PathIdentSegment::SelfValue(_) => p.push_raw("self"),
            PathIdentSegment::SelfType(_) => p.push_raw("Self"),
            PathIdentSegment::Crate(_) => p.push_raw("crate"),
        }
    }
}

#[derive(Debug, Default)]
pub struct PathInExpr {
    pub items: Vec<PathExprSegment>, // items.len() > 0
}
impl ToLang for PathInExpr {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw("::");
            }
            p.push_rust(item);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw("::");
            }
            p.push_u(item);
        }
    }
}
#[derive(Debug, Default)]
pub struct PathExprSegment {
    pub name: PathIdentSegment,
    pub generic_args: GenericArgs,
}
impl ToLang for PathExprSegment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.name);
        if !s.generic_args.items.is_empty() {
            p.push_raw("::");
            p.push_rust(&s.generic_args);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.name);
        if !s.generic_args.items.is_empty() {
            p.push_raw("::");
            p.push_u(&s.generic_args);
        }
    }
}

#[derive(Debug)]
pub struct QualifiedPathInExpr {
    pub type_: QualifiedPathType,
    pub items: Vec<PathExprSegment>, // items.len() > 0
}
impl ToLang for QualifiedPathInExpr {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.type_);
        for item in &s.items {
            p.push_raw("::");
            p.push_rust(item);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.type_);
        for item in &s.items {
            p.push_raw("::");
            p.push_u(item);
        }
    }
}
#[derive(Debug)]
pub struct QualifiedPathInType {
    pub type_: QualifiedPathType,
    pub items: Vec<TypePathSegment>, // items.len() > 0
}
impl ToLang for QualifiedPathInType {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.type_);
        for item in &s.items {
            p.push_raw("::");
            p.push_rust(item);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.type_);
        for item in &s.items {
            p.push_raw("..");
            p.push_u(item);
        }
    }
}
#[derive(Debug, Default)]
pub struct QualifiedPathType {
    pub type_: Type,
    pub type_path: Option<TypePath>,

    pub span_square_open: Span,
    pub span_square_close: Span,
}
impl ToLang for QualifiedPathType {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("<", s.span_square_open);
        p.push_rust(&s.type_);
        if let Some(type_path) = &s.type_path {
            p.push_raw(" as ");
            p.push_rust(type_path);
        }
        p.push_str(">", s.span_square_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("[");
        p.push_u(&s.type_);
        if let Some(type_path) = &s.type_path {
            p.push_raw(".(");
            p.push_u(type_path);
            p.push_raw(")");
        }
        p.push_raw("]");
    }
}

#[derive(Debug, Default)]
pub struct TypePath {
    pub items: Vec<TypePathSegment>,
}
impl ToLang for TypePath {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw("::");
            }
            p.push_rust(item);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw("..");
            }
            p.push_u(item);
        }
    }
}
#[derive(Debug, Default)]
pub struct TypePathSegment {
    pub name: PathIdentSegment,
    pub payload: Option<TypePathSegmentPayload>,
}
impl ToLang for TypePathSegment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.name);
        if let Some(a) = &s.payload {
            match a {
                TypePathSegmentPayload::GenericArgs(a) => p.push_rust(a),
                TypePathSegmentPayload::TypePathFunc(a) => p.push_rust(a),
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.name);
        if let Some(a) = &s.payload {
            match a {
                TypePathSegmentPayload::GenericArgs(a) => p.push_u(a),
                TypePathSegmentPayload::TypePathFunc(a) => p.push_u(a),
            }
        }
    }
}
#[derive(Debug)]
pub enum TypePathSegmentPayload {
    GenericArgs(GenericArgs),
    TypePathFunc(TypePathFunc),
}
impl Default for TypePathSegmentPayload {
    fn default() -> Self {
        TypePathSegmentPayload::GenericArgs(GenericArgs::default())
    }
}

#[derive(Debug)]
pub struct TypePathFunc {
    pub inputs: Vec<Type>,
    pub return_type: Option<Type>,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for TypePathFunc {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("(", s.span_paren_open);
        for (i, item) in s.inputs.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_str(")", s.span_paren_close);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_rust(return_type);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("(");
        for (i, item) in s.inputs.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        p.push_raw(")");

        if let Some(return_type) = &s.return_type {
            p.push_raw(" ");
            p.push_u(return_type);
        }
    }
}

#[derive(Debug, Default)]
pub struct GenericArgs {
    pub items: Vec<GenericArg>,

    pub span_square_open: Span,
    pub span_square_close: Span,
}
impl ToLang for GenericArgs {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("<", s.span_square_open);
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_str(">", s.span_square_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("[");
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        p.push_raw("]");
    }
}

#[derive(Debug)]
pub enum GenericArg {
    Lifetime(Lifetime),
    Type(Type),
    Const(GenericArgsConst),
    Binding(GenericArgsBinding),
    Bounds(GenericArgsBounds),
}
impl ToLang for GenericArg {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            GenericArg::Lifetime(a) => p.push_rust(a),
            GenericArg::Type(a) => p.push_rust(a),
            GenericArg::Const(a) => p.push_rust(a),
            GenericArg::Binding(a) => {
                p.push_rust(&a.name);
                p.push_raw(" ");
                p.push_str("=", a.span_eq);
                p.push_raw(" ");
                p.push_rust(&a.type_);
            }
            GenericArg::Bounds(a) => {
                p.push_rust(&a.name);
                p.push_raw(": ");
                p.push_rust(&a.bounds);
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            GenericArg::Lifetime(a) => p.push_u(a),
            GenericArg::Type(a) => p.push_u(a),
            GenericArg::Const(a) => p.push_u(a),
            GenericArg::Binding(a) => {
                p.push_u(&a.name);
                p.push_raw(" = ");
                p.push_u(&a.type_);
            }
            GenericArg::Bounds(a) => {
                p.push_u(&a.name);
                p.push_raw(": ");
                p.push_u(&a.bounds);
            }
        }
    }
}
#[derive(Debug)]
pub enum GenericArgsConst {
    BlockExpr(BlockExpr),
    LiteralExpr(ExprLiteral),
    MinusLiteralExpr(ExprLiteral, Span),
}
impl ToLang for GenericArgsConst {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            GenericArgsConst::BlockExpr(a) => p.push_rust(a),
            GenericArgsConst::LiteralExpr(a) => p.push_rust(a),
            GenericArgsConst::MinusLiteralExpr(a, _) => {
                p.push_raw("-");
                p.push_rust(a);
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            GenericArgsConst::BlockExpr(a) => p.push_u(a),
            GenericArgsConst::LiteralExpr(a) => p.push_u(a),
            GenericArgsConst::MinusLiteralExpr(a, _) => p.push_u(a),
        }
    }
}
#[derive(Debug)]
pub struct GenericArgsBinding {
    pub name: Identifier,
    pub type_: Type,

    pub span_eq: Span,
}
#[derive(Debug)]
pub struct GenericArgsBounds {
    pub name: Identifier,
    pub bounds: TypeParamBounds,
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Pattern {
    pub items: Vec<PatternNoTopAlt>,
}
impl ToLang for Pattern {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(" | ");
            }
            p.push_rust(item);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(" | ");
            }
            p.push_rust(item);
        }
    }
}

#[derive(Debug)]
pub enum PatternNoTopAlt {
    WithoutRange(PatternWithoutRange),
    WithRange(PatternRange),
}
impl Default for PatternNoTopAlt {
    fn default() -> Self {
        PatternNoTopAlt::WithoutRange(PatternWithoutRange::Rest(Span::default()))
    }
}
impl ToLang for PatternNoTopAlt {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternNoTopAlt::WithoutRange(a) => p.push_rust(a),
            PatternNoTopAlt::WithRange(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternNoTopAlt::WithoutRange(a) => p.push_u(a),
            PatternNoTopAlt::WithRange(a) => p.push_u(a),
        }
    }
}

#[derive(Debug)]
pub enum PatternWithoutRange {
    Literal(PatternLiteral),
    Identifier(PatternIdentifier),
    // Wildcard,
    Rest(Span),
    Obsolete(PatternRange), // must have end range
    Reference(PatternReference),
    Struct(PatternStruct),
    TupleStruct(PatternTupleStruct),
    Tuple(PatternTuple),
    Grouped(PatternGrouped),
    Slice(PatternSlice),
    Path(PatternPath),
    MacroCall(MacroCall),
}
impl ToLang for PatternWithoutRange {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternWithoutRange::Literal(a) => p.push_rust(a),
            PatternWithoutRange::Identifier(a) => {
                if a.ref_ {
                    p.push_str("ref", a.span_ref.unwrap());
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_str("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                }
                p.push_rust(&a.name);
                if let Some(pattern) = &a.pattern {
                    p.push_raw(" ");
                    p.push_str("@", a.span_at.unwrap());
                    p.push_raw(" ");
                    p.push_rust(pattern.as_ref());
                }
            }
            PatternWithoutRange::Rest(span) => p.push_str("..", *span),
            PatternWithoutRange::Obsolete(a) => {
                if let Some(start) = &a.start {
                    p.push_rust(start);
                }
                p.push_str("...", a.span);
                if let Some(end) = &a.end {
                    p.push_rust(end);
                }
            }
            PatternWithoutRange::Reference(a) => {
                p.push_str("&", a.span_ref);
                if a.mut_ {
                    p.push_str("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                }
                p.push_rust(a.pattern_without_range.as_ref());
            }
            PatternWithoutRange::Struct(a) => {
                p.push_rust(&a.path);
                p.push_str("{", a.span_brace_open);
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                p.push_str("}", a.span_brace_close);
            }
            PatternWithoutRange::TupleStruct(a) => {
                p.push_rust(&a.path);
                p.push_str("(", a.span_paren_open);
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                p.push_str(")", a.span_paren_close);
            }
            PatternWithoutRange::Tuple(a) => {
                p.push_str("(", a.span_paren_open);
                match &a.payload {
                    PatternTuplePayload::Patterns(b) => {
                        for (i, item) in b.iter().enumerate() {
                            if i != 0 {
                                p.push_raw(", ");
                            }
                            p.push_rust(item);
                        }
                        if b.len() == 1 {
                            p.push_raw(", ");
                        }
                    }
                    PatternTuplePayload::Rest(span) => {
                        p.push_raw(" ");
                        p.push_str("..", *span);
                        p.push_raw(" ");
                    }
                }
                p.push_str(")", a.span_paren_close);
            }
            PatternWithoutRange::Grouped(a) => {
                p.push_str("(", a.span_paren_open);
                p.push_rust(a.pattern.as_ref());
                p.push_str(")", a.span_paren_close);
            }
            PatternWithoutRange::Slice(a) => {
                p.push_str("[", a.span_square_open);
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                p.push_str("]", a.span_square_close);
            }
            PatternWithoutRange::Path(a) => match a {
                PatternPath::Path(b) => p.push_rust(b),
                PatternPath::QualifiedPath(b) => p.push_rust(b),
            },
            PatternWithoutRange::MacroCall(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternWithoutRange::Literal(a) => p.push_u(a),
            PatternWithoutRange::Identifier(a) => {
                if a.ref_ {
                    p.push_raw("ref ");
                }
                if a.mut_ {
                    p.push_raw("mut ");
                }
                p.push_u(&a.name);
                if let Some(pattern) = &a.pattern {
                    p.push_raw(" @ ");
                    p.push_u(pattern.as_ref());
                }
            }
            PatternWithoutRange::Rest(_) => p.push_raw(".."),
            PatternWithoutRange::Obsolete(a) => {
                if let Some(start) = &a.start {
                    p.push_u(start);
                }
                p.push_raw("...");
                if let Some(end) = &a.end {
                    p.push_rust(end);
                }
            }
            PatternWithoutRange::Reference(a) => {
                p.push_raw("&");
                if a.mut_ {
                    p.push_raw("mut ");
                }
                p.push_u(a.pattern_without_range.as_ref());
            }
            PatternWithoutRange::Struct(a) => {
                p.push_u(&a.path);
                p.push_raw("{");
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_u(item);
                }
                p.push_raw("}");
            }
            PatternWithoutRange::TupleStruct(a) => {
                p.push_u(&a.path);
                p.push_raw("(");
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_u(item);
                }
                p.push_raw(")");
            }
            PatternWithoutRange::Tuple(a) => {
                p.push_raw("(");
                match &a.payload {
                    PatternTuplePayload::Patterns(b) => {
                        for (i, item) in b.iter().enumerate() {
                            if i != 0 {
                                p.push_raw(", ");
                            }
                            p.push_u(item);
                        }
                        if b.len() == 1 {
                            p.push_raw(", ");
                        }
                    }
                    PatternTuplePayload::Rest(_) => {
                        p.push_raw(" .. ");
                    }
                }
                p.push_raw(")");
            }
            PatternWithoutRange::Grouped(a) => {
                p.push_raw("(");
                p.push_u(a.pattern.as_ref());
                p.push_raw(")");
            }
            PatternWithoutRange::Slice(a) => {
                p.push_raw("[");
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_u(item);
                }
                p.push_raw("]");
            }
            PatternWithoutRange::Path(a) => match a {
                PatternPath::Path(b) => p.push_u(b),
                PatternPath::QualifiedPath(b) => p.push_u(b),
            },
            PatternWithoutRange::MacroCall(a) => p.push_u(a),
        }
    }
}
#[derive(Debug)]
pub struct PatternLiteral {
    pub name: Literal,
    pub minus: bool,

    pub span_minus: Option<Span>,
}
impl ToLang for PatternLiteral {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.minus {
            p.push_str("-", s.span_minus.unwrap());
        }
        p.push_rust(&s.name);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if s.minus {
            p.push_raw("-");
        }
        p.push_u(&s.name);
    }
}
#[derive(Debug, Default)]
pub struct PatternIdentifier {
    pub ref_: bool,
    pub mut_: bool,
    pub name: Identifier,
    pub pattern: Option<Box<Pattern>>,

    pub span_ref: Option<Span>,
    pub span_mut: Option<Span>,
    pub span_at: Option<Span>,
}
#[derive(Debug)]
pub struct PatternReference {
    pub multi_ref: bool,
    pub mut_: bool,
    pub pattern_without_range: Box<PatternWithoutRange>,

    pub span_ref: Span,
    pub span_mut: Option<Span>,
}

#[derive(Debug, Default)]
pub struct PatternStruct {
    pub path: PathInExpr,
    pub items: Vec<PatternStructItem>,

    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
#[derive(Debug)]
pub enum PatternStructItem {
    Field(PatternStructField),
    Etc(Attrs, Span),
}
impl Default for PatternStructItem {
    fn default() -> Self {
        PatternStructItem::Etc(Attrs::default(), Span::default())
    }
}
impl ToLang for PatternStructItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternStructItem::Field(f) => {
                p.push_rust(&f.outer_attrs);
                match &f.payload {
                    PatternStructFieldPayload::A(a) => {
                        p.push_str(&a.tuple_index, a.span_tuple_index);
                        p.push_raw(": ");
                        p.push_rust(&a.pattern);
                    }
                    PatternStructFieldPayload::B(b) => {
                        p.push_rust(&b.name);
                        p.push_raw(": ");
                        p.push_rust(&b.pattern);
                    }
                    PatternStructFieldPayload::C(c) => {
                        if c.ref_ {
                            p.push_str("ref", c.span_ref.unwrap());
                            p.push_raw(" ");
                        }
                        if c.mut_ {
                            p.push_str("mut", c.span_mut.unwrap());
                            p.push_raw(" ");
                        }
                        p.push_rust(&c.name);
                    }
                }
            }
            PatternStructItem::Etc(a, span) => {
                p.push_rust(a);
                p.push_str("..", *span);
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternStructItem::Field(f) => {
                p.push_u(&f.outer_attrs);
                match &f.payload {
                    PatternStructFieldPayload::A(a) => {
                        p.push_raw(&a.tuple_index);
                        p.push_raw(": ");
                        p.push_u(&a.pattern);
                    }
                    PatternStructFieldPayload::B(b) => {
                        p.push_u(&b.name);
                        p.push_raw(": ");
                        p.push_u(&b.pattern);
                    }
                    PatternStructFieldPayload::C(c) => {
                        if c.ref_ {
                            p.push_raw("ref ");
                        }
                        if c.mut_ {
                            p.push_raw("mut ");
                        }
                        p.push_u(&c.name);
                    }
                }
            }
            PatternStructItem::Etc(a, _) => {
                p.push_u(a);
                p.push_raw("..");
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct PatternStructField {
    pub outer_attrs: Attrs,
    pub payload: PatternStructFieldPayload,
}
#[derive(Debug)]
pub enum PatternStructFieldPayload {
    A(PatternStructFieldA),
    B(PatternStructFieldB),
    C(PatternStructFieldC),
}
impl Default for PatternStructFieldPayload {
    fn default() -> Self {
        PatternStructFieldPayload::C(PatternStructFieldC::default())
    }
}
#[derive(Debug)]
pub struct PatternStructFieldA {
    pub tuple_index: String,
    pub pattern: Pattern,

    pub span_tuple_index: Span,
}
#[derive(Debug)]
pub struct PatternStructFieldB {
    pub name: Identifier,
    pub pattern: Pattern,
}
#[derive(Debug, Default)]
pub struct PatternStructFieldC {
    pub ref_: bool,
    pub mut_: bool,
    pub name: Identifier,

    pub span_ref: Option<Span>,
    pub span_mut: Option<Span>,
}

#[derive(Debug, Default)]
pub struct PatternTupleStruct {
    pub path: PathInExpr,
    pub items: Vec<Pattern>,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
#[derive(Debug)]
pub struct PatternTuple {
    pub payload: PatternTuplePayload,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
#[derive(Debug)]
pub enum PatternTuplePayload {
    Patterns(Vec<Pattern>), // p.len() > 0
    Rest(Span),
}
#[derive(Debug)]
pub struct PatternGrouped {
    pub pattern: Box<Pattern>,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
#[derive(Debug)]
pub struct PatternSlice {
    pub items: Vec<Pattern>,

    pub span_square_open: Span,
    pub span_square_close: Span,
}
#[derive(Debug)]
pub enum PatternPath {
    Path(PathInExpr),
    QualifiedPath(QualifiedPathInExpr),
}

#[derive(Debug)]
pub struct PatternRange {
    pub start: Option<PatternRangeBound>,
    pub end: Option<PatternRangeBound>,
    pub inclusive: bool,

    pub span: Span,
}
impl ToLang for PatternRange {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(start) = &s.start {
            p.push_rust(start);
        }
        if s.inclusive {
            p.push_str("..=", s.span);
        } else {
            p.push_str("..", s.span);
        }
        if let Some(end) = &s.end {
            p.push_rust(end);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(start) = &s.start {
            p.push_u(start);
        }
        if s.inclusive {
            p.push_str("..=", s.span);
        } else {
            p.push_str("..", s.span);
        }
        if let Some(end) = &s.end {
            p.push_u(end);
        }
    }
}
#[derive(Debug)]
pub enum PatternRangeBound {
    Literal(PatternLiteral),
    Path(PathInExpr),
    QualifiedPath(QualifiedPathInExpr),
}
impl ToLang for PatternRangeBound {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternRangeBound::Literal(a) => p.push_rust(a),
            PatternRangeBound::Path(a) => p.push_rust(a),
            PatternRangeBound::QualifiedPath(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternRangeBound::Literal(a) => p.push_u(a),
            PatternRangeBound::Path(a) => p.push_u(a),
            PatternRangeBound::QualifiedPath(a) => p.push_u(a),
        }
    }
}

// --------------------------------------------------------------------------------
#[derive(Debug)]
pub enum Type {
    TypeNoBounds(Box<TypeNoBounds>),
    ImplInterface(TypeParamBounds, Span),
    InterfaceObject(TypeInterfaceObject),
}
impl Default for Type {
    fn default() -> Self {
        Type::ImplInterface(TypeParamBounds::default(), Span::default())
    }
}
impl ToLang for Type {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            Type::TypeNoBounds(a) => p.push_rust(a.as_ref()),
            Type::ImplInterface(a, span) => {
                p.push_str("impl", *span);
                p.push_raw(" ");
                p.push_rust(a);
            }
            Type::InterfaceObject(a) => {
                p.push_str("dyn", a.span);
                p.push_raw(" ");
                p.push_rust(&a.bounds);
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            Type::TypeNoBounds(a) => p.push_u(a.as_ref()),
            Type::ImplInterface(a, _) => {
                p.push_raw("impl ");
                p.push_u(a);
            }
            Type::InterfaceObject(a) => {
                p.push_raw("dyn ");
                p.push_u(&a.bounds);
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeNoBounds {
    Parenthesized(TypeParenthesized),                     // ( Type )
    ImplInterfaceOneBound(TypeImplInterfaceOneBound),     // impl TraitBound
    InterfaceObjectOneBound(TypeInterfaceObjectOneBound), // dyn TraitBound
    Tuple(TypeTuple),                                     // () | ( (Type ,)+ Type? )
    RawPointer(TypeRawPointer),                           // *(mut | const) TypeNoBounds
    Reference(TypeReference),                             // & Lifetime? mut? TypeNoBounds
    Array(TypeArray),                                     // {{ Type ; Expression }}
    Slice(TypeSlice),                                     // {{ Type }}
    TypePath(TypePath),                                   // TypePathSegment(..TypePathSegment)*
    QualifiedPathInType(QualifiedPathInType), // [ Type.(TypePath)? ]..(TypePathSegment)+
    Never(Span),                              // !
    BareFunction(Box<TypeBareFunction>),      // ForLifetimes? Qualifers func(Param?) ReturnType
    MacroCall(MacroCall),                     // SimplePath,, DelimTokenTree
}
impl Default for TypeNoBounds {
    fn default() -> Self {
        TypeNoBounds::Never(Span::default())
    }
}
impl ToLang for TypeNoBounds {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            TypeNoBounds::Parenthesized(a) => {
                p.push_str("(", a.span_paren_open);
                p.push_rust(&a.type_);
                p.push_str(")", a.span_paren_close);
            }
            TypeNoBounds::ImplInterfaceOneBound(a) => {
                p.push_str("impl", a.span_impl);
                p.push_raw(" ");
                p.push_rust(&a.bound);
            }
            TypeNoBounds::InterfaceObjectOneBound(a) => {
                p.push_str("dyn", a.span_dyn);
                p.push_raw(" ");
                p.push_rust(&a.bound);
            }
            TypeNoBounds::Tuple(a) => {
                p.push_str("(", a.span_paren_open);
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                if a.items.len() == 1 {
                    p.push_raw(", ");
                }
                p.push_str(")", a.span_paren_close);
            }
            TypeNoBounds::RawPointer(a) => {
                p.push_str("*", a.span_pointer);
                if a.mut_ {
                    p.push_str("mut", a.span_mut);
                } else {
                    p.push_str("const", a.span_mut);
                }
                p.push_raw(" ");
                p.push_rust(&*a.type_no_bounds);
            }
            TypeNoBounds::Reference(a) => {
                p.push_str("&", a.span_ref);
                if let Some(lifetime) = &a.lifetime {
                    p.push_rust(lifetime);
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_str("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                }
                p.push_rust(&*a.type_no_bounds);
            }
            TypeNoBounds::Array(a) => {
                p.push_str("[", a.span_double_brace_open);
                p.push_rust(&a.type_);
                p.push_str(";", a.span_semicolon);
                p.push_raw(" ");
                p.push_rust(&a.expr);
                p.push_str("]", a.span_double_brace_close);
            }
            TypeNoBounds::Slice(a) => {
                p.push_str("[", a.span_double_brace_open);
                p.push_rust(&a.type_);
                p.push_str("]", a.span_double_brace_close);
            }
            TypeNoBounds::TypePath(a) => p.push_rust(a),
            TypeNoBounds::QualifiedPathInType(a) => p.push_rust(a),
            TypeNoBounds::Never(span) => p.push_str("!", *span),
            TypeNoBounds::BareFunction(a) => p.push_rust(a.as_ref()),
            TypeNoBounds::MacroCall(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            TypeNoBounds::Parenthesized(a) => {
                p.push_raw("(");
                p.push_u(&a.type_);
                p.push_raw(")");
            }
            TypeNoBounds::ImplInterfaceOneBound(a) => {
                p.push_raw("impl ");
                p.push_u(&a.bound);
            }
            TypeNoBounds::InterfaceObjectOneBound(a) => {
                p.push_raw("dyn ");
                p.push_u(&a.bound);
            }
            TypeNoBounds::Tuple(a) => {
                p.push_raw("(");
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_u(item);
                }
                if a.items.len() == 1 {
                    p.push_raw(", ");
                }
                p.push_raw(")");
            }
            TypeNoBounds::RawPointer(a) => {
                p.push_raw("*");
                if a.mut_ {
                    p.push_raw("mut ");
                } else {
                    p.push_raw("const ");
                }
                p.push_u(&*a.type_no_bounds);
            }
            TypeNoBounds::Reference(a) => {
                p.push_raw("&");
                if let Some(lifetime) = &a.lifetime {
                    p.push_u(lifetime);
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_raw("mut ");
                }
                p.push_u(&*a.type_no_bounds);
            }
            TypeNoBounds::Array(a) => {
                p.push_raw("{{");
                p.push_u(&a.type_);
                p.push_raw(": ");
                p.push_u(&a.expr);
                p.push_raw("}}");
            }
            TypeNoBounds::Slice(a) => {
                p.push_raw("{{");
                p.push_u(&a.type_);
                p.push_raw("}}");
            }
            TypeNoBounds::TypePath(a) => p.push_u(a),
            TypeNoBounds::QualifiedPathInType(a) => p.push_u(a),
            TypeNoBounds::Never(_) => p.push_raw("!"),
            TypeNoBounds::BareFunction(a) => p.push_u(a.as_ref()),
            TypeNoBounds::MacroCall(a) => p.push_u(a),
        }
    }
}

#[derive(Debug, Default)]
pub struct TypeInterfaceObject {
    pub bounds: TypeParamBounds,

    pub span: Span,
}

#[derive(Debug)]
pub struct TypeParenthesized {
    pub type_: Type,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}

#[derive(Debug)]
pub struct TypeImplInterfaceOneBound {
    pub bound: InterfaceBound,

    pub span_impl: Span,
}

#[derive(Debug)]
pub struct TypeInterfaceObjectOneBound {
    pub bound: InterfaceBound,

    pub span_dyn: Span,
}

#[derive(Debug)]
pub struct TypeTuple {
    pub items: Vec<Type>,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}

#[derive(Debug)]
pub struct TypeRawPointer {
    pub mut_: bool,
    pub type_no_bounds: Box<TypeNoBounds>,

    pub span_pointer: Span,
    pub span_mut: Span,
}

#[derive(Debug, Default)]
pub struct TypeReference {
    pub lifetime: Option<Lifetime>,
    pub mut_: bool,
    pub type_no_bounds: Box<TypeNoBounds>,

    pub span_ref: Span,
    pub span_mut: Option<Span>,
}

#[derive(Debug)]
pub struct TypeArray {
    pub type_: Type,
    pub expr: Expr,

    pub span_double_brace_open: Span,
    pub span_double_brace_close: Span,
    pub span_semicolon: Span,
}

#[derive(Debug)]
pub struct TypeSlice {
    pub type_: Type,

    pub span_double_brace_open: Span,
    pub span_double_brace_close: Span,
}

#[derive(Debug, Default)]
pub struct TypeBareFunction {
    pub for_lifetimes: Option<ForLifetimes>,
    pub func_type: FuncType,
    pub qualifier: FuncTypeQualifier,
    pub params: Vec<MaybeNamedParam>,
    pub return_type: Option<BareFunctionReturnType>,
    pub variadics: Option<MaybeNamedParamVariadics>,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for TypeBareFunction {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(for_) = &s.for_lifetimes {
            p.push_rust(for_);
        }

        if s.qualifier.unsafe_ {
            p.push_str("unsafe", s.qualifier.span_unsafe.unwrap());
            p.push_raw(" ");
        }
        if let Some(extern_) = &s.qualifier.extern_ {
            p.push_str("extern", s.qualifier.span_extern.unwrap());
            p.push_raw(" ");
            if let Some(abi) = extern_ {
                abi.push_span(p, s.qualifier.span_abi.unwrap());
                p.push_raw(" ");
            }
        }

        match s.func_type {
            FuncType::Func(span) => p.push_str("fn", span),
            FuncType::Other => {}
        }

        p.push_str("(", s.span_paren_open);
        for (i, param) in s.params.iter().enumerate() {
            p.push_rust(&param.outer_attrs);
            if let Some(name) = &param.name {
                p.push_rust(name);
                p.push_raw(": ");
            }
            p.push_rust(&param.type_);
            if i != s.params.len() - 1 {
                p.push_raw(", ");
            }
        }
        if let Some(v) = &s.variadics {
            p.push_raw(", ");
            p.push_rust(&v.outer_attrs);
            p.push_str("...", v.span);
        }
        p.push_str(")", s.span_paren_close);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_rust(&*return_type.0);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(for_) = &s.for_lifetimes {
            p.push_u(for_);
        }

        match s.func_type {
            FuncType::Func(_) => p.push_raw("func"),
            FuncType::Other => {}
        }

        if s.qualifier.unsafe_ {
            p.push_raw("unsafe ");
        }
        if let Some(extern_) = &s.qualifier.extern_ {
            p.push_raw("extern ");
            if let Some(abi) = extern_ {
                p.push_u(abi);
            }
        }

        p.push_raw("(");
        for (i, param) in s.params.iter().enumerate() {
            p.push_u(&param.outer_attrs);
            if let Some(name) = &param.name {
                p.push_u(name);
                p.push_raw(" ");
            }
            p.push_u(&param.type_);
            if i != s.params.len() - 1 {
                p.push_raw(", ");
            }
        }
        p.push_raw(")");

        if let Some(return_type) = &s.return_type {
            p.push_raw(" ");
            p.push_u(&*return_type.0);
        }
    }
}
#[derive(Debug, Default)]
pub enum FuncType {
    #[default]
    Other,
    Func(Span),
}
#[derive(Debug, Default)]
pub struct FuncTypeQualifier {
    pub unsafe_: bool,
    pub extern_: Option<Option<LiteralString>>,

    pub span_unsafe: Option<Span>,
    pub span_extern: Option<Span>,
    pub span_abi: Option<Span>,
}
#[derive(Debug, Default)]
pub struct MaybeNamedParam {
    pub outer_attrs: Attrs,
    pub name: Option<Identifier>,
    pub type_: Type,
}
#[derive(Debug)]
pub struct MaybeNamedParamVariadics {
    pub outer_attrs: Attrs,

    pub span: Span,
}

#[derive(Debug)]
pub struct BareFunctionReturnType(pub Box<TypeNoBounds>);

// --------------------------------------------------------------------------------
#[derive(Debug)]
pub enum Stmt {
    Item(Item),
    Let(StmtLet),
    Expr(Expr),
    Value(StmtValue),
    // MacroCall(MacroCall),
}
impl ToLang for Stmt {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match &s {
            Stmt::Item(a) => p.push_rust(a),
            Stmt::Let(a) => p.push_rust(a),
            Stmt::Expr(a) => {
                p.indent();
                p.push_rust(a);
                if matches!(a.payload, ExprPayload::WithoutBlock(_)) {
                    p.push_raw(";");
                }
                p.push_raw("\n");
            }
            Stmt::Value(a) => {
                p.indent();
                p.push_rust(a);
                p.push_raw("\n");
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match &s {
            Stmt::Item(a) => p.push_u(a),
            Stmt::Let(a) => p.push_u(a),
            Stmt::Expr(a) => {
                p.indent();
                p.push_u(a);
                if matches!(a.payload, ExprPayload::WithoutBlock(_)) {
                    p.push_raw(";");
                }
                p.push_raw("\n");
            }
            Stmt::Value(a) => {
                p.indent();
                p.push_u(a);
                p.push_raw("\n");
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct StmtLet {
    pub outer_attrs: Attrs,
    pub pattern: Pattern,
    pub type_: Option<Type>,
    pub expr: Option<Expr>,
    pub else_: Option<BlockExpr>,

    pub span_let: Span,
    pub span_eq: Option<Span>,
    pub span_else: Option<Span>,
}
impl ToLang for StmtLet {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        p.indent();
        p.push_str("let", s.span_let);
        p.push_raw(" ");
        p.push_rust(&s.pattern);
        if let Some(type_) = &s.type_ {
            p.push_raw(": ");
            p.push_rust(type_);
        }
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_str("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(expr);
        }
        if let Some(else_) = &s.else_ {
            p.push_raw(" ");
            p.push_str("else", s.span_else.unwrap());
            p.push_raw(" ");
            p.push_rust(else_);
        }
        p.push_raw(";\n");
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.outer_attrs);
        p.indent();
        p.push_u(&s.pattern);
        p.push_raw(" let");
        if let Some(type_) = &s.type_ {
            p.push_raw(" ");
            p.push_u(type_);
        }
        if let Some(expr) = &s.expr {
            p.push_raw(" = ");
            p.push_u(expr);
        }
        if let Some(else_) = &s.else_ {
            p.push_raw(" else ");
            p.push_u(else_);
        }
        p.push_raw("\n");
    }
}

#[derive(Debug)]
pub struct StmtValue {
    pub outer_attrs: Attrs,
    pub expr: Expr,
}
impl ToLang for StmtValue {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.expr);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.expr);
    }
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Expr {
    pub outer_attrs: Attrs,
    pub payload: ExprPayload,
}
impl ToLang for Expr {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        match &s.payload {
            ExprPayload::WithBlock(a) => p.push_rust(a.as_ref()),
            ExprPayload::WithoutBlock(a) => p.push_rust(a.as_ref()),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.outer_attrs);
        match &s.payload {
            ExprPayload::WithBlock(a) => p.push_u(a.as_ref()),
            ExprPayload::WithoutBlock(a) => p.push_u(a.as_ref()),
        }
    }
}

#[derive(Debug)]
pub enum ExprPayload {
    WithBlock(Box<ExprWithBlock>),
    WithoutBlock(Box<ExprWithoutBlock>),
}
impl Default for ExprPayload {
    fn default() -> Self {
        ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Literal(ExprLiteral(
            Literal::default(),
        ))))
    }
}

#[derive(Debug, Default)]
pub struct BlockExpr {
    pub inner_attrs: Attrs,
    pub label: Option<Identifier>,
    pub stmts: Vec<Stmt>,

    pub span_label: Option<Span>,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for BlockExpr {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(label) = &s.label {
            p.push_str("'", s.span_label.unwrap());
            p.push_rust(label);
            p.push_raw(": ");
        }

        p.push_str("{", s.span_brace_open);
        p.push_raw("\n");
        p.inc_indent();
        p.push_rust(&s.inner_attrs);
        for item in &s.stmts {
            p.push_rust(item);
        }
        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(label) = &s.label {
            p.push_raw("'");
            p.push_u(label);
            p.push_raw(": ");
        }

        p.push_raw("{\n");
        p.inc_indent();
        p.push_u(&s.inner_attrs);
        for item in &s.stmts {
            p.push_u(item);
        }
        p.dec_indent();
        p.indent();
        p.push_raw("}");
    }
}

#[derive(Debug)]
pub enum ExprWithBlock {
    BlockExpr(BlockExpr),
    AsyncBlock(ExprAsyncBlock),
    UnsafeBlock(ExprUnsafeBlock),
    ConstBlock(ExprConstBlock),
    Loop(Box<ExprLoop>),
    If(ExprIf),
    Match(ExprMatch),
}
impl ToLang for ExprWithBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprWithBlock::BlockExpr(a) => p.push_rust(a),
            ExprWithBlock::AsyncBlock(a) => p.push_rust(a),
            ExprWithBlock::UnsafeBlock(a) => p.push_rust(a),
            ExprWithBlock::ConstBlock(a) => p.push_rust(a),
            ExprWithBlock::Loop(a) => p.push_rust(a.as_ref()),
            ExprWithBlock::If(a) => p.push_rust(a),
            ExprWithBlock::Match(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprWithBlock::BlockExpr(a) => p.push_u(a),
            ExprWithBlock::AsyncBlock(a) => p.push_u(a),
            ExprWithBlock::UnsafeBlock(a) => p.push_u(a),
            ExprWithBlock::ConstBlock(a) => p.push_u(a),
            ExprWithBlock::Loop(a) => p.push_u(a.as_ref()),
            ExprWithBlock::If(a) => p.push_u(a),
            ExprWithBlock::Match(a) => p.push_u(a),
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprAsyncBlock {
    pub move_: bool,
    pub block: BlockExpr,

    pub span_async: Span,
    pub span_move: Option<Span>,
}
impl ToLang for ExprAsyncBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("async", s.span_async);
        if s.move_ {
            p.push_raw(" ");
            p.push_str("move", s.span_move.unwrap());
        }
        p.push_raw(" ");
        p.push_rust(&s.block);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("async");
        if s.move_ {
            p.push_raw(" move");
        }
        p.push_u(&s.block);
    }
}
#[derive(Debug)]
pub struct ExprUnsafeBlock {
    pub block: BlockExpr,

    pub span_unsafe: Span,
}
impl ToLang for ExprUnsafeBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("unsafe", s.span_unsafe);
        p.push_raw(" ");
        p.push_rust(&s.block);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("unsafe");
        p.push_u(&s.block);
    }
}
#[derive(Debug)]
pub struct ExprConstBlock {
    pub block: BlockExpr,

    pub span_const: Span,
}
impl ToLang for ExprConstBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("const", s.span_const);
        p.push_raw(" ");
        p.push_rust(&s.block);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("const");
        p.push_u(&s.block);
    }
}
#[derive(Debug, Default)]
pub struct ExprLoop {
    pub label: Option<Identifier>,
    pub body: BlockExpr,
    pub payload: ExprLoopPayload,

    pub span_for: Span,
    pub span_label: Option<Span>,
}
impl ToLang for ExprLoop {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(label) = &s.label {
            p.push_str("'", s.span_label.unwrap());
            p.push_rust(label);
            p.push_raw(": ");
        }

        match &s.payload {
            ExprLoopPayload::Loop => p.push_str("loop", s.span_for),
            ExprLoopPayload::While(a) => {
                p.push_str("while", s.span_for);
                p.push_raw(" ");
                if !a.match_arm_patterns.is_empty() {
                    p.push_raw("let ");
                    for (i, item) in a.match_arm_patterns.iter().enumerate() {
                        if i != 0 {
                            p.push_raw("| ");
                        }
                        p.push_rust(item);
                    }
                    p.push_raw(" = ");
                }
                p.push_rust(&a.expr);
            }
            ExprLoopPayload::In(a) => {
                p.push_str("for", s.span_for);
                p.push_raw(" ");
                p.push_rust(&a.pattern);
                p.push_raw(" ");
                p.push_str("in", a.span_in);
                p.push_raw(" ");
                p.push_rust(&a.in_expr);
            }
        }

        p.push_raw(" ");
        p.push_rust(&s.body);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(label) = &s.label {
            p.push_raw("'");
            p.push_u(label);
            p.push_raw(": ");
        }

        match &s.payload {
            ExprLoopPayload::Loop => p.push_raw("loop"),
            ExprLoopPayload::While(a) => {
                p.push_raw("while ");
                if !a.match_arm_patterns.is_empty() {
                    p.push_raw("let ");
                    for (i, item) in a.match_arm_patterns.iter().enumerate() {
                        if i != 0 {
                            p.push_raw("| ");
                        }
                        p.push_u(item);
                    }
                    p.push_raw(" = ");
                }
                p.push_u(&a.expr);
            }
            ExprLoopPayload::In(a) => {
                p.push_raw("for ");
                p.push_u(&a.pattern);
                p.push_raw(" in ");
                p.push_u(&a.in_expr);
            }
        }

        p.push_u(&s.body);
    }
}
#[derive(Debug, Default)]
pub enum ExprLoopPayload {
    #[default]
    Loop,
    While(ExprLoopWhile),
    In(ExprLoopIn),
}
// while and while let
#[derive(Debug)]
pub struct ExprLoopWhile {
    pub expr: Expr, // except struct expression
    pub match_arm_patterns: Vec<Pattern>,
}
// in
#[derive(Debug)]
pub struct ExprLoopIn {
    pub pattern: Pattern,
    pub in_expr: Expr, // except struct expression

    pub span_in: Span,
}

#[derive(Debug, Default)]
pub struct ExprIf {
    pub predicate: Expr, // except struct expression
    pub match_arm_patterns: Vec<Pattern>,
    pub body: BlockExpr,
    pub else_: Option<ExprIfElse>,

    pub span_if: Span,
    pub span_else: Option<Span>,
}
impl ToLang for ExprIf {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("if", s.span_if);
        p.push_raw(" ");
        if !s.match_arm_patterns.is_empty() {
            p.push_raw("let ");
            for (i, item) in s.match_arm_patterns.iter().enumerate() {
                if i != 0 {
                    p.push_raw("| ");
                }
                p.push_rust(item);
            }
            p.push_raw(" = ");
        }
        p.push_rust(&s.predicate);
        p.push_raw(" ");
        p.push_rust(&s.body);

        if let Some(else_) = &s.else_ {
            p.push_raw(" ");
            p.push_str("else", s.span_else.unwrap());
            p.push_raw(" ");
            match else_ {
                ExprIfElse::BlockExpr(a) => p.push_rust(a),
                ExprIfElse::If(a) => p.push_rust(a.as_ref()),
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("if ");
        if !s.match_arm_patterns.is_empty() {
            p.push_raw("let ");
            for (i, item) in s.match_arm_patterns.iter().enumerate() {
                if i != 0 {
                    p.push_raw("| ");
                }
                p.push_u(item);
            }
            p.push_raw(" = ");
        }
        p.push_u(&s.predicate);
        p.push_u(&s.body);

        if let Some(else_) = &s.else_ {
            p.push_raw(" else ");
            match else_ {
                ExprIfElse::BlockExpr(a) => p.push_u(a),
                ExprIfElse::If(a) => p.push_u(a.as_ref()),
            }
        }
    }
}
#[derive(Debug)]
pub enum ExprIfElse {
    BlockExpr(BlockExpr),
    If(Box<ExprIf>),
}
impl Default for ExprIfElse {
    fn default() -> Self {
        ExprIfElse::BlockExpr(BlockExpr::default())
    }
}

#[derive(Debug, Default)]
pub struct ExprMatch {
    pub expr: Expr, // except struct expression
    pub inner_attrs: Attrs,
    pub arms: Vec<ExprMatchItem>,

    pub span_match: Span,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for ExprMatch {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("match", s.span_match);
        p.push_raw(" ");
        p.push_rust(&s.expr);

        p.push_raw(" ");
        p.push_str("{", s.span_brace_open);
        p.push_raw("\n");
        p.inc_indent();

        p.push_rust(&s.inner_attrs);
        for item in &s.arms {
            p.indent();
            p.push_rust(&item.arm);
            p.push_raw(" => ");
            p.push_rust(&item.expr);
            p.push_raw(",\n");
        }

        p.dec_indent();
        p.indent();
        p.push_str("}", s.span_brace_close);
        p.push_raw("\n");
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("match ");
        p.push_u(&s.expr);

        p.push_raw(" {\n");
        p.inc_indent();

        p.push_u(&s.inner_attrs);
        for item in &s.arms {
            p.indent();
            p.push_u(&item.arm);
            p.push_raw(" => ");
            p.push_u(&item.expr);
            p.push_raw(",\n");
        }

        p.dec_indent();
        p.indent();
        p.push_raw("}\n");
    }
}
#[derive(Debug, Default)]
pub struct ExprMatchItem {
    pub arm: ExprMatchArm,
    pub expr: Expr,
}
#[derive(Debug, Default)]
pub struct ExprMatchArm {
    pub outer_attrs: Attrs,
    pub patterns: Vec<Pattern>,
    pub guard: Option<ExprMatchArmGuard>,
}
impl ToLang for ExprMatchArm {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        for (i, item) in s.patterns.iter().enumerate() {
            if i != 0 {
                p.push_raw(" | ");
            }
            p.push_rust(item);
        }
        if let Some(guard) = &s.guard {
            p.push_raw(" ");
            p.push_str("if", guard.span_if);
            p.push_raw(" ");
            p.push_rust(&guard.expr);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.outer_attrs);
        for (i, item) in s.patterns.iter().enumerate() {
            if i != 0 {
                p.push_raw(" | ");
            }
            p.push_u(item);
        }
        if let Some(guard) = &s.guard {
            p.push_raw(" if ");
            p.push_u(&guard.expr);
        }
    }
}
#[derive(Debug)]
pub struct ExprMatchArmGuard {
    pub expr: Expr,

    pub span_if: Span,
}

#[derive(Debug)]
pub enum ExprWithoutBlock {
    Literal(ExprLiteral),
    Path(ExprPath),
    Operator(ExprOperator),
    Grouped(ExprGrouped),
    Array(ExprArray),
    Await(ExprAwait),
    Index(ExprIndex),
    Tuple(ExprTuple),
    TupleIndexing(ExprTupleIndexing),
    Struct(ExprStruct),
    Call(ExprCall),
    MethodCall(ExprMethodCall),
    Field(ExprField),
    Closure(ExprClosure),
    Continue(ExprContinue),
    Break(ExprBreak),
    Range(ExprRange),
    Return(ExprReturn),
    MacroCall(MacroCall),
}
impl Default for ExprWithoutBlock {
    fn default() -> Self {
        ExprWithoutBlock::Literal(ExprLiteral(Literal::default()))
    }
}
impl ToLang for ExprWithoutBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprWithoutBlock::Literal(a) => p.push_rust(a),
            ExprWithoutBlock::Path(a) => p.push_rust(a),
            ExprWithoutBlock::Operator(a) => p.push_rust(a),
            ExprWithoutBlock::Grouped(a) => p.push_rust(a),
            ExprWithoutBlock::Array(a) => p.push_rust(a),
            ExprWithoutBlock::Await(a) => p.push_rust(a),
            ExprWithoutBlock::Index(a) => p.push_rust(a),
            ExprWithoutBlock::Tuple(a) => p.push_rust(a),
            ExprWithoutBlock::TupleIndexing(a) => p.push_rust(a),
            ExprWithoutBlock::Struct(a) => p.push_rust(a),
            ExprWithoutBlock::Call(a) => p.push_rust(a),
            ExprWithoutBlock::MethodCall(a) => p.push_rust(a),
            ExprWithoutBlock::Field(a) => p.push_rust(a),
            ExprWithoutBlock::Closure(a) => p.push_rust(a),
            ExprWithoutBlock::Continue(a) => p.push_rust(a),
            ExprWithoutBlock::Break(a) => p.push_rust(a),
            ExprWithoutBlock::Range(a) => p.push_rust(a),
            ExprWithoutBlock::Return(a) => p.push_rust(a),
            ExprWithoutBlock::MacroCall(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprWithoutBlock::Literal(a) => p.push_u(a),
            ExprWithoutBlock::Path(a) => p.push_u(a),
            ExprWithoutBlock::Operator(a) => p.push_u(a),
            ExprWithoutBlock::Grouped(a) => p.push_u(a),
            ExprWithoutBlock::Array(a) => p.push_u(a),
            ExprWithoutBlock::Await(a) => p.push_u(a),
            ExprWithoutBlock::Index(a) => p.push_u(a),
            ExprWithoutBlock::Tuple(a) => p.push_u(a),
            ExprWithoutBlock::TupleIndexing(a) => p.push_u(a),
            ExprWithoutBlock::Struct(a) => p.push_u(a),
            ExprWithoutBlock::Call(a) => p.push_u(a),
            ExprWithoutBlock::MethodCall(a) => p.push_u(a),
            ExprWithoutBlock::Field(a) => p.push_u(a),
            ExprWithoutBlock::Closure(a) => p.push_u(a),
            ExprWithoutBlock::Continue(a) => p.push_u(a),
            ExprWithoutBlock::Break(a) => p.push_u(a),
            ExprWithoutBlock::Range(a) => p.push_u(a),
            ExprWithoutBlock::Return(a) => p.push_u(a),
            ExprWithoutBlock::MacroCall(a) => p.push_u(a),
        }
    }
}

#[derive(Debug)]
pub struct ExprLiteral(pub Literal);
impl ToLang for ExprLiteral {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.0);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.0);
    }
}

#[derive(Debug)]
pub enum ExprPath {
    Path(PathInExpr),
    QualifiedPath(QualifiedPathInExpr),
}
impl ToLang for ExprPath {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprPath::Path(a) => p.push_rust(a),
            ExprPath::QualifiedPath(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprPath::Path(a) => p.push_u(a),
            ExprPath::QualifiedPath(a) => p.push_u(a),
        }
    }
}

#[derive(Debug)]
pub enum ExprOperator {
    Borrow(ExprBorrow),
    Dereference(ExprDereference),
    ErrorPropagation(ExprErrorPropagation),
    Negation(ExprNegation),
    ArithmeticOrLogical(ExprArithmeticOrLogical),
    Comparison(ExprComparison),
    LazyBoolean(ExprLazyBoolean),
    TypeCast(Box<ExprTypeCast>),
    Assignment(ExprAssignment),
    CompoundAssignment(ExprCompoundAssignment),
}
impl ToLang for ExprOperator {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprOperator::Borrow(a) => {
                p.push_str("&", a.span_ref);
                if a.raw {
                    p.push_str("raw", a.span_raw.unwrap());
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_str("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                } else if a.raw {
                    p.push_raw("const ");
                }
                p.push_rust(&a.expr);
            }
            ExprOperator::Dereference(a) => {
                p.push_str("*", a.span);
                p.push_rust(&a.expr);
            }
            ExprOperator::ErrorPropagation(a) => {
                p.push_rust(&a.expr);
                p.push_str("?", a.span);
            }
            ExprOperator::Negation(a) => {
                if a.not {
                    p.push_str("!", a.span);
                } else {
                    p.push_str("-", a.span);
                }
                p.push_rust(&a.expr);
            }
            ExprOperator::ArithmeticOrLogical(a) => {
                p.push_rust(&a.left);
                p.push_raw(" ");
                if let TokenCode::Op(op) = &a.op {
                    match op {
                        Op::Add => p.push_str("+", a.span_op),
                        Op::Sub => p.push_str("-", a.span_op),
                        Op::Star => p.push_str("*", a.span_op),
                        Op::Div => p.push_str("/", a.span_op),
                        Op::Rem => p.push_str("%", a.span_op),
                        Op::BitAnd => p.push_str("&", a.span_op),
                        Op::BitOr => p.push_str("|", a.span_op),
                        Op::BitXor => p.push_str("^", a.span_op),
                        Op::LeftShift => p.push_str("<<", a.span_op),
                        Op::RightShift => p.push_str(">>", a.span_op),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_rust(&a.right);
            }
            ExprOperator::Comparison(a) => {
                p.push_rust(&a.left);
                p.push_raw(" ");
                if let TokenCode::Op(op) = &a.op {
                    match op {
                        Op::Eq => p.push_str("==", a.span_op),
                        Op::Neq => p.push_str("!=", a.span_op),
                        Op::Greater => p.push_str(">", a.span_op),
                        Op::Less => p.push_str("<", a.span_op),
                        Op::GreaterEq => p.push_str(">=", a.span_op),
                        Op::LessEq => p.push_str("<=", a.span_op),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_rust(&a.right);
            }
            ExprOperator::LazyBoolean(a) => {
                p.push_rust(&a.left);
                p.push_raw(" ");
                if let TokenCode::Op(op) = &a.op {
                    match op {
                        Op::And => p.push_str("&&", a.span_op),
                        Op::Or => p.push_str("||", a.span_op),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_rust(&a.right);
            }
            ExprOperator::TypeCast(a) => {
                p.push_rust(&a.expr);
                p.push_raw(" as ");
                if let TypeNoBounds::Parenthesized(a) = &a.type_no_bounds {
                    p.push_rust(&a.type_);
                } else {
                    p.push_rust(&a.type_no_bounds);
                }
            }
            ExprOperator::Assignment(a) => {
                p.push_rust(&a.left);
                p.push_raw(" ");
                p.push_str("=", a.span_op);
                p.push_raw(" ");
                p.push_rust(&a.right);
            }
            ExprOperator::CompoundAssignment(a) => {
                p.push_rust(&a.left);
                p.push_raw(" ");
                if let TokenCode::Assign(assign) = &a.op {
                    match assign {
                        Assign::Add => p.push_str("+=", a.span_op),
                        Assign::Sub => p.push_str("-=", a.span_op),
                        Assign::Star => p.push_str("*=", a.span_op),
                        Assign::Div => p.push_str("/=", a.span_op),
                        Assign::Rem => p.push_str("%=", a.span_op),
                        Assign::BitAnd => p.push_str("&=", a.span_op),
                        Assign::BitOr => p.push_str("|=", a.span_op),
                        Assign::BitXor => p.push_str("^=", a.span_op),
                        Assign::LeftShift => p.push_str("<<=", a.span_op),
                        Assign::RightShift => p.push_str(">>=", a.span_op),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_rust(&a.right);
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprOperator::Borrow(a) => {
                p.push_raw("&");
                if a.raw {
                    p.push_raw("raw ");
                }
                if a.mut_ {
                    p.push_raw("mut ");
                } else if a.raw {
                    p.push_raw("const ");
                }
                p.push_u(&a.expr);
            }
            ExprOperator::Dereference(a) => {
                p.push_raw("*");
                p.push_u(&a.expr);
            }
            ExprOperator::ErrorPropagation(a) => {
                p.push_u(&a.expr);
                p.push_raw("?");
            }
            ExprOperator::Negation(a) => {
                if a.not {
                    p.push_raw("!");
                } else {
                    p.push_raw("-");
                }
                p.push_u(&a.expr);
            }
            ExprOperator::ArithmeticOrLogical(a) => {
                p.push_u(&a.left);
                p.push_raw(" ");
                if let TokenCode::Op(op) = &a.op {
                    match op {
                        Op::Add => p.push_raw("+"),
                        Op::Sub => p.push_raw("-"),
                        Op::Star => p.push_raw("*"),
                        Op::Div => p.push_raw("/"),
                        Op::Rem => p.push_raw("%"),
                        Op::BitAnd => p.push_raw("&"),
                        Op::BitOr => p.push_raw("|"),
                        Op::BitXor => p.push_raw("^"),
                        Op::LeftShift => p.push_raw("<<"),
                        Op::RightShift => p.push_raw(">>"),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_u(&a.right);
            }
            ExprOperator::Comparison(a) => {
                p.push_u(&a.left);
                p.push_raw(" ");
                if let TokenCode::Op(op) = &a.op {
                    match op {
                        Op::Eq => p.push_raw("=="),
                        Op::Neq => p.push_raw("!="),
                        Op::Greater => p.push_raw(">"),
                        Op::Less => p.push_raw("<"),
                        Op::GreaterEq => p.push_raw(">="),
                        Op::LessEq => p.push_raw("<="),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_u(&a.right);
            }
            ExprOperator::LazyBoolean(a) => {
                p.push_u(&a.left);
                p.push_raw(" ");
                if let TokenCode::Op(op) = &a.op {
                    match op {
                        Op::And => p.push_raw("&&"),
                        Op::Or => p.push_raw("||"),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_u(&a.right);
            }
            ExprOperator::TypeCast(a) => {
                p.push_u(&a.expr);
                p.push_raw(" as ");
                if let TypeNoBounds::Parenthesized(a) = &a.type_no_bounds {
                    p.push_u(&a.type_);
                } else {
                    p.push_u(&a.type_no_bounds);
                }
            }
            ExprOperator::Assignment(a) => {
                p.push_u(&a.left);
                p.push_raw(" = ");
                p.push_u(&a.right);
            }
            ExprOperator::CompoundAssignment(a) => {
                p.push_u(&a.left);
                p.push_raw(" ");
                if let TokenCode::Assign(assign) = &a.op {
                    match assign {
                        Assign::Add => p.push_raw("+="),
                        Assign::Sub => p.push_raw("-="),
                        Assign::Star => p.push_raw("*="),
                        Assign::Div => p.push_raw("/="),
                        Assign::Rem => p.push_raw("%="),
                        Assign::BitAnd => p.push_raw("&="),
                        Assign::BitOr => p.push_raw("|="),
                        Assign::BitXor => p.push_raw("^="),
                        Assign::LeftShift => p.push_raw("<<="),
                        Assign::RightShift => p.push_raw(">>="),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_u(&a.right);
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct ExprBorrow {
    pub raw: bool,
    pub mut_: bool,
    pub expr: Expr,

    pub span_ref: Span,
    pub span_raw: Option<Span>,
    pub span_mut: Option<Span>,
}
#[derive(Debug)]
pub struct ExprDereference {
    pub expr: Expr,

    pub span: Span,
}
#[derive(Debug)]
pub struct ExprErrorPropagation {
    pub expr: Expr,

    pub span: Span,
}
#[derive(Debug)]
pub struct ExprNegation {
    pub not: bool, // true for -, false for !(bitwise not)
    pub expr: Expr,

    pub span: Span,
}
#[derive(Debug)]
pub struct ExprArithmeticOrLogical {
    pub left: Expr,
    pub op: TokenCode, // must be TokenCode::Op(+ ~ * / % & | ^ << >>)
    pub right: Expr,

    pub span_op: Span,
}
#[derive(Debug)]
pub struct ExprComparison {
    pub left: Expr,
    pub op: TokenCode, // must be TokenCode::Op(== != > < >= <=)
    pub right: Expr,

    pub span_op: Span,
}
#[derive(Debug)]
pub struct ExprLazyBoolean {
    pub left: Expr,
    pub op: TokenCode, // must be TokenCode::Op(&& ||)
    pub right: Expr,

    pub span_op: Span,
}
#[derive(Debug)]
pub struct ExprTypeCast {
    pub expr: Expr,
    pub type_no_bounds: TypeNoBounds,

    pub span_as: Span,
}
#[derive(Debug)]
pub struct ExprAssignment {
    pub left: Expr,
    pub right: Expr,

    pub span_op: Span,
}
#[derive(Debug)]
pub struct ExprCompoundAssignment {
    pub op: TokenCode, // must be TokenCode::Op(+= ~= *= /= %= &= |= ^= <<= >>=)
    pub left: Expr,
    pub right: Expr,

    pub span_op: Span,
}

#[derive(Debug)]
pub struct ExprGrouped {
    pub inner_attrs: Attrs,
    pub expr: Expr,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for ExprGrouped {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("(", s.span_paren_open);
        p.push_rust(&s.inner_attrs);
        p.push_rust(&s.expr);
        p.push_str(")", s.span_paren_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("(");
        p.push_u(&s.inner_attrs);
        p.push_u(&s.expr);
        p.push_raw(")");
    }
}
#[derive(Debug)]
pub struct ExprArray {
    pub inner_attrs: Attrs,
    pub payload: ExprArrayPayload,

    pub span_double_brace_open: Span,
    pub span_double_brace_close: Span,
}
impl ToLang for ExprArray {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("[", s.span_double_brace_open);
        p.push_rust(&s.inner_attrs);
        match &s.payload {
            ExprArrayPayload::One(a) => {
                p.push_rust(&a.value);
                p.push_str(";", a.span_semicolon);
                p.push_raw(" ");
                p.push_rust(&a.len);
            }
            ExprArrayPayload::Many(a) => {
                for (i, item) in a.0.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
            }
        }
        p.push_str("]", s.span_double_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("[");
        p.push_u(&s.inner_attrs);
        match &s.payload {
            ExprArrayPayload::One(a) => {
                p.push_u(&a.value);
                p.push_raw("; ");
                p.push_u(&a.len);
            }
            ExprArrayPayload::Many(a) => {
                for (i, item) in a.0.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_u(item);
                }
            }
        }
        p.push_raw("]");
    }
}
#[derive(Debug)]
pub enum ExprArrayPayload {
    One(ExprArrayOne),
    Many(ExprArrayMany),
}
#[derive(Debug)]
pub struct ExprArrayOne {
    pub value: Expr,
    pub len: Expr,

    pub span_semicolon: Span,
}
#[derive(Debug)]
pub struct ExprArrayMany(pub Vec<Expr>);

#[derive(Debug)]
pub struct ExprAwait {
    pub expr: Expr,

    pub span: Span,
}
impl ToLang for ExprAwait {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.expr);
        p.push_str(".await", s.span);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.expr);
        p.push_raw(".await");
    }
}

#[derive(Debug)]
pub struct ExprIndex {
    pub left: Expr,
    pub right: Expr,

    pub span_double_brace_open: Span,
    pub span_double_brace_close: Span,
}
impl ToLang for ExprIndex {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.left);
        p.push_str("[", s.span_double_brace_open);
        p.push_rust(&s.right);
        p.push_str("]", s.span_double_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.left);
        p.push_raw("[");
        p.push_u(&s.right);
        p.push_raw("]");
    }
}
#[derive(Debug)]
pub struct ExprTuple {
    pub inner_attrs: Attrs,
    pub items: Vec<Expr>,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for ExprTuple {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("(", s.span_paren_open);
        p.push_rust(&s.inner_attrs);
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        if s.items.len() == 1 {
            p.push_raw(", ");
        }
        p.push_str(")", s.span_paren_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("(");
        p.push_u(&s.inner_attrs);
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        if s.items.len() == 1 {
            p.push_raw(", ");
        }
        p.push_raw(")");
    }
}
#[derive(Debug)]
pub struct ExprTupleIndexing {
    pub left: Expr,
    pub index: String,

    pub span_dot: Span,
    pub span_index: Span,
}
impl ToLang for ExprTupleIndexing {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.left);
        p.push_str(".", s.span_dot);
        p.push_str(&s.index, s.span_index);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.left);
        p.push_raw(".");
        p.push_raw(&s.index);
    }
}
#[derive(Debug)]
pub enum ExprStruct {
    Struct(ExprStructStruct),
    Tuple(ExprStructTuple),
}
impl ToLang for ExprStruct {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprStruct::Struct(a) => p.push_rust(a),
            ExprStruct::Tuple(a) => p.push_rust(a),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprStruct::Struct(a) => p.push_u(a),
            ExprStruct::Tuple(a) => p.push_u(a),
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprStructStruct {
    pub path: PathInExpr,
    pub inner_attrs: Attrs,
    pub fields: Vec<ExprStructField>,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
    pub span_base: Option<Span>,
}
impl ToLang for ExprStructStruct {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.path);
        p.push_str("{", s.span_brace_open);
        p.push_rust(&s.inner_attrs);
        for (i, item) in s.fields.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_str("}", s.span_brace_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.path);
        p.push_raw("{");
        p.push_u(&s.inner_attrs);
        for (i, item) in s.fields.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        p.push_raw("}");
    }
}
#[derive(Debug)]
pub enum ExprStructField {
    A(ExprStructFieldA),
    B(ExprStructFieldB),
    C(ExprStructFieldC),
    Base(Option<Expr>, Span),
}
impl ToLang for ExprStructField {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprStructField::A(a) => p.push_rust(&a.name),
            ExprStructField::B(a) => {
                p.push_rust(&a.name);
                p.push_raw(": ");
                p.push_rust(&a.expr);
            }
            ExprStructField::C(a) => {
                p.push_str(&a.tuple_index, a.span_index);
                p.push_raw(": ");
                p.push_rust(&a.expr);
            }
            ExprStructField::Base(a, span) => {
                p.push_str("..", *span);
                if let Some(a) = a {
                    p.push_rust(a);
                } else {
                    p.push_raw("Default::default()")
                }
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprStructField::A(a) => p.push_u(&a.name),
            ExprStructField::B(a) => {
                p.push_u(&a.name);
                p.push_raw(": ");
                p.push_u(&a.expr);
            }
            ExprStructField::C(a) => {
                p.push_raw(&a.tuple_index);
                p.push_raw(": ");
                p.push_u(&a.expr);
            }
            ExprStructField::Base(a, _) => {
                p.push_raw("..");
                if let Some(a) = a {
                    p.push_u(a);
                }
            }
        }
    }
}
impl Default for ExprStructField {
    fn default() -> Self {
        ExprStructField::A(ExprStructFieldA::default())
    }
}
#[derive(Debug, Default)]
pub struct ExprStructFieldA {
    pub name: Identifier,
}
#[derive(Debug, Default)]
pub struct ExprStructFieldB {
    pub name: Identifier,
    pub expr: Expr,
}
#[derive(Debug, Default)]
pub struct ExprStructFieldC {
    pub tuple_index: String,
    pub expr: Expr,

    pub span_index: Span,
}
#[derive(Debug, Default)]
pub struct ExprStructTuple {
    pub path: PathInExpr,
    pub inner_attrs: Attrs,
    pub exprs: Vec<Expr>,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for ExprStructTuple {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.path);
        p.push_str("(", s.span_paren_open);
        p.push_rust(&s.inner_attrs);
        for (i, item) in s.exprs.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_str(")", s.span_paren_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.path);
        p.push_raw("(");
        p.push_u(&s.inner_attrs);
        for (i, item) in s.exprs.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        p.push_raw(")");
    }
}

#[derive(Debug)]
pub struct ExprCall {
    pub left: Expr,
    pub params: Vec<Expr>,

    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for ExprCall {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.left);
        p.push_str("(", s.span_paren_open);
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_str(")", s.span_paren_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.left);
        p.push_raw("(");
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        p.push_raw(")");
    }
}
#[derive(Debug, Default)]
pub struct ExprMethodCall {
    pub left: Expr,
    pub path_expr_segment: PathExprSegment,
    pub params: Vec<Expr>,

    pub span_dot: Span,
    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for ExprMethodCall {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.left);
        p.push_str(".", s.span_dot);
        p.push_rust(&s.path_expr_segment);
        p.push_str("(", s.span_paren_open);
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_str(")", s.span_paren_close);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.left);
        p.push_raw(".");
        p.push_u(&s.path_expr_segment);
        p.push_raw("(");
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_u(item);
        }
        p.push_raw(")");
    }
}
#[derive(Debug)]
pub struct ExprField {
    pub left: Expr,
    pub name: Identifier,

    pub span_dot: Span,
}
impl ToLang for ExprField {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.left);
        p.push_str(".", s.span_dot);
        p.push_rust(&s.name);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_u(&s.left);
        p.push_raw(".");
        p.push_u(&s.name);
    }
}
#[derive(Debug, Default)]
pub struct FuncParamOptionalType {
    pub outer_attrs: Attrs,
    // pub pattern: Pattern, // I don't think using patterns for function params is neccessary
    pub name: Identifier,
    pub type_: Option<Type>,
}
#[derive(Debug, Default)]
pub struct ExprClosure {
    pub async_: bool,
    pub move_: bool,
    pub params: Vec<FuncParamOptionalType>,
    pub return_type: Option<TypeNoBounds>,
    pub expr: Expr,

    pub span_async: Option<Span>,
    pub span_move: Option<Span>,
    pub span_paren_open: Span,
    pub span_paren_close: Span,
}
impl ToLang for ExprClosure {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.async_ {
            p.push_str("async", s.span_async.unwrap());
            p.push_raw(" ");
        }
        if s.move_ {
            p.push_str("move", s.span_move.unwrap());
            p.push_raw(" ");
        }

        p.push_str("|", s.span_paren_open);
        for (i, item) in s.params.iter().enumerate() {
            p.push_rust(&item.outer_attrs);
            if &item.name.id != "-" {
                p.push_raw("mut ");
            }
            p.push_rust(&item.name);
            if let Some(type_) = &item.type_ {
                p.push_raw(": ");
                p.push_rust(type_);
            }
            if i != s.params.len() - 1 {
                p.push_raw(", ");
            }
        }
        p.push_str("|", s.span_paren_close);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_rust(return_type);
        }
        p.push_raw(" ");
        p.push_rust(&s.expr);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if s.async_ {
            p.push_raw("async ");
        }
        if s.move_ {
            p.push_raw("move ");
        }

        p.push_raw("|");
        for (i, item) in s.params.iter().enumerate() {
            p.push_u(&item.outer_attrs);
            if &item.name.id != "-" {
                p.push_raw("mut ");
            }
            p.push_u(&item.name);
            if let Some(type_) = &item.type_ {
                p.push_raw(": ");
                p.push_u(type_);
            }
            if i != s.params.len() - 1 {
                p.push_raw(", ");
            }
        }
        p.push_raw("|");

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_u(return_type);
        }
        p.push_u(&s.expr);
    }
}
#[derive(Debug, Default)]
pub struct ExprContinue {
    pub label: Option<Identifier>,

    pub span_continue: Span,
    pub span_label: Option<Span>,
}
impl ToLang for ExprContinue {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("continue", s.span_continue);
        if let Some(identifier) = &s.label {
            p.push_raw(" ");
            p.push_str("'", s.span_label.unwrap());
            p.push_rust(identifier);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("continue");
        if let Some(identifier) = &s.label {
            p.push_raw(" '");
            p.push_u(identifier);
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprBreak {
    pub label: Option<Identifier>,
    pub expr: Option<Expr>,

    pub span_break: Span,
    pub span_label: Option<Span>,
}
impl ToLang for ExprBreak {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("break", s.span_break);
        if let Some(identifier) = &s.label {
            p.push_raw(" ");
            p.push_str("'", s.span_label.unwrap());
            p.push_rust(identifier);
        }
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_rust(expr);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("break");
        if let Some(identifier) = &s.label {
            p.push_raw(" '");
            p.push_u(identifier);
        }
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_u(expr);
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprReturn {
    pub expr: Option<Expr>,

    pub span_return: Span,
}
impl ToLang for ExprReturn {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_str("return", s.span_return);
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_rust(expr);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("return");
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_u(expr);
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprRange {
    pub start: Option<Expr>,
    pub inclusive: bool, // can't be false when start == None && end == None
    pub end: Option<Expr>,

    pub span: Span,
}
impl ToLang for ExprRange {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(start) = &s.start {
            p.push_rust(start);
        }
        if s.inclusive {
            p.push_str("..=", s.span);
        } else {
            p.push_str("..", s.span);
        }
        if let Some(end) = &s.end {
            p.push_rust(end);
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(start) = &s.start {
            p.push_u(start);
        }
        if s.inclusive {
            p.push_raw("..=");
        } else {
            p.push_raw("..");
        }
        if let Some(end) = &s.end {
            p.push_u(end);
        }
    }
}
