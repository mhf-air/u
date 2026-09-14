use super::lex::*;
use crate::T;
use std::convert::TryFrom;
use std::mem;

pub struct LangFormatter<'a> {
    buf: String,
    indent: u8,

    tokens: &'a [Token],
    end_tokens: &'a [Token],

    span_pairs: Vec<SpanPair>,
    line: usize,
    column: usize,

    in_impl_static: bool,
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
    // width in bytes
    pub width: usize,
    pub line: usize,
    pub column: usize,

    #[cfg(debug_assertions)]
    pub text: String,
}

#[derive(Debug)]
pub struct ToRust {
    pub buf: String,
    pub span_pairs: Vec<SpanPair>,
}

impl<'a> LangFormatter<'a> {
    pub fn new(tokens: &'a [Token], end_tokens: &'a [Token]) -> LangFormatter<'a> {
        LangFormatter {
            buf: String::new(),
            indent: 0,

            tokens,
            end_tokens,

            span_pairs: Vec::new(),
            line: 1,
            column: 1,

            in_impl_static: false,
        }
    }

    pub fn buf(self) -> String {
        let s = self;
        s.buf
    }
    pub fn buf_ref(&self) -> &String {
        let s = self;
        &s.buf
    }
    pub fn span_pairs(&self) -> &Vec<SpanPair> {
        let s = self;
        &s.span_pairs
    }
    pub fn span_pairs_move(&mut self) -> Vec<SpanPair> {
        let s = self;
        mem::take(&mut s.span_pairs)
    }

    pub fn to_rust(self) -> ToRust {
        let s = self;
        ToRust {
            buf: s.buf,
            span_pairs: s.span_pairs,
        }
    }

    pub fn push_rust(&mut self, a: &impl ToLang) {
        let s = self;
        a.to_rust(s);
    }

    // push all tokens including whitespaces and comments as they are in .u file
    pub fn push_u_pair(&mut self, pair: &UPair) {
        let s = self;
        let list = &s.tokens[pair.start..pair.end];
        for it in list {
            for a in &it.tokens_before {
                s.buf.push_str(&a.u);
            }
            s.buf.push_str(&it.u);
        }
    }
    // condense all whitespaces and comments into one space
    pub fn push_u_pair_space(&mut self, pair: &UPair) {
        let s = self;
        let list = &s.tokens[pair.start..pair.end];
        for it in list {
            if !it.tokens_before.is_empty() {
                s.buf.push_str(" ");
            }
            s.buf.push_str(&it.u);
        }
    }

    pub fn push_token(&mut self, rs: &str, delta_line: usize, span: Span) {
        let s = self;

        let rust_span = RustSpan {
            byte_start: s.buf.len(),
            width: rs.len(),
            line: s.line,
            column: s.column,
            #[cfg(debug_assertions)]
            text: rs.to_string(),
        };
        s.span_pairs.push(SpanPair{
            u: span,
            rust: rust_span,
        });

        s.line += delta_line;
        if delta_line > 0 {
            s.column = span.end_column;
        } else {
            s.column += rs.len();
        }
        s.buf.push_str(rs);
    }

    pub fn push_s(&mut self, a: &str, span: Span) {
        let s = self;

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
        let spaces = s.indent * 4;
        for _ in 0..spaces {
            // s.buf.push('\t');
            s.buf.push(' ');
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
}

#[derive(Debug, Default)]
pub struct UPair {
    pub start: usize,
    pub end: usize,
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Package {
    pub shebang: Option<Token>,
    pub inner_attrs: Attrs,
    pub items: Vec<Item>,
    pub end_tokens: Vec<Token>,
    pub u_pair: UPair,
}
impl ToLang for Package {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(a) = &s.shebang {
            a.to_rust(p);
        }

        s.inner_attrs.to_rust(p);
        for it in &s.items {
            it.to_rust(p);
        }

        for it in &s.end_tokens {
            it.to_rust(p);
        }
    }
}
impl Package {
    pub fn to_u(&self, p: &mut LangFormatter) {
        p.push_u_pair(&self.u_pair);
        // for file, need to print the ending tokens
        for it in p.end_tokens {
            p.push_raw(&it.u);
        }
    }
}

/// NOTE the form is fixed: pub(crate)
/// no whitespaces or comments within Visibility
#[derive(Debug, Default)]
pub struct Visibility {
    pub payload: VisibilityPayload,
}
#[derive(Debug)]
pub enum VisibilityPayload {
    Private(bool),
    Pub,
    Crate,
    Super,
    SimplePath(SimplePath),
}
impl Default for VisibilityPayload {
    fn default() -> Self {
        Self::Private(false)
    }
}
impl ToLang for Visibility {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;
        use VisibilityPayload as A;

        match &s.payload {
            A::Private(is_self) => {
                if *is_self {
                    p.push_raw("pub(self) ");
                }
            }
            A::Pub => {
                p.push_raw("pub ");
            }
            A::Crate => {
                p.push_raw("pub(crate) ");
            }
            A::Super => {
                p.push_raw("pub(super) ");
            }
            A::SimplePath(path) => {
                p.push_raw("pub(in");
                path.to_rust(p);
                p.push_raw(") ");
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct Item {
    pub outer_attrs: Attrs,
    pub vis: Visibility,
    pub payload: ItemPayload,
}
impl ToLang for Item {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        // add #[derive(Debug)] for all struct and enum
        if matches!(&s.payload, ItemPayload::Struct(_) | ItemPayload::Enum(_)) {
            let mut derive = true;
            for sp in &s.outer_attrs.sp_list {
                if sp == r#""no-derive-debug""# {
                    derive = false;
                    break;
                }
            }
            if derive {
                p.indent();
                p.push_raw("#[derive(Debug)]\n");
            }
        }
        s.outer_attrs.to_rust(p);

        s.payload.to_rust_before(p);
        s.vis.to_rust(p);
        s.payload.to_rust(p);
    }
}

#[derive(Debug)]
pub enum ItemPayload {
    Mod(Mod),
    Crate(Crate),
    Extern(ExternalBlock),
    Use(Use),
    Func(Box<Func>),
    TypeAlias(TypeAlias),
    Struct(Struct),
    Enum(Enum),
    Union(Union),
    Const(Const),
    Static(Static),
    Trait(Trait),
    Impl(Impl),
    Macro(Macro),
    MacroCall(MacroCall),
}
impl Default for ItemPayload {
    fn default() -> Self {
        ItemPayload::Crate(Crate::default())
    }
}

impl ItemPayload {
    fn to_rust_before(&self, p: &mut LangFormatter) {
        let s = self;
        use ItemPayload as A;

        let t_a = Token::default();

        let first_u_token = match s {
            A::Mod(_a) => &t_a,
            A::Crate(a) => &a.t_extern,
            A::Extern(_a) => &t_a,
            A::Use(a) => &a.t_use,
            A::Func(_a) => &t_a,
            A::TypeAlias(_a) => &t_a,
            A::Struct(_a) => &t_a,
            A::Enum(_a) => &t_a,
            A::Union(_a) => &t_a,
            A::Const(_a) => &t_a,
            A::Static(_a) => &t_a,
            A::Trait(_a) => &t_a,
            A::Impl(_a) => &t_a,
            A::Macro(_a) => &t_a,
            A::MacroCall(_a) => &t_a,
        };
        first_u_token.to_rs_before(p);
    }
}

impl ToLang for ItemPayload {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;
        use ItemPayload as A;

        match s {
            A::Mod(a) => a.to_rust(p),
            A::Crate(a) => a.to_rust(p),
            A::Extern(a) => a.to_rust(p),
            A::Use(a) => a.to_rust(p),
            A::Func(a) => a.to_rust(p),
            A::TypeAlias(a) => a.to_rust(p),
            A::Struct(a) => a.to_rust(p),
            A::Enum(a) => a.to_rust(p),
            A::Union(a) => a.to_rust(p),
            A::Const(a) => a.to_rust(p),
            A::Static(a) => a.to_rust(p),
            A::Trait(a) => a.to_rust(p),
            A::Impl(a) => a.to_rust(p),
            A::Macro(a) => a.to_rust(p),
            A::MacroCall(a) => {
                a.to_rust(p);
                // for macro calls in item
                if matches!(a.body, MacroCallBody::Expr(_)) {
                    p.push_raw(";");
                }
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct Mod {
    pub name: Identifier,
    pub inner_attrs: Attrs,
    pub items: Option<Vec<Item>>,

    pub span_mod: Span,
    pub span_brace_open: Option<Span>,
    pub span_brace_close: Option<Span>,
}

impl ToLang for Mod {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_s("mod", s.span_mod);
        p.push_raw(" ");
        p.push_rust(&s.name);
        let Some(items) = &s.items else {
            p.push_raw(";");
            return;
        };

        p.push_raw(" ");
        p.push_s("{", s.span_brace_open.unwrap());
        p.push_raw("\n");

        p.inc_indent();
        p.push_rust(&s.inner_attrs);
        for item in items {
            p.push_rust(item);
        }
        p.dec_indent();
        p.indent();
        p.push_s("}", s.span_brace_close.unwrap());
    }
}

#[derive(Debug, Default)]
pub struct Crate {
    pub name: Token,
    // AS, ALIAS
    pub alias: Option<(Token, Token)>,

    pub t_extern: Token,
}
impl ToLang for Crate {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("extern crate ");
        s.name.to_rs_one(p);
        if let Some(alias) = &s.alias {
            alias.0.to_rust(p);
            alias.1.to_rust(p);
        }
        p.push_raw(";");
    }
}

#[derive(Debug, Default)]
pub struct Use {
    pub item: UseItem,
    pub t_use: Token,
}
impl ToLang for Use {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("use");
        s.item.to_rust(p);
        p.push_raw(";");
    }
}
#[derive(Debug, Default)]
pub struct UseItem {
    // paths may end with .. when ..{}
    pub paths: Vec<Token>,
    pub alias: Option<(Token, Token)>,
    pub sub: Option<UseItemSub>,
}
#[derive(Debug, Default)]
pub struct UseItemSub {
    pub items: Vec<(UseItem, Option<Token>)>,
    pub leading_sep: bool,

    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
impl ToLang for UseItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if !s.paths.is_empty() {
            let list = if let TokenCode::Identifier(id) = &s.paths[0].code && id.id == "-"{
                s.paths[0].to_rs_before(p);
                &s.paths[1..]
            } else {
                &s.paths
            };
            for it in list {
                it.to_rust(p);
            }
            if let Some(alias) = &s.alias {
                alias.0.to_rust(p);
                alias.1.to_rust(p);
            }
        }
        if let Some(sub) = &s.sub {
            sub.t_brace_open.to_rust(p);
            if sub.leading_sep {
                p.push_raw("::");
            }
            for it in &sub.items {
                let (item, comma) = it;
                item.to_rust(p);
                if let Some(comma) = comma {
                    comma.to_rust(p);
                }
            }
            sub.t_brace_close.to_rust(p);
        }
    }
}

#[derive(Debug, Default)]
pub struct Func {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub qualifier: FuncQualifier,
    pub no_s: bool,

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
            p.push_s("const", s.qualifier.span_const.unwrap());
            p.push_raw(" ");
        }
        if s.qualifier.async_ {
            p.push_s("async", s.qualifier.span_async.unwrap());
            p.push_raw(" ");
        }
        if s.qualifier.unsafe_ {
            p.push_s("unsafe", s.qualifier.span_unsafe.unwrap());
            p.push_raw(" ");
        }
        if s.qualifier.safe {
            p.push_s("safe", s.qualifier.span_safe.unwrap());
            p.push_raw(" ");
        }
        if let Some(extern_) = &s.qualifier.extern_ {
            p.push_s("extern", s.qualifier.span_extern.unwrap());
            p.push_raw(" ");
            if let Some(abi) = extern_ {
                abi.push_span(p, s.qualifier.span_abi.unwrap());
                p.push_raw(" ");
            }
        }

        p.push_s("fn", s.span_func);
        p.push_raw(" ");
        p.push_rust(&s.name);

        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }

        p.push_s("(", s.span_paren_open);
        if let Some(self_param) = &s.self_param {
            p.push_rust(self_param);
            if !s.params.is_empty() {
                p.push_raw(", ");
            }
        }
        for (i, param) in s.params.iter().enumerate() {
            p.push_rust(&param.outer_attrs);
            if let Some(pat) = &param.pat {
                p.push_rust(pat);
            } else {
                p.push_rust(&param.name);
            }
            p.push_raw(": ");
            p.push_rust(&param.type_);
            if i != s.params.len() - 1 {
                p.push_raw(", ");
            }
        }
        if let Some(v) = &s.variadics {
            p.push_raw(", ");
            p.push_rust(&v.outer_attrs);
            p.push_s("...", v.span);
        }
        p.push_s(")", s.span_paren_close);

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
            p.push_s("{", body.span_brace_open);
            p.push_raw("\n");
            p.inc_indent();
            p.push_rust(&body.inner_attrs);

            if !s.no_s && s.self_param.is_some() && !body.stmts.is_empty() {
                p.indent();
                p.push_raw("let s = self;\n");
            }

            for item in &body.stmts {
                p.push_rust(item);
            }
            p.dec_indent();
            p.indent();
            p.push_s("}", body.span_brace_close);
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
                if let Some(span) = a.span_mut {
                    p.push_s("mut ", span);
                    if a.typ.is_none() {
                        p.push_raw("self");
                        return;
                    }
                }
                p.push_raw("self: ");
                if let Some(typ) = &a.typ {
                    p.push_rust(typ);
                }
            }
            SelfParamPayload::Short(a) => {
                if let Some(span) = a.span_self {
                    p.push_s("self", span);
                    return;
                }
                p.push_s("&", a.span_ref);
                if let Some(label) = &a.label {
                    p.push_rust(label);
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_s("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                }
                p.push_raw("self");
            }
        }
    }
}
#[derive(Debug)]
pub enum SelfParamPayload {
    Short(SelfParamShort),
    Type(SelfParamType),
}
impl Default for SelfParamPayload {
    fn default() -> Self {
        SelfParamPayload::Short(SelfParamShort::default())
    }
}
#[derive(Debug, Default)]
pub struct SelfParamShort {
    pub label: Option<Token>,
    pub mut_: bool,

    pub span_self: Option<Span>,
    pub span_ref: Span,
    pub span_mut: Option<Span>,
}
#[derive(Debug, Default)]
pub struct SelfParamType {
    pub typ: Option<Type>,

    pub span_mut: Option<Span>,
}
#[derive(Debug, Default)]
pub struct FuncParam {
    pub outer_attrs: Attrs,
    pub name: Identifier,
    pub pat: Option<Pattern>,
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
    pub type_where: Option<Where>,

    pub span_type: Span,
    pub span_eq: Option<Span>,
}
impl ToLang for TypeAlias {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_s("type", s.span_type);
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
            p.push_s("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(type_);
            if let Some(where_) = &s.type_where {
                p.push_rust(where_);
            }
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

        p.push_s("struct", s.span_struct);
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
                    p.push_s("{", s.span_brace_open.unwrap());
                    p.push_raw("\n");

                    p.inc_indent();
                    for item in a {
                        p.push_rust(&item.outer_attrs);
                        p.indent();
                        p.push_rust(&item.vis);
                        p.push_rust(&item.name);
                        p.push_raw(": ");
                        p.push_rust(&item.type_);
                        p.push_raw(",\n");
                    }
                    p.dec_indent();
                    p.indent();
                    p.push_s("}", s.span_brace_close.unwrap());
                }
                StructPayload::Tuple(a) => {
                    p.push_s("(", s.span_paren_open.unwrap());
                    for (i, item) in a.iter().enumerate() {
                        if i != 0 {
                            p.push_raw(", ");
                        }
                        p.push_rust(&item.outer_attrs);
                        p.push_rust(&item.vis);
                        p.push_rust(&item.type_);
                    }
                    p.push_s(")", s.span_paren_close.unwrap());
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
    pub vis: Visibility,
    pub type_: Type,
}
#[derive(Debug, Default)]
pub struct TupleField {
    pub outer_attrs: Attrs,
    pub vis: Visibility,
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

        p.push_s("enum", s.span_enum);
        p.push_raw(" ");
        p.push_rust(&s.name);
        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }
        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }
        p.push_raw(" ");
        p.push_s("{", s.span_brace_open.unwrap());
        p.push_raw("\n");

        p.inc_indent();
        for item in &s.items {
            p.push_rust(item);
            p.push_raw(",\n");
        }
        p.dec_indent();
        p.indent();
        p.push_s("}", s.span_brace_close.unwrap());
    }
}
#[derive(Debug, Default)]
pub struct EnumItem {
    pub outer_attrs: Attrs,
    pub name: Identifier,
    // pub vis: Visibility, // it's not needed
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
        // p.push_rust(&s.vis);
        p.push_rust(&s.name);

        match &s.payload {
            EnumItemPayload::Struct(a) => {
                p.push_raw(" ");
                p.push_s("{", s.span_brace_open.unwrap());
                p.push_raw("\n");
                p.inc_indent();
                for item in a {
                    p.indent();
                    // p.push_rust(&item.vis);
                    p.push_rust(&item.name);
                    p.push_raw(": ");
                    p.push_rust(&item.type_);
                    p.push_raw(",\n");
                }
                p.dec_indent();
                p.indent();
                p.push_s("}", s.span_brace_close.unwrap());
            }
            EnumItemPayload::Tuple(a) => {
                p.push_s("(", s.span_paren_open.unwrap());
                for (i, item) in a.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(&item.outer_attrs);
                    // p.push_rust(&item.vis);
                    p.push_rust(&item.type_);
                }
                p.push_s(")", s.span_paren_close.unwrap());
            }
            EnumItemPayload::None => {}
        }
        if let Some(a) = &s.discriminant {
            p.push_raw(" ");
            p.push_s("=", s.span_eq.unwrap());
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

        p.push_s("union", s.span_union);
        p.push_raw(" ");
        p.push_rust(&s.name);
        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }

        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }
        p.push_raw(" ");
        p.push_s("{", s.span_brace_open.unwrap());
        p.push_raw("\n");

        p.inc_indent();
        for item in &s.fields {
            p.push_rust(&item.outer_attrs);
            p.indent();
            p.push_rust(&item.vis);
            p.push_rust(&item.name);
            p.push_raw(": ");
            p.push_rust(&item.type_);
            p.push_raw(",\n");
        }
        p.dec_indent();
        p.indent();
        p.push_s("}", s.span_brace_close.unwrap());
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

        p.push_s("const", s.span_const);
        p.push_raw(" ");
        p.push_rust(&s.name);
        p.push_raw(": ");
        p.push_rust(&s.type_);
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_s("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(expr);
        }
        p.push_raw(";");
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
            p.push_s("safe", s.span_safe.unwrap());
            p.push_raw(" ");
        }
        p.push_s("static", s.span_static);
        p.push_raw(" ");
        if s.mut_ {
            p.push_s("mut", s.span_mut.unwrap());
            p.push_raw(" ");
        }
        p.push_rust(&s.name);
        p.push_raw(": ");
        p.push_rust(&s.type_);
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_s("=", s.span_eq.unwrap());
            p.push_raw(" ");
            p.push_rust(expr);
        }
        p.push_raw(";");
    }
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Trait {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,

    pub unsafe_: bool,
    pub bounds: TypeParamBounds,
    pub inner_attrs: Attrs,
    pub items: Vec<AssociatedItem>,

    pub span_trait: Span,
    pub span_unsafe: Option<Span>,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for Trait {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.unsafe_ {
            p.push_s("unsafe", s.span_unsafe.unwrap());
            p.push_raw(" ");
        }
        p.push_s("trait", s.span_trait);
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
        p.push_s("{", s.span_brace_open);
        p.push_raw("\n");
        p.inc_indent();
        p.push_rust(&s.inner_attrs);
        for item in &s.items {
            p.push_rust(item);
        }
        p.dec_indent();
        p.indent();
        p.push_s("}", s.span_brace_close);
    }
}

#[derive(Debug)]
pub struct AssociatedItem {
    pub outer_attrs: Attrs,
    pub vis: Visibility,
    pub payload: AssociatedItemPayload,
}
impl ToLang for AssociatedItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.indent();
        p.push_rust(&s.outer_attrs);
        // vis only needed here
        if p.in_impl_static {
            if !matches!(&s.payload, AssociatedItemPayload::MacroCall(_)) {
                p.push_rust(&s.vis);
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

        if let ImplPayload::Trait(a) = &s.payload {
            if a.unsafe_ {
                p.push_s("unsafe", a.span_unsafe);
                p.push_raw(" ");
            }
        }
        p.push_s("impl", s.span_impl);

        if let Some(generics) = &s.generics {
            p.push_rust(generics);
        }

        p.push_raw(" ");
        match &s.payload {
            ImplPayload::Static => {
                p.push_rust(&s.type_);
            }
            ImplPayload::Trait(a) => {
                p.push_rust(&a.type_path);
                p.push_raw(" for ");
                p.push_rust(&s.type_);
            }
        }

        if let Some(where_) = &s.where_ {
            p.push_rust(where_);
        }

        p.push_raw(" ");
        p.push_s("{", s.span_brace_open);
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
        p.push_s("}", s.span_brace_close);
    }
}

#[derive(Debug, Default)]
pub enum ImplPayload {
    #[default]
    Static,
    Trait(ImplTrait),
}

#[derive(Debug, Default)]
pub struct ImplTrait {
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

        p.push_s("macro_rules!", s.span_macro);
        p.push_raw(" ");
        p.push_rust(&s.name);

        p.push_raw(" ");
        p.push_s("{", s.span_brace_open);

        tokens_to_rust(s.span_brace_open, &s.tokens, s.span_brace_close, p);

        p.push_s("}", s.span_brace_close);
    }
}

pub fn tokens_to_rust(span_open: Span, list: &[Token], span_close: Span, p: &mut LangFormatter) {
    if list.is_empty() {
        return;
    }

    let mut last_line = span_open.end_line;
    let mut last_end = span_open.end_column;
    let mut is_attr = false;

    for item in list {
        let cur_line = item.span.line;
        if cur_line > last_line {
            push_many_raw(cur_line - last_line, "\n", p);
            last_end = 0;
        }
        push_many_raw(item.span.column - last_end - 1, " ", p);
        last_line = item.span.end_line;
        last_end = item.span.end_column;

        // for attrs in macro and macro call, #[ ]
        match &item.code {
            T!["#["] | T!["#!["] => {
                is_attr = true;
                p.push_rust(item);
            }
            T!["]"] => {
                if !is_attr {
                    p.push_rust(item);
                } else if is_attr {
                    is_attr = false;
                    p.push_rust(&Token {
                        span: item.span,
                        code: T!["}}"],
                        tokens_before: Vec::new(),
                        u: "]".to_owned(),
                    });
                }
            }
            _ => {
                p.push_rust(item);
            }
        }
    }

    let cur_line = span_close.line;
    if cur_line > last_line {
        push_many_raw(cur_line - last_line, "\n", p);
        last_end = 0;
    }
    push_many_raw(span_close.column - last_end - 1, " ", p);

    fn push_many_raw(n: usize, text: &str, p: &mut LangFormatter) {
        for _ in 0..n {
            p.push_raw(text);
        }
    }
}

#[derive(Debug, Default)]
pub struct MacroCall {
    pub path: SimplePath,
    pub body: MacroCallBody,
    // 1: (), 2: [], 0: {}
    pub delim: i32,

    pub span_macro_call: Span,
    pub span_delim_open: Span,
    pub span_delim_close: Span,
}
impl ToLang for MacroCall {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.path);
        p.push_s("!", s.span_macro_call);

        match &s.body {
            MacroCallBody::Token(list) => {
                let pairs = match s.delim {
                    1 => ("(", ")"),
                    2 => ("[", "]"),
                    _ => ("{", "}"),
                };
                p.push_s(pairs.0, s.span_delim_open);
                tokens_to_rust(s.span_delim_open, list, s.span_delim_close, p);
                p.push_s(pairs.1, s.span_delim_close);
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
        }
    }
}
#[derive(Debug)]
pub enum MacroCallBody {
    Token(Vec<Token>), // ,,{} ,,[p]{} ,,[s]{}
    Expr(Vec<Expr>),   // ,,()
    Stmt(Vec<Stmt>),   // ,,[stmt]{}
}
impl Default for MacroCallBody {
    fn default() -> Self {
        MacroCallBody::Token(Vec::new())
    }
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
            p.push_s("unsafe", s.span_unsafe.unwrap());
            p.push_raw(" ");
        }
        p.push_s("extern", s.span_extern);
        p.push_raw(" ");
        if let Some(abi) = &s.extern_ {
            abi.push_span(p, s.span_abi.unwrap());
            p.push_raw(" ");
        }
        p.push_s("{", s.span_brace_open);
        p.push_raw("\n");

        p.inc_indent();
        for item in &s.items {
            p.push_rust(item);
            p.push_raw("\n");
        }
        p.dec_indent();
        p.indent();
        p.push_s("}", s.span_brace_close);
    }
}
#[derive(Debug)]
pub struct ExternalBlockItem {
    pub outer_attrs: Attrs,
    pub vis: Visibility,
    pub payload: ExternalBlockItemPayload,
}
impl ToLang for ExternalBlockItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        p.indent();
        p.push_rust(&s.vis);
        match &s.payload {
            ExternalBlockItemPayload::Func(a) => p.push_rust(a),
            ExternalBlockItemPayload::Static(a) => p.push_rust(a),
            ExternalBlockItemPayload::MacroCall(a) => p.push_rust(a),
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
    pub sp_list: Vec<String>,
}
impl ToLang for Attrs {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for it in &s.items {
            it.to_rust(p);
        }
    }
}

#[derive(Debug, Default)]
pub struct Attr {
    pub tokens: Vec<Token>,

    pub t_start: Token,
    pub t_end: Token,
}
impl ToLang for Attr {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.tokens.is_empty() {
            return;
        }

        s.t_start.to_rust(p);
        for it in &s.tokens {
            it.to_rust(p);
        }
        s.t_end.to_rust(p);
    }
}

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

        p.push_s("<", s.span_square_open);
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_s(">", s.span_square_close);
    }
}
#[derive(Debug, Default)]
pub struct GenericParam {
    pub outer_attrs: Attrs,
    pub payload: GenericParamPayload,
}
impl ToLang for GenericParam {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        match &s.payload {
            GenericParamPayload::Lifetime(a) => {
                p.push_rust(&a.lifetime);
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
                p.push_rust(&a.name);
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
                p.push_s("const", a.span_const);
                p.push_raw(" ");
                p.push_rust(&a.name);
                p.push_raw(": ");
                p.push_rust(&a.type_);
                if let Some(expr) = &a.expr {
                    p.push_raw(" = ");
                    p.push_rust(expr);
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
    pub lifetime: Token,
    pub lifetime_bounds: Vec<Lifetime>,
}
#[derive(Debug, Default)]
pub struct Lifetime {
    pub name: Token,
}
impl ToLang for Lifetime {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.name);
    }
}
#[derive(Debug, Default)]
pub struct TypeParam {
    pub name: Identifier,
    pub bounds: TypeParamBounds,
    pub type_: Option<Type>,
}
// Identifier const Type
#[derive(Debug, Default)]
pub struct ConstParam {
    pub name: Identifier,
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
        p.push_s("where", s.span_where);
        p.push_raw(" ");
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
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
}
#[derive(Debug)]
pub enum TypeParamBound {
    Lifetime(Lifetime),
    TraitBound(TraitBound),
    UseBound(UseBound),
}
impl ToLang for TypeParamBound {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            TypeParamBound::Lifetime(a) => p.push_rust(a),
            TypeParamBound::TraitBound(a) => p.push_rust(a),
            TypeParamBound::UseBound(a) => p.push_rust(a),
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

        p.push_s("use", s.span_use);
        p.push_s("<", s.span_square_open);
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_s(">", s.span_square_close);
    }
}
impl ToLang for UseBoundGenericArg {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            UseBoundGenericArg::Lifetime(a) => p.push_rust(a),
            UseBoundGenericArg::Id(a) => p.push_rust(a),
            UseBoundGenericArg::Self_(a) => p.push_s("Self", *a),
        }
    }
}

#[derive(Debug, Default)]
pub struct TraitBound {
    pub unsized_: bool,
    pub for_lifetimes: Option<ForLifetimes>,
    pub type_path: TypePath,

    pub span_unsized: Option<Span>,
}
impl ToLang for TraitBound {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.unsized_ {
            p.push_s("?", s.span_unsized.unwrap());
        }
        if let Some(for_) = &s.for_lifetimes {
            p.push_rust(for_);
        }
        p.push_rust(&s.type_path);
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

        p.push_s("for", s.span_for);
        p.push_raw(" ");
        p.push_rust(&s.for_);
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
            } else if let SimplePathSegment::Identifier(a) = item && a.id == "-" {
                continue;
            }
            p.push_rust(item);
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
            SimplePathSegment::Super(span) => p.push_s("super", *span),
            SimplePathSegment::SelfValue(span) => p.push_s("self", *span),
            SimplePathSegment::Crate(span) => p.push_s("crate", *span),
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
            PathIdentSegment::Super(span) => p.push_s("super", *span),
            PathIdentSegment::SelfValue(span) => p.push_s("self", *span),
            PathIdentSegment::SelfType(span) => p.push_s("Self", *span),
            PathIdentSegment::Crate(span) => p.push_s("crate", *span),
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
            } else if let PathIdentSegment::Identifier(a) = &item.name && a.id == "-" {
                // -..crate-abc..Abc turns into ::crate_abc::Abc
                continue;
            }
            p.push_rust(item);
        }
    }
}
/* NOTE
MethodCallExpression →  Expression . PathExprSegment ( CallParams? )

PathExprSegment →  PathIdentSegment ( :: GenericArgs )?

GenericArgs →
    < GenericArgList? >
    | ( TypeList? ) ( -> TypeNoBounds )?

but, it's very rare to write code like
    a.b(i32)(1)
    a.b::(i32) -> i32(1)
even thougn they are correct.
so don't consider such cases for now.
*/
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

        p.push_s("<", s.span_square_open);
        p.push_rust(&s.type_);
        if let Some(type_path) = &s.type_path {
            p.push_raw(" as ");
            p.push_rust(type_path);
        }
        p.push_s(">", s.span_square_close);
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
            } else if let PathIdentSegment::Identifier(a) = &item.name && a.id == "-" {
                // -..crate-abc..Abc turns into ::crate_abc::Abc
                continue;
            }
            p.push_rust(item);
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

        p.push_s("(", s.span_paren_open);
        for (i, item) in s.inputs.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_s(")", s.span_paren_close);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_rust(return_type);
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

        p.push_s("<", s.span_square_open);
        for (i, item) in s.items.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_s(">", s.span_square_close);
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
                p.push_s("=", a.span_eq);
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
                    p.push_s("ref", a.span_ref.unwrap());
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_s("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                }
                p.push_rust(&a.name);
                if let Some(pattern) = &a.pattern {
                    p.push_raw(" ");
                    p.push_s("@", a.span_at.unwrap());
                    p.push_raw(" ");
                    p.push_rust(pattern.as_ref());
                }
            }
            PatternWithoutRange::Rest(span) => p.push_s("..", *span),
            PatternWithoutRange::Obsolete(a) => {
                if let Some(start) = &a.start {
                    p.push_rust(start);
                }
                p.push_s("...", a.span);
                if let Some(end) = &a.end {
                    p.push_rust(end);
                }
            }
            PatternWithoutRange::Reference(a) => {
                p.push_s("&", a.span_ref);
                if a.mut_ {
                    p.push_s("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                }
                p.push_rust(a.pattern_without_range.as_ref());
            }
            PatternWithoutRange::Struct(a) => {
                p.push_rust(&a.path);
                p.push_s("{", a.span_brace_open);
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                p.push_s("}", a.span_brace_close);
            }
            PatternWithoutRange::TupleStruct(a) => {
                p.push_rust(&a.path);
                p.push_s("(", a.span_paren_open);
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                p.push_s(")", a.span_paren_close);
            }
            PatternWithoutRange::Tuple(a) => {
                p.push_s("(", a.span_paren_open);
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
                        p.push_s("..", *span);
                        p.push_raw(" ");
                    }
                }
                p.push_s(")", a.span_paren_close);
            }
            PatternWithoutRange::Grouped(a) => {
                p.push_s("(", a.span_paren_open);
                p.push_rust(a.pattern.as_ref());
                p.push_s(")", a.span_paren_close);
            }
            PatternWithoutRange::Slice(a) => {
                p.push_s("[", a.span_square_open);
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                p.push_s("]", a.span_square_close);
            }
            PatternWithoutRange::Path(a) => match a {
                PatternPath::Path(b) => p.push_rust(b),
                PatternPath::QualifiedPath(b) => p.push_rust(b),
            },
            PatternWithoutRange::MacroCall(a) => p.push_rust(a),
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
            p.push_s("-", s.span_minus.unwrap());
        }
        p.push_rust(&s.name);
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
                        p.push_s(&a.tuple_index, a.span_tuple_index);
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
                            p.push_s("ref", c.span_ref.unwrap());
                            p.push_raw(" ");
                        }
                        if c.mut_ {
                            p.push_s("mut", c.span_mut.unwrap());
                            p.push_raw(" ");
                        }
                        p.push_rust(&c.name);
                    }
                }
            }
            PatternStructItem::Etc(a, span) => {
                p.push_rust(a);
                p.push_s("..", *span);
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
            p.push_s("..=", s.span);
        } else {
            p.push_s("..", s.span);
        }
        if let Some(end) = &s.end {
            p.push_rust(end);
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
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Type {
    pub payload: TypePayload,
    pub u_pair: UPair,
}
impl ToLang for Type {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;
        p.push_rust(&s.payload);
    }
}
#[derive(Debug)]
pub enum TypePayload {
    TypeNoBounds(Box<TypeNoBounds>),
    ImplTrait(TypeParamBounds, Span),
    TraitObject(TypeTraitObject),
}
impl Default for TypePayload {
    fn default() -> Self {
        TypePayload::ImplTrait(TypeParamBounds::default(), Span::default())
    }
}
impl ToLang for TypePayload {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            TypePayload::TypeNoBounds(a) => p.push_rust(a.as_ref()),
            TypePayload::ImplTrait(a, span) => {
                p.push_s("impl", *span);
                p.push_raw(" ");
                p.push_rust(a);
            }
            TypePayload::TraitObject(a) => {
                p.push_s("dyn", a.span);
                p.push_raw(" ");
                p.push_rust(&a.bounds);
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeNoBounds {
    Parenthesized(TypeParenthesized),                     // ( Type )
    ImplTraitOneBound(TypeImplTraitOneBound),     // impl TraitBound
    TraitObjectOneBound(TypeTraitObjectOneBound), // dyn TraitBound
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
                p.push_s("(", a.span_paren_open);
                p.push_rust(&a.type_);
                p.push_s(")", a.span_paren_close);
            }
            TypeNoBounds::ImplTraitOneBound(a) => {
                p.push_s("impl", a.span_impl);
                p.push_raw(" ");
                p.push_rust(&a.bound);
            }
            TypeNoBounds::TraitObjectOneBound(a) => {
                p.push_s("dyn", a.span_dyn);
                p.push_raw(" ");
                p.push_rust(&a.bound);
            }
            TypeNoBounds::Tuple(a) => {
                p.push_s("(", a.span_paren_open);
                for (i, item) in a.items.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(", ");
                    }
                    p.push_rust(item);
                }
                if a.items.len() == 1 {
                    p.push_raw(", ");
                }
                p.push_s(")", a.span_paren_close);
            }
            TypeNoBounds::RawPointer(a) => {
                p.push_s("*", a.span_pointer);
                if a.mut_ {
                    p.push_s("mut", a.span_mut);
                } else {
                    p.push_s("const", a.span_mut);
                }
                p.push_raw(" ");
                p.push_rust(&*a.type_no_bounds);
            }
            TypeNoBounds::Reference(a) => {
                p.push_s("&", a.span_ref);
                if let Some(lifetime) = &a.lifetime {
                    p.push_rust(lifetime);
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_s("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                }
                p.push_rust(&*a.type_no_bounds);
            }
            TypeNoBounds::Array(a) => {
                p.push_s("[", a.span_double_brace_open);
                p.push_rust(&a.type_);
                p.push_s(";", a.span_semicolon);
                p.push_raw(" ");
                p.push_rust(&a.expr);
                p.push_s("]", a.span_double_brace_close);
            }
            TypeNoBounds::Slice(a) => {
                p.push_s("[", a.span_double_brace_open);
                p.push_rust(&a.type_);
                p.push_s("]", a.span_double_brace_close);
            }
            TypeNoBounds::TypePath(a) => p.push_rust(a),
            TypeNoBounds::QualifiedPathInType(a) => p.push_rust(a),
            TypeNoBounds::Never(span) => p.push_s("!", *span),
            TypeNoBounds::BareFunction(a) => p.push_rust(a.as_ref()),
            TypeNoBounds::MacroCall(a) => p.push_rust(a),
        }
    }
}

#[derive(Debug, Default)]
pub struct TypeTraitObject {
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
pub struct TypeImplTraitOneBound {
    pub bound: TraitBound,

    pub span_impl: Span,
}

#[derive(Debug)]
pub struct TypeTraitObjectOneBound {
    pub bound: TraitBound,

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
    pub return_type: Option<Type>,
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
            p.push_s("unsafe", s.qualifier.span_unsafe.unwrap());
            p.push_raw(" ");
        }
        if let Some(extern_) = &s.qualifier.extern_ {
            p.push_s("extern", s.qualifier.span_extern.unwrap());
            p.push_raw(" ");
            if let Some(abi) = extern_ {
                abi.push_span(p, s.qualifier.span_abi.unwrap());
                p.push_raw(" ");
            }
        }

        match s.func_type {
            FuncType::Func(span) => p.push_s("fn", span),
            FuncType::Other => {}
        }

        p.push_s("(", s.span_paren_open);
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
            p.push_s("...", v.span);
        }
        p.push_s(")", s.span_paren_close);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_rust(return_type);
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
}

#[derive(Debug, Default)]
pub struct StmtLet {
    pub outer_attrs: Attrs,
    pub pattern: Pattern,
    pub type_: Option<Type>,
    pub expr: Option<Expr>,
    pub else_: Option<BlockExpr>,

    pub span_eq: Span,
    pub span_else: Option<Span>,
}
impl ToLang for StmtLet {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        p.indent();
        p.push_raw("let ");
        p.push_rust(&s.pattern);
        if let Some(type_) = &s.type_ {
            p.push_raw(": ");
            p.push_rust(type_);
        }
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_s("=", s.span_eq);
            p.push_raw(" ");
            p.push_rust(expr);
        }
        if let Some(else_) = &s.else_ {
            p.push_raw(" ");
            p.push_s("else", s.span_else.unwrap());
            p.push_raw(" ");
            p.push_rust(else_);
        }
        p.push_raw(";\n");
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
    pub label: Option<Token>,
    pub stmts: Vec<Stmt>,

    pub span_label: Option<Span>,
    pub span_brace_open: Span,
    pub span_brace_close: Span,
}
impl ToLang for BlockExpr {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(label) = &s.label {
            p.push_rust(label);
            p.push_raw(": ");
        }

        p.push_s("{", s.span_brace_open);
        p.push_raw("\n");
        p.inc_indent();
        p.push_rust(&s.inner_attrs);
        for item in &s.stmts {
            p.push_rust(item);
        }
        p.dec_indent();
        p.indent();
        p.push_s("}", s.span_brace_close);
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

        p.push_s("async", s.span_async);
        if s.move_ {
            p.push_raw(" ");
            p.push_s("move", s.span_move.unwrap());
        }
        p.push_raw(" ");
        p.push_rust(&s.block);
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

        p.push_s("unsafe", s.span_unsafe);
        p.push_raw(" ");
        p.push_rust(&s.block);
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

        p.push_s("const", s.span_const);
        p.push_raw(" ");
        p.push_rust(&s.block);
    }
}
#[derive(Debug, Default)]
pub struct ExprLoop {
    pub label: Option<Token>,
    pub body: BlockExpr,
    pub payload: ExprLoopPayload,

    pub span_for: Span,
    pub span_label: Option<Span>,
}
impl ToLang for ExprLoop {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(label) = &s.label {
            p.push_rust(label);
            p.push_raw(": ");
        }

        match &s.payload {
            ExprLoopPayload::Loop => p.push_s("loop", s.span_for),
            ExprLoopPayload::While(a) => {
                p.push_s("while", s.span_for);
                p.push_rust(&a.conditions);
            }
            ExprLoopPayload::In(a) => {
                p.push_s("for", s.span_for);
                p.push_raw(" ");
                p.push_rust(&a.pattern);
                p.push_raw(" ");
                p.push_s("in", a.span_in);
                p.push_raw(" ");
                p.push_rust(&a.in_expr);
            }
        }

        p.push_raw(" ");
        p.push_rust(&s.body);
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
    pub conditions: Conditions,
}
// in
#[derive(Debug)]
pub struct ExprLoopIn {
    pub pattern: Pattern,
    pub in_expr: Expr, // except struct expression

    pub span_in: Span,
}

#[derive(Debug)]
pub enum Conditions {
    Expr(Expr),
    LetChain(Vec<LetChainCondition>),
}
impl Default for Conditions {
    fn default() -> Self {
        Conditions::LetChain(Vec::new())
    }
}
impl ToLang for Conditions {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw(" ");
        match &s {
            Conditions::Expr(expr) => {
                p.push_rust(expr);
            }
            Conditions::LetChain(list) => {
                for (i, item) in list.iter().enumerate() {
                    if i != 0 {
                        p.push_raw(" && ");
                    }
                    match item {
                        LetChainCondition::Expr(expr) => {
                            p.push_rust(expr);
                        }
                        LetChainCondition::Condition(a) => {
                            p.push_rust(&a.outer_attrs);
                            p.push_raw("let ");
                            p.push_rust(&a.pattern);
                            p.push_raw(" = ");
                            p.push_rust(&a.scrutinee);
                        }
                    }
                }
            }
        }
        p.push_raw(" ");
    }
}

#[derive(Debug)]
pub enum LetChainCondition {
    Expr(Expr),
    Condition(LetChainConditionItem),
}
impl Default for LetChainCondition {
    fn default() -> Self {
        LetChainCondition::Expr(Expr::default())
    }
}
#[derive(Debug)]
pub struct LetChainConditionItem {
    pub outer_attrs: Attrs,
    pub pattern: Pattern,
    pub scrutinee: Expr,
}

#[derive(Debug, Default)]
pub struct ExprIf {
    pub conditions: Conditions,
    pub body: BlockExpr,
    pub else_: Option<ExprIfElse>,

    pub span_if: Span,
    pub span_else: Option<Span>,
}
impl ToLang for ExprIf {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_s("if", s.span_if);
        p.push_rust(&s.conditions);
        p.push_rust(&s.body);

        if let Some(else_) = &s.else_ {
            p.push_raw(" ");
            p.push_s("else", s.span_else.unwrap());
            p.push_raw(" ");
            match else_ {
                ExprIfElse::BlockExpr(a) => p.push_rust(a),
                ExprIfElse::If(a) => p.push_rust(a.as_ref()),
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

        p.push_s("match", s.span_match);
        p.push_raw(" ");
        p.push_rust(&s.expr);

        p.push_raw(" ");
        p.push_s("{", s.span_brace_open);
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
        p.push_s("}", s.span_brace_close);
        p.push_raw("\n");
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
    pub pattern: Pattern,
    pub guard: Option<ExprMatchArmGuard>,
}
impl ToLang for ExprMatchArm {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.outer_attrs);
        p.push_rust(&s.pattern);
        if let Some(guard) = &s.guard {
            p.push_raw(" ");
            p.push_s("if", guard.span_if);
            p.push_raw(" ");
            p.push_rust(&guard.conditions);
        }
    }
}
#[derive(Debug)]
pub struct ExprMatchArmGuard {
    pub conditions: Conditions,

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
}

#[derive(Debug)]
pub struct ExprLiteral(pub Literal);
impl ToLang for ExprLiteral {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_rust(&s.0);
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
                p.push_s("&", a.span_ref);
                if a.raw {
                    p.push_s("raw", a.span_raw.unwrap());
                    p.push_raw(" ");
                }
                if a.mut_ {
                    p.push_s("mut", a.span_mut.unwrap());
                    p.push_raw(" ");
                } else if a.raw {
                    p.push_raw("const ");
                }
                p.push_rust(&a.expr);
            }
            ExprOperator::Dereference(a) => {
                p.push_s("*", a.span);
                p.push_rust(&a.expr);
            }
            ExprOperator::ErrorPropagation(a) => {
                p.push_rust(&a.expr);
                p.push_s("?", a.span);
            }
            ExprOperator::Negation(a) => {
                if a.not {
                    p.push_s("!", a.span);
                } else {
                    p.push_s("-", a.span);
                }
                p.push_rust(&a.expr);
            }
            ExprOperator::ArithmeticOrLogical(a) => {
                p.push_rust(&a.left);
                p.push_raw(" ");
                if let TokenCode::Op(op) = &a.op {
                    match op {
                        Op::Add => p.push_s("+", a.span_op),
                        Op::Sub => p.push_s("-", a.span_op),
                        Op::Star => p.push_s("*", a.span_op),
                        Op::Div => p.push_s("/", a.span_op),
                        Op::Rem => p.push_s("%", a.span_op),
                        Op::BitAnd => p.push_s("&", a.span_op),
                        Op::BitOr => p.push_s("|", a.span_op),
                        Op::BitXor => p.push_s("^", a.span_op),
                        Op::LeftShift => p.push_s("<<", a.span_op),
                        Op::RightShift => p.push_s(">>", a.span_op),
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
                        Op::Eq => p.push_s("==", a.span_op),
                        Op::Neq => p.push_s("!=", a.span_op),
                        Op::Greater => p.push_s(">", a.span_op),
                        Op::Less => p.push_s("<", a.span_op),
                        Op::GreaterEq => p.push_s(">=", a.span_op),
                        Op::LessEq => p.push_s("<=", a.span_op),
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
                        Op::And => p.push_s("&&", a.span_op),
                        Op::Or => p.push_s("||", a.span_op),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_rust(&a.right);
            }
            ExprOperator::TypeCast(a) => {
                p.push_rust(&a.expr);
                p.push_raw(" as ");
                p.push_rust(&a.type_no_bounds);
            }
            ExprOperator::Assignment(a) => {
                p.push_rust(&a.left);
                p.push_raw(" ");
                p.push_s("=", a.span_op);
                p.push_raw(" ");
                p.push_rust(&a.right);
            }
            ExprOperator::CompoundAssignment(a) => {
                p.push_rust(&a.left);
                p.push_raw(" ");
                if let TokenCode::Assign(assign) = &a.op {
                    match assign {
                        Assign::Add => p.push_s("+=", a.span_op),
                        Assign::Sub => p.push_s("-=", a.span_op),
                        Assign::Star => p.push_s("*=", a.span_op),
                        Assign::Div => p.push_s("/=", a.span_op),
                        Assign::Rem => p.push_s("%=", a.span_op),
                        Assign::BitAnd => p.push_s("&=", a.span_op),
                        Assign::BitOr => p.push_s("|=", a.span_op),
                        Assign::BitXor => p.push_s("^=", a.span_op),
                        Assign::LeftShift => p.push_s("<<=", a.span_op),
                        Assign::RightShift => p.push_s(">>=", a.span_op),
                        _ => {}
                    }
                }
                p.push_raw(" ");
                p.push_rust(&a.right);
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

        p.push_s("(", s.span_paren_open);
        p.push_rust(&s.inner_attrs);
        p.push_rust(&s.expr);
        p.push_s(")", s.span_paren_close);
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

        p.push_s("[", s.span_double_brace_open);
        p.push_rust(&s.inner_attrs);
        match &s.payload {
            ExprArrayPayload::One(a) => {
                p.push_rust(&a.value);
                p.push_s(";", a.span_semicolon);
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
        p.push_s("]", s.span_double_brace_close);
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
        p.push_s(".await", s.span);
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
        p.push_s("[", s.span_double_brace_open);
        p.push_rust(&s.right);
        p.push_s("]", s.span_double_brace_close);
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

        p.push_s("(", s.span_paren_open);
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
        p.push_s(")", s.span_paren_close);
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
        p.push_s(".", s.span_dot);
        p.push_s(&s.index, s.span_index);
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
        p.push_s("{", s.span_brace_open);
        p.push_rust(&s.inner_attrs);
        for (i, item) in s.fields.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_s("}", s.span_brace_close);
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
                p.push_s(&a.tuple_index, a.span_index);
                p.push_raw(": ");
                p.push_rust(&a.expr);
            }
            ExprStructField::Base(a, span) => {
                p.push_s("..", *span);
                if let Some(a) = a {
                    p.push_rust(a);
                } else {
                    p.push_raw("Default::default()")
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
        p.push_s("(", s.span_paren_open);
        p.push_rust(&s.inner_attrs);
        for (i, item) in s.exprs.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_s(")", s.span_paren_close);
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
        p.push_s("(", s.span_paren_open);
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_s(")", s.span_paren_close);
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
        p.push_s(".", s.span_dot);
        p.push_rust(&s.path_expr_segment);
        p.push_s("(", s.span_paren_open);
        for (i, item) in s.params.iter().enumerate() {
            if i != 0 {
                p.push_raw(", ");
            }
            p.push_rust(item);
        }
        p.push_s(")", s.span_paren_close);
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
        p.push_s(".", s.span_dot);
        p.push_rust(&s.name);
    }
}
#[derive(Debug, Default)]
pub struct FuncParamOptionalType {
    pub outer_attrs: Attrs,
    pub name: Identifier,
    pub pat: Option<Pattern>,
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
            p.push_s("async", s.span_async.unwrap());
            p.push_raw(" ");
        }
        if s.move_ {
            p.push_s("move", s.span_move.unwrap());
            p.push_raw(" ");
        }

        p.push_s("|", s.span_paren_open);
        for (i, item) in s.params.iter().enumerate() {
            p.push_rust(&item.outer_attrs);
            if let Some(pat) = &item.pat {
                p.push_rust(pat);
            } else {
                p.push_rust(&item.name);
            }
            if let Some(type_) = &item.type_ {
                p.push_raw(": ");
                p.push_rust(type_);
            }
            if i != s.params.len() - 1 {
                p.push_raw(", ");
            }
        }
        p.push_s("|", s.span_paren_close);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" -> ");
            p.push_rust(return_type);
        }
        p.push_raw(" ");
        p.push_rust(&s.expr);
    }
}
#[derive(Debug, Default)]
pub struct ExprContinue {
    pub label: Option<Token>,

    pub span_continue: Span,
    pub span_label: Option<Span>,
}
impl ToLang for ExprContinue {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_s("continue", s.span_continue);
        if let Some(identifier) = &s.label {
            p.push_raw(" ");
            p.push_rust(identifier);
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprBreak {
    pub label: Option<Token>,
    pub expr: Option<Expr>,

    pub span_break: Span,
    pub span_label: Option<Span>,
}
impl ToLang for ExprBreak {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_s("break", s.span_break);
        if let Some(identifier) = &s.label {
            p.push_raw(" ");
            p.push_rust(identifier);
        }
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_rust(expr);
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

        p.push_s("return", s.span_return);
        if let Some(expr) = &s.expr {
            p.push_raw(" ");
            p.push_rust(expr);
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
            p.push_s("..=", s.span);
        } else {
            p.push_s("..", s.span);
        }
        if let Some(end) = &s.end {
            p.push_rust(end);
        }
    }
}
