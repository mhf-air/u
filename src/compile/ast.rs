use super::lex::*;
use crate::T;
use std::convert::TryFrom;
use std::mem;

pub struct LangFormatter<'a> {
    buf: String,

    tokens: &'a [Token],
    end_tokens: &'a [Token],

    span_pairs: Vec<SpanPair>,
    line: usize,
    column: usize,

    in_impl_static: bool,
    no_first_before: bool,
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

            tokens,
            end_tokens,

            span_pairs: Vec::new(),
            line: 1,
            column: 1,

            in_impl_static: false,
            no_first_before: false,
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

    pub fn push_raw(&mut self, a: &str) {
        let s = self;
        s.buf.push_str(a);
    }
    pub fn push_raw_nl(&mut self, a: &str) {
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

/// NOTE the form is fixed: "pub(crate) NAME"
/// no whitespaces or comments within Visibility, and one space after
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
        s.to_rs(p, true);
    }
}
impl Visibility {
    fn to_rs(&self, p: &mut LangFormatter, with_space: bool) {
        let s = self;
        use VisibilityPayload as A;

        let mut none = false;
        match &s.payload {
            A::Private(is_self) => {
                if *is_self {
                    p.push_raw("pub(self)");
                } else {
                    none = true;
                }
            }
            A::Pub => {
                p.push_raw("pub");
            }
            A::Crate => {
                p.push_raw("pub(crate)");
            }
            A::Super => {
                p.push_raw("pub(super)");
            }
            A::SimplePath(path) => {
                p.push_raw("pub(in");
                path.to_rust(p);
                p.push_raw(")");
            }
        }
        if with_space && !none {
            p.push_raw(" ");
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

        let t_no_before = Token::default();

        let first_u_token = match s {
            A::Mod(a) => &a.name,
            A::Crate(a) => &a.t_extern,
            A::Extern(a) => &a.t_extern,
            A::Use(a) => &a.t_use,
            A::Func(a) => &a.name,
            A::TypeAlias(a) => &a.name,
            A::Struct(a) => &a.name,
            A::Enum(a) => &a.name,
            A::Union(a) => &a.name,
            A::Const(a) => &a.name,
            A::Static(a) => &a.name,
            A::Trait(a) => &a.name,
            A::Impl(_a) => &t_no_before,
            A::Macro(a) => &a.name,
            A::MacroCall(_a) => &t_no_before,
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
            A::MacroCall(a) => a.to_rust(p),
        }
    }
}

#[derive(Debug, Default)]
pub struct Mod {
    pub name: Token,
    pub inner_attrs: Attrs,
    pub items: Vec<Item>,

    pub t_mod: Token,
    pub t_braces: Option<(Token, Token)>,
}

impl ToLang for Mod {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_mod.to_rs_one(p);
        s.t_mod.to_rs_before(p);
        s.name.to_rs_one(p);
        let Some((t_open, t_close)) = &s.t_braces else {
            p.push_raw(";");
            return;
        };

        t_open.to_rust(p);
        s.inner_attrs.to_rust(p);
        for item in &s.items {
            item.to_rust(p);
        }
        t_close.to_rust(p);
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
    pub name: Token,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub qualifier: FuncQualifier,

    pub self_param: Option<SelfParam>,
    pub params: Vec<FuncParam>,
    pub variadics: Option<FuncVariadics>, // only functions in external blocks might have this
    pub return_type: Option<Type>,
    pub body: Option<BlockExpr>,

    pub t_func: Token,
    pub t_paren_open: Token,
    pub t_paren_close: Token,
    pub t_arrow: Option<Token>,
    pub t_first_after_paren: Token,
}
impl ToLang for Func {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        let mut is_first = true;

        // qualifier
        if let Some(t) = &s.qualifier.t_const {
            if is_first {
                t.to_rs_one(p);
                is_first = false;
            } else {
                t.to_rust(p);
            }
        }
        if let Some(t) = &s.qualifier.t_async {
            if is_first {
                t.to_rs_one(p);
                is_first = false;
            } else {
                t.to_rust(p);
            }
        }
        if let Some(t) = &s.qualifier.t_unsafe {
            if is_first {
                t.to_rs_one(p);
                is_first = false;
            } else {
                t.to_rust(p);
            }
        }
        if let Some((t, abi)) = &s.qualifier.t_extern {
            if is_first {
                t.to_rs_one(p);
                is_first = false;
            } else {
                t.to_rust(p);
            }
            if let Some(abi) = abi {
                abi.to_rust(p);
            }
        }

        // name
        if is_first {
            s.t_func.to_rs_one(p);
        } else {
            s.t_func.to_rust(p);
        }
        s.t_func.to_rs_before(p);
        s.name.to_rs_one(p);

        // generics
        if let Some(g) = &s.generics {
            g.to_rust(p);
        }

        // params
        s.t_paren_open.to_rust(p);
        if let Some(sp) = &s.self_param {
            // NOTE for multi-line params, only consider the common cases
            // not (
            //     &self
            // )
            let line_paren = s.t_paren_open.span.line;
            let line_first = s.t_first_after_paren.span.line;
            if line_paren != line_first {
                let t = &s.t_first_after_paren.tokens_before;
                if !t.is_empty() {
                    t[0].to_rs_one(p);
                }
            }
            sp.to_rust(p);
            if !s.params.is_empty() {
                p.push_raw(",");
                if line_paren == line_first {
                    p.push_raw(" ");
                }
            }
        }
        for param in &s.params {
            param.outer_attrs.to_rust(p);
            if let Some(pat) = &param.pat {
                pat.to_rust(p);
            } else {
                param.name.to_rust(p);
            }
            p.push_raw(":");
            param.type_.to_rust(p);
            if let Some(t) = &param.t_comma {
                t.to_rust(p);
            }
        }
        if let Some(v) = &s.variadics {
            v.outer_attrs.to_rust(p);
            v.t_var.to_rust(p);
            if let Some(t) = &v.t_comma {
                t.to_rust(p);
            }
        }
        s.t_paren_close.to_rust(p);

        // return type
        if let Some(return_type) = &s.return_type {
            if let Some(t) = &s.t_arrow {
                t.to_rs_before(p);
                p.push_raw("->");
            } else {
                p.push_raw(" ->");
            }
            return_type.to_rust(p);
        }

        // where
        if let Some(where_) = &s.where_ {
            where_.to_rust(p);
        }

        // body
        if let Some(body) = &s.body {
            if let Some(where_) = &s.where_ {
                where_.t_brace_close.to_rs_before(p);
                body.t_brace_open.to_rs_one(p);
            } else {
                body.t_brace_open.to_rust(p);
            }

            body.inner_attrs.to_rust(p);
            if s.qualifier.t_no_s.is_none() && s.self_param.is_some() && !body.stmts.is_empty() {
                let line_paren = body.t_brace_open.span.line;
                let line_first = body.t_first_after_brace.span.line;
                if line_paren != line_first {
                    let t = &body.t_first_after_brace.tokens_before;
                    if !t.is_empty() {
                        t[0].to_rs_one(p);
                    }
                    p.push_raw("let s = self;");
                } else {
                    p.push_raw(" let s = self;");
                }
            }
            for it in &body.stmts {
                it.to_rust(p);
            }
            body.t_brace_close.to_rust(p);
        } else {
            p.push_raw(";");
        }
    }
}
#[derive(Debug, Default)]
pub struct FuncQualifier {
    pub t_const: Option<Token>,
    pub t_async: Option<Token>,
    pub t_unsafe: Option<Token>,
    pub t_extern: Option<(Token, Option<Token>)>,
    pub t_no_s: Option<Token>,
}
#[derive(Debug, Default)]
pub struct SelfParam {
    pub outer_attrs: Attrs,
    pub payload: SelfParamPayload,
}
impl ToLang for SelfParam {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.outer_attrs.to_rust(p);
        match &s.payload {
            SelfParamPayload::Type(a) => {
                if let Some(t) = &a.t_mut {
                    t.to_rust(p);
                    p.push_raw(" self: ");
                } else {
                    p.push_raw("self: ");
                }
                a.typ.to_rust(p);
            }
            SelfParamPayload::Short(a) => {
                if let Some(t_ref) = &a.t_ref {
                    t_ref.to_rust(p);
                } else {
                    if let Some(t_mut) = &a.t_mut {
                        // (mut)
                        t_mut.to_rust(p);
                        p.push_raw(" self");
                    } else {
                        // ()
                        p.push_raw("self");
                    }
                    return;
                }
                if let Some(label) = &a.label {
                    label.to_rust(p);
                }
                if let Some(t_mut) = &a.t_mut {
                    t_mut.to_rust(p);
                }
                if a.label.is_none() && a.t_mut.is_none() {
                    p.push_raw("self");
                } else {
                    p.push_raw(" self");
                }
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
    pub t_ref: Option<Token>,
    pub label: Option<Token>,
    pub t_mut: Option<Token>,
}
#[derive(Debug, Default)]
pub struct SelfParamType {
    pub typ: Type,

    pub t_mut: Option<Token>,
}
#[derive(Debug, Default)]
pub struct FuncParam {
    pub outer_attrs: Attrs,
    pub name: Token,
    pub pat: Option<Pattern>,
    pub type_: Type,
    pub t_comma: Option<Token>,
}
#[derive(Debug)]
pub struct FuncVariadics {
    pub outer_attrs: Attrs,

    pub t_var: Token,
    pub t_comma: Option<Token>,
}

#[derive(Debug, Default)]
pub struct TypeAlias {
    pub name: Token,
    pub generics: Option<Generics>,
    pub bounds: TypeParamBounds,
    pub where_: Option<Where>,
    pub rhs: Option<(Token, Type, Option<Where>)>,

    pub t_type: Token,
}
impl ToLang for TypeAlias {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_type.to_rs_one(p);
        s.t_type.to_rs_before(p);
        s.name.to_rs_one(p);
        if let Some(generics) = &s.generics {
            generics.to_rust(p);
        }
        if !s.bounds.items.is_empty() {
            p.push_raw(":");
            s.bounds.to_rust(p);
        }
        if let Some(where_) = &s.where_ {
            where_.to_rust(p);
        }
        if let Some((t_eq, type_, where_)) = &s.rhs {
            if let Some(where_) = &s.where_ {
                where_.t_brace_close.to_rs_before(p);
                t_eq.to_rs_one(p);
            } else {
                t_eq.to_rust(p);
            }

            type_.to_rust(p);
            if let Some(where_) = &where_ {
                where_.to_rust(p);
            }
        }
        p.push_raw(";");
    }
}

#[derive(Debug, Default)]
pub struct Struct {
    pub name: Token,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub payload: Option<StructPayload>,

    pub t_struct: Token,
    pub t_brace_open: Option<Token>,
    pub t_brace_close: Option<Token>,
    pub t_paren_open: Option<Token>,
    pub t_paren_close: Option<Token>,
}
impl ToLang for Struct {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_struct.to_rs_one(p);
        s.t_struct.to_rs_before(p);
        s.name.to_rs_one(p);
        if let Some(generics) = &s.generics {
            generics.to_rust(p);
        }

        if let Some(payload) = &s.payload {
            match payload {
                StructPayload::Struct(a) => {
                    if let Some(where_) = &s.where_ {
                        where_.to_rust(p);
                    }
                    if let Some(t) = &s.t_brace_open {
                        if let Some(where_) = &s.where_ {
                            where_.t_brace_close.to_rs_before(p);
                            t.to_rs_one(p);
                        } else {
                            t.to_rust(p);
                        }
                    }
                    for it in a {
                        it.to_rust(p);
                    }
                    if let Some(t) = &s.t_brace_close {
                        t.to_rust(p);
                    }
                }
                StructPayload::Tuple(a) => {
                    if let Some(t) = &s.t_paren_open {
                        t.to_rust(p);
                    }
                    for it in a {
                        it.to_rust(p);
                    }
                    if let Some(t) = &s.t_paren_close {
                        t.to_rust(p);
                    }
                    if let Some(where_) = &s.where_ {
                        where_.to_rust(p);
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
    Struct(Vec<StructField>) ,
    Tuple(Vec<TupleField>),
}
#[derive(Debug, Default)]
pub struct StructField {
    pub outer_attrs: Attrs,
    pub name: Token,
    pub vis: Visibility,
    pub type_: Type,
    pub t_comma: Option<Token>,
}
impl ToLang for StructField {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.outer_attrs.to_rust(p);

        s.name.to_rs_before(p);
        s.vis.to_rust(p);
        s.name.to_rs_one(p);
        p.push_raw(":");

        s.type_.to_rust(p);
        if let Some(t) = &s.t_comma {
            t.to_rust(p);
        }
    }
}
#[derive(Debug, Default)]
pub struct TupleField {
    pub outer_attrs: Attrs,
    pub vis: Visibility,
    pub type_: Type,
    pub t_comma: Option<Token>,
}
impl ToLang for TupleField {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.outer_attrs.to_rust(p);
        s.vis.to_rs(p, false);
        s.type_.to_rust(p);
        if let Some(t) = &s.t_comma {
            t.to_rust(p);
        }
    }
}

#[derive(Debug, Default)]
pub struct Enum {
    pub name: Token,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub items: Vec<EnumItem>,

    pub t_enum: Token,
    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
impl ToLang for Enum {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_enum.to_rs_one(p);
        s.t_enum.to_rs_before(p);
        s.name.to_rs_one(p);

        if let Some(generics) = &s.generics {
            generics.to_rust(p);
        }
        if let Some(where_) = &s.where_ {
            where_.to_rust(p);
            where_.t_brace_close.to_rs_before(p);
            s.t_brace_open.to_rs_one(p);
        } else {
            s.t_brace_open.to_rust(p);
        }

        for it in &s.items {
            it.to_rust(p);
        }
        s.t_brace_close.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct EnumItem {
    pub outer_attrs: Attrs,
    pub name: Token,
    // pub vis: Visibility, // it's not needed
    pub payload: EnumItemPayload,
    pub discriminant: Option<(Token, Expr)>,

    pub t_brace_open: Option<Token>,
    pub t_brace_close: Option<Token>,
    pub t_paren_open: Option<Token>,
    pub t_paren_close: Option<Token>,
    pub t_eq: Option<Token>,
    pub t_comma: Option<Token>,
}
impl ToLang for EnumItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.outer_attrs.to_rust(p);
        s.name.to_rust(p);

        match &s.payload {
            EnumItemPayload::Struct(a) => {
                if let Some(t) = &s.t_brace_open {
                    t.to_rust(p);
                }
                for it in a {
                    it.to_rust(p);
                }
                if let Some(t) = &s.t_brace_close {
                    t.to_rust(p);
                }
            }
            EnumItemPayload::Tuple(a) => {
                if let Some(t) = &s.t_paren_open {
                    t.to_rust(p);
                }
                for it in a {
                    it.to_rust(p);
                }
                if let Some(t) = &s.t_paren_close {
                    t.to_rust(p);
                }
            }
            EnumItemPayload::None => {}
        }
        if let Some((t, expr)) = &s.discriminant {
            t.to_rust(p);
            expr.to_rust(p);
        }
        if let Some(comma) = &s.t_comma {
            comma.to_rust(p);
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
    pub name: Token,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,
    pub fields: Vec<StructField>,

    pub t_union: Token,
    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
impl ToLang for Union {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_union.to_rs_one(p);
        s.t_union.to_rs_before(p);
        s.name.to_rs_one(p);

        if let Some(generics) = &s.generics {
            generics.to_rust(p);
        }

        if let Some(where_) = &s.where_ {
            where_.to_rust(p);
            where_.t_brace_close.to_rs_before(p);
            s.t_brace_open.to_rs_one(p);
        } else {
            s.t_brace_open.to_rust(p);
        }

        for it in &s.fields {
            it.to_rust(p);
        }
        s.t_brace_close.to_rust(p);
    }
}

#[derive(Debug, Default)]
pub struct Const {
    pub name: Token,
    pub type_: Type,
    pub expr: Option<(Token, Expr)>,

    pub t_const: Token,
}
impl ToLang for Const {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("const");
        s.t_const.to_rs_before(p);
        s.name.to_rs_one(p);
        p.push_raw(":");
        s.type_.to_rust(p);
        if let Some((t_eq, expr)) = &s.expr {
            t_eq.to_rust(p);
            expr.to_rust(p);
        }
        p.push_raw(";");
    }
}

#[derive(Debug, Default)]
pub struct Static {
    pub name: Token,
    pub type_: Type,
    pub expr: Option<(Token, Expr)>,

    pub t_static: Token,
    pub t_safe: Option<Token>,
    pub t_mut: Option<Token>,
}
impl ToLang for Static {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(safe) = &s.t_safe {
            safe.to_rs_one(p);
            s.t_static.to_rust(p);
        } else {
            s.t_static.to_rs_one(p);
        }
        if let Some(mut_) = &s.t_mut {
            mut_.to_rust(p);
        }
        s.t_static.to_rs_before(p);
        s.name.to_rs_one(p);
        p.push_raw(":");
        s.type_.to_rust(p);
        if let Some((t_eq, expr)) = &s.expr {
            t_eq.to_rust(p);
            expr.to_rust(p);
        }
        p.push_raw(";");
    }
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Trait {
    pub name: Token,
    pub generics: Option<Generics>,
    pub where_: Option<Where>,

    pub bounds: TypeParamBounds,
    pub inner_attrs: Attrs,
    pub items: Vec<AssociatedItem>,

    pub t_trait: Token,
    pub t_unsafe: Option<Token>,
    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
impl ToLang for Trait {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(t) = &s.t_unsafe {
            t.to_rs_one(p);
            p.push_raw(" ");
            s.t_trait.to_rs_one(p);
        } else {
            s.t_trait.to_rs_one(p);
        }
        s.t_trait.to_rs_before(p);
        s.name.to_rs_one(p);
        if let Some(generics) = &s.generics {
            generics.to_rust(p);
        }
        if !s.bounds.items.is_empty() {
            p.push_raw(":");
            s.bounds.to_rust(p);
        }
        if let Some(where_) = &s.where_ {
            where_.to_rust(p);
            where_.t_brace_close.to_rs_before(p);
            s.t_brace_open.to_rs_one(p);
        } else {
            s.t_brace_open.to_rust(p);
        }

        s.inner_attrs.to_rust(p);
        for it in &s.items {
            it.to_rust(p);
        }
        s.t_brace_close.to_rust(p);
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
        use AssociatedItemPayload as A;

        s.outer_attrs.to_rust(p);

        let t_no_before = Token::default();
        let first_u_token = match &s.payload {
            A::Func(a) => &a.name,
            A::TypeAlias(a) => &a.name,
            A::Const(a) => &a.name,
            A::MacroCall(_a) => &t_no_before,
        };
        first_u_token.to_rs_before(p);

        // vis only needed here
        if p.in_impl_static {
            if !matches!(&s.payload, A::MacroCall(_)) {
                s.vis.to_rust(p);
            }
        }
        match &s.payload {
            A::Func(a) => a.to_rust(p),
            A::Const(a) => a.to_rust(p),
            A::TypeAlias(a) => a.to_rust(p),
            A::MacroCall(a) => a.to_rust(p),
        }
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

    pub t_impl: Token,
    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
impl ToLang for Impl {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let ImplPayload::Trait(a) = &s.payload && let Some(t) = &a.t_unsafe {
            s.t_impl.to_rs_before(p);
            t.to_rs_one(p);
            p.push_raw(" ");
            s.t_impl.to_rs_one(p);
        } else {
            s.t_impl.to_rust(p);
        }

        if let Some(generics) = &s.generics {
            generics.to_rust(p);
        }

        match &s.payload {
            ImplPayload::Static => {
                s.type_.to_rust(p);
            }
            ImplPayload::Trait(a) => {
                a.type_path.to_rust(p);
                a.t_for.to_rust(p);
                s.type_.to_rust(p);
            }
        }

        if let Some(where_) = &s.where_ {
            where_.to_rust(p);
            where_.t_brace_close.to_rs_before(p);
            s.t_brace_open.to_rs_one(p);
        } else {
            s.t_brace_open.to_rust(p);
        }

        s.inner_attrs.to_rust(p);
        if matches!(&s.payload, ImplPayload::Static) {
            p.in_impl_static = true;
        }
        for it in &s.items {
            it.to_rust(p);
        }
        if p.in_impl_static {
            p.in_impl_static = false;
        }
        s.t_brace_close.to_rust(p);
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
    pub type_path: TypePath,
    pub t_for: Token,
    pub t_unsafe: Option<Token>,
}

// --------------------------------------------------------------------------------
// as of now(rustc 1.50.0), no VisItem can appear before macro items, so don't generate them
// tokens are pure Rust tokens, except identifers and "~" and "~="
#[derive(Debug, Default)]
pub struct Macro {
    pub name: Token,
    pub tokens: Vec<Token>,

    pub t_macro: Token,
    pub t_delim_open: Token,
    pub t_delim_close: Token,
}
impl ToLang for Macro {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_macro.to_rs_one(p);
        p.push_raw("!");
        s.t_macro.to_rs_before(p);
        s.name.to_rs_one(p);

        if matches!(s.t_delim_open.code, T!["["]) {
            s.t_delim_open.to_rs_before(p);
            p.push_raw("[");
        } else {
            s.t_delim_open.to_rust(p);
        }

        let new_list = transform_macro_tokens(&s.tokens);
        for it in new_list {
            it.to_rust(p);
        }

        if matches!(s.t_delim_close.code, T!["]"]) {
            s.t_delim_close.to_rs_before(p);
            p.push_raw("]");
        } else {
            s.t_delim_close.to_rust(p);
        }

        if !matches!(s.t_delim_open.code, T!["{"]) {
            p.push_raw(";");
        }
    }
}

// tokens are pure Rust tokens, except identifers and "~" and "~="
#[derive(Debug, Default)]
pub struct MacroCall {
    pub path: SimplePath,
    pub body: MacroCallBody,
    // 1: (), 2: [], 0: {}
    pub delim: i32,
    pub is_stmt: bool,

    pub t_macro_call: Token,
    pub t_delim_open: Token,
    pub t_delim_close: Token,
}
impl ToLang for MacroCall {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.path.to_rust(p);
        s.t_macro_call.to_rust(p);

        match &s.body {
            MacroCallBody::Token(list) => {
                let pairs = match s.delim {
                    1 => ("(", ")"),
                    2 => ("[", "]"),
                    _ => ("{", "}"),
                };
                s.t_delim_open.to_rs_before(p);
                p.push_raw(pairs.0);

                let new_list = transform_macro_tokens(&list);
                for it in new_list {
                    it.to_rust(p);
                }

                s.t_delim_close.to_rs_before(p);
                p.push_raw(pairs.1);
                if s.is_stmt && s.delim > 0 {
                    p.push_raw(";");
                }
            }
            MacroCallBody::Expr(list) => {
                s.t_delim_open.to_rust(p);
                for it in list {
                    it.to_rust(p);
                }
                s.t_delim_close.to_rust(p);
                if s.is_stmt {
                    p.push_raw(";");
                }
            }
            MacroCallBody::Stmt(list) => {
                s.t_delim_open.to_rust(p);
                for it in list {
                    it.to_rust(p);
                }
                s.t_delim_close.to_rust(p);
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

// transform all '[' and ']' into 'rs[' and 'rs]'
fn transform_macro_tokens(tokens: &[Token]) -> Vec<Token> {
    let mut r = Vec::new();
    for it in tokens {
        let mut t = it.clone();
        t.code = match &it.code {
            T![func] => T!["rs:func"],
            T![macro] => T!["rs:macro"],
            T![ret] => T!["rs:ret"],
            T![..] => T!["rs:.."],
            T![...] => T!["rs:..."],
            T!["["] => T!["rs:["],
            T!["]"] => T!["rs:]"],
            _ => t.code,
        };
        r.push(t);
    }
    r
}

// --------------------------------------------------------------------------------
/*
extern[unsafe?] Abi? {
    foo func(x i32, ...)
    identifier static mut? Type
}
*/
#[derive(Debug, Default)]
pub struct ExternalBlock {
    pub inner_attrs: Attrs,
    pub items: Vec<ExternalBlockItem>,

    pub t_extern: Token,
    pub t_unsafe: Option<Token>,
    pub t_abi: Option<Token>,
    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
impl ToLang for ExternalBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(t) = &s.t_unsafe {
            t.to_rs_one(p);
            p.push_raw(" ");
        }
        s.t_extern.to_rs_one(p);
        if let Some(t) = &s.t_abi {
            t.to_rust(p);
        }

        s.t_brace_open.to_rust(p);
        for it in &s.items {
            it.to_rust(p);
        }
        s.t_brace_close.to_rust(p);
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

        s.outer_attrs.to_rust(p);
        s.vis.to_rust(p);
        match &s.payload {
            ExternalBlockItemPayload::Func(a) => {
                a.name.to_rs_before(p);
                a.to_rust(p);
            }
            ExternalBlockItemPayload::Static(a) => {
                a.name.to_rs_before(p);
                a.to_rust(p);
            }
            ExternalBlockItemPayload::MacroCall(a) => {
                // TODO
                a.to_rust(p);
            }
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

    pub t_square_open: Token,
    pub t_square_close: Token,
}
impl ToLang for Generics {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_square_open.to_rust(p);
        for it in &s.params {
            it.to_rust(p);
        }
        s.t_square_close.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct GenericParam {
    pub outer_attrs: Attrs,
    pub payload: GenericParamPayload,
    pub t_comma: Option<Token>,
}
impl ToLang for GenericParam {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.outer_attrs.to_rust(p);
        match &s.payload {
            GenericParamPayload::Lifetime(a) => {
                a.lifetime.to_rust(p);
                if !a.lifetime_bounds.is_empty() {
                    p.push_raw(":");
                    for (it, t) in &a.lifetime_bounds {
                        it.to_rust(p);
                        if let Some(t) = t {
                            t.to_rust(p);
                        }
                    }
                }
            }
            GenericParamPayload::Type(a) => {
                a.name.to_rust(p);
                if !a.bounds.items.is_empty() {
                    p.push_raw(":");
                    a.bounds.to_rust(p);
                }
                if let Some(type_) = &a.type_ {
                    type_.0.to_rust(p);
                    type_.1.to_rust(p);
                }
            }
            GenericParamPayload::Const(a) => {
                a.name.to_rs_before(p);
                a.t_const.to_rs_one(p);
                a.t_const.to_rs_before(p);
                a.name.to_rs_one(p);
                p.push_raw(":");
                a.type_.to_rust(p);
                if let Some((t, expr)) = &a.expr {
                    t.to_rust(p);
                    expr.to_rust(p);
                }
            }
        }
        if let Some(t) = &s.t_comma {
            t.to_rust(p);
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
    pub lifetime_bounds: Vec<(Lifetime, Option<Token>)>,
}
#[derive(Debug, Default)]
pub struct Lifetime {
    pub name: Token,
}
impl ToLang for Lifetime {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.name.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct TypeParam {
    pub name: Token,
    pub bounds: TypeParamBounds,
    pub type_: Option<(Token, Type)>,
}
// Identifier const Type
#[derive(Debug, Default)]
pub struct ConstParam {
    pub name: Token,
    pub type_: Type,
    pub expr: Option<(Token, Expr)>,

    pub t_const: Token,
}

// -----------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Where {
    pub items: Vec<(WhereItem, Option<Token>)>,

    pub t_where: Token,
    pub t_brace_close: Token,
}
impl ToLang for Where {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_where.to_rust(p);
        for (it, t) in &s.items {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
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
                a.lifetime.to_rust(p);
                p.push_raw(":");
                for (it, t) in &a.lifetime_bounds {
                    it.to_rust(p);
                    if let Some(t) = t {
                        t.to_rust(p);
                    }
                }
            }
            WhereItem::TypeBound(a) => {
                if let Some(for_) = &a.for_lifetimes {
                    for_.to_rust(p);
                }
                a.type_.to_rust(p);
                p.push_raw(":");
                a.bounds.to_rust(p);
            }
        }
    }
}
#[derive(Debug, Default)]
pub struct WhereItemLifetime {
    pub lifetime: Lifetime,
    pub lifetime_bounds: Vec<(Lifetime, Option<Token>)>,
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
    pub items: Vec<(TypeParamBound, Option<Token>)>,
}
impl ToLang for TypeParamBounds {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for (it, t) in &s.items {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
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
            TypeParamBound::Lifetime(a) => a.to_rust(p),
            TypeParamBound::TraitBound(a) => a.to_rust(p),
            TypeParamBound::UseBound(a) => a.to_rust(p),
        }
    }
}

#[derive(Debug, Default)]
pub struct UseBound {
    pub items: Vec<(Token, Option<Token>)>,

    pub t_use: Token,
    pub t_square_open: Token,
    pub t_square_close: Token,
}
impl ToLang for UseBound {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_use.to_rust(p);
        s.t_square_open.to_rust(p);
        for (it, t) in &s.items {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
        }
        s.t_square_close.to_rust(p);
    }
}

#[derive(Debug, Default)]
pub struct TraitBound {
    pub t_unsized: Option<Token>,
    pub for_lifetimes: Option<ForLifetimes>,
    pub type_path: TypePath,
}
impl ToLang for TraitBound {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(t) = &s.t_unsized {
            t.to_rust(p);
        }
        if let Some(for_) = &s.for_lifetimes {
            for_.to_rust(p);
        }
        s.type_path.to_rust(p);
    }
}

#[derive(Debug, Default)]
pub struct ForLifetimes {
    pub for_: Generics,

    pub t_for: Token,
}
impl ToLang for ForLifetimes {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_for.to_rust(p);
        s.for_.to_rust(p);
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

        if s.items.is_empty() {
            return;
        }

        // -..crate-abc..Abc turns into ::crate_abc::Abc
        let first = s.items.first().unwrap();
        if let TokenCode::Identifier(a) = &first.token.code && a.id == "-" {
            if s.items.len() == 1 {
                first.to_rust(p);
            } else {
                if p.no_first_before {
                    p.no_first_before = false;
                } else {
                    first.token.to_rs_before(p);
                }
            }
        } else {
            first.to_rust(p);
        }

        for it in &s.items[1..] {
            it.to_rust(p);
        }
    }
}
#[derive(Debug)]
pub struct SimplePathSegment {
    pub t_sep: Option<Token>,
    pub token: Token,
}
impl ToLang for SimplePathSegment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(t) = &s.t_sep {
            if p.no_first_before {
                p.no_first_before = false;
                t.to_rs_one(p);
            } else {
                t.to_rust(p);
            }
        }
        if p.no_first_before {
            p.no_first_before = false;
            s.token.to_rs_one(p);
        } else {
            s.token.to_rust(p);
        }
    }
}

impl TryFrom<PathExprSegment> for SimplePathSegment {
    type Error = &'static str;

    fn try_from(p: PathExprSegment) -> Result<Self, Self::Error> {
        if matches!(p.name.token.code, T![Self]) {
            return Err("illegal simple path segment");
        }
        Ok(SimplePathSegment{
            t_sep: p.t_sep,
            token: p.name.token,
        })
    }
}
impl TryFrom<TypePathSegment> for SimplePathSegment {
    type Error = &'static str;

    fn try_from(p: TypePathSegment) -> Result<Self, Self::Error> {
        if matches!(p.name.token.code, T![Self]) {
            return Err("illegal simple path segment");
        }
        Ok(SimplePathSegment{
            t_sep: p.t_sep,
            token: p.name.token,
        })
    }
}

#[derive(Debug, Default)]
pub struct PathIdentSegment {
    pub token: Token,
}
impl ToLang for PathIdentSegment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.token.to_rs_one(p);
        } else {
            s.token.to_rust(p);
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

        if s.items.is_empty() {
            return;
        }

        // -..crate-abc..Abc turns into ::crate_abc::Abc
        let first = s.items.first().unwrap();
        if let TokenCode::Identifier(a) = &first.name.token.code && a.id == "-" {
            if s.items.len() == 1 {
                first.to_rust(p);
            } else {
                if p.no_first_before {
                    p.no_first_before = false;
                } else {
                    first.name.token.to_rs_before(p);
                }
            }
        } else {
            first.to_rust(p);
        }

        for it in &s.items[1..] {
            it.to_rust(p);
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
    pub t_sep: Option<Token>,
    pub name: PathIdentSegment,
    pub generic_args: GenericArgs,
}
impl ToLang for PathExprSegment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(t) = &s.t_sep {
            if p.no_first_before {
                p.no_first_before = false;
                t.to_rs_one(p);
            } else {
                t.to_rust(p);
            }
        }
        s.name.to_rust(p);
        if !s.generic_args.items.is_empty() {
            s.generic_args.to_rust(p);
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

        s.type_.to_rust(p);
        for it in &s.items {
            p.push_raw("::");
            it.to_rust(p);
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

        s.type_.to_rust(p);
        for it in &s.items {
            it.to_rust(p);
        }
    }
}
#[derive(Debug, Default)]
pub struct QualifiedPathType {
    pub type_: Type,
    pub type_path: Option<(Token, TypePath)>,

    pub t_square_open: Token,
    pub t_square_close: Token,
}
impl ToLang for QualifiedPathType {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_square_open.to_rs_one(p);
        } else {
            s.t_square_open.to_rust(p);
        }
        s.type_.to_rust(p);
        if let Some((t, type_path)) = &s.type_path {
            t.to_rust(p);
            type_path.to_rust(p);
        }
        s.t_square_close.to_rust(p);
    }
}

#[derive(Debug, Default)]
pub struct TypePath {
    pub items: Vec<TypePathSegment>,
}
impl ToLang for TypePath {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.items.is_empty() {
            return;
        }

        // -..crate-abc..Abc turns into ::crate_abc::Abc
        let first = s.items.first().unwrap();
        if let TokenCode::Identifier(a) = &first.name.token.code && a.id == "-" {
            if s.items.len() == 1 {
                first.to_rust(p);
            } else {
                first.name.token.to_rs_before(p);
            }
        } else {
            first.to_rust(p);
        }

        for it in &s.items[1..] {
            it.to_rust(p);
        }
    }
}
#[derive(Debug, Default)]
pub struct TypePathSegment {
    pub t_sep: Option<Token>,
    pub name: PathIdentSegment,
    pub payload: Option<TypePathSegmentPayload>,
}
impl ToLang for TypePathSegment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(t) = &s.t_sep {
            t.to_rust(p);
        }
        s.name.to_rust(p);
        if let Some(a) = &s.payload {
            match a {
                TypePathSegmentPayload::GenericArgs(a) => a.to_rust(p),
                TypePathSegmentPayload::TypePathFunc(a) => a.to_rust(p),
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
    pub inputs: Vec<(Type, Option<Token>)>,
    pub return_type: Option<Type>,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
impl ToLang for TypePathFunc {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.t_paren_open.to_rust(p);
        for (it, t) in &s.inputs {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
        }
        s.t_paren_close.to_rust(p);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" ->");
            return_type.to_rust(p);
        }
    }
}

#[derive(Debug, Default)]
pub struct GenericArgs {
    pub items: Vec<(GenericArg, Option<Token>)>,
    pub need_colons: bool,

    pub t_square_open: Token,
    pub t_square_close: Token,
}
impl ToLang for GenericArgs {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.need_colons {
            p.push_raw("::");
        }
        s.t_square_open.to_rust(p);
        for (it, t) in &s.items {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
        }
        s.t_square_close.to_rust(p);
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
            GenericArg::Lifetime(a) => a.to_rust(p),
            GenericArg::Type(a) => a.to_rust(p),
            GenericArg::Const(a) => a.to_rust(p),
            GenericArg::Binding(a) => {
                a.path_seg.to_rust(p);
                a.t_eq.to_rust(p);
                a.type_.to_rust(p);
            }
            GenericArg::Bounds(a) => {
                a.path_seg.to_rust(p);
                if !a.bounds.items.is_empty() {
                    p.push_raw(":");
                    a.bounds.to_rust(p);
                }
            }
        }
    }
}
#[derive(Debug)]
pub enum GenericArgsConst {
    BlockExpr(BlockExpr),
    LiteralExpr(ExprLiteral),
    MinusLiteralExpr(Token, ExprLiteral),
}
impl ToLang for GenericArgsConst {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            GenericArgsConst::BlockExpr(a) => a.to_rust(p),
            GenericArgsConst::LiteralExpr(a) => a.to_rust(p),
            GenericArgsConst::MinusLiteralExpr(t, a) => {
                t.to_rust(p);
                a.to_rust(p);
            }
        }
    }
}
#[derive(Debug)]
pub struct GenericArgsBinding {
    pub path_seg: TypePathSegment,
    pub type_: Type,
    pub t_eq: Token,
}
#[derive(Debug)]
pub struct GenericArgsBounds {
    pub path_seg: TypePathSegment,
    pub bounds: TypeParamBounds,
}

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct Pattern {
    pub items: Vec<(PatternNoTopAlt, Option<Token>)>,
}
impl ToLang for Pattern {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for (it, t) in &s.items {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
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
        PatternNoTopAlt::WithoutRange(PatternWithoutRange::Rest(Token::default()))
    }
}
impl ToLang for PatternNoTopAlt {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternNoTopAlt::WithoutRange(a) => a.to_rust(p),
            PatternNoTopAlt::WithRange(a) => a.to_rust(p),
        }
    }
}

#[derive(Debug)]
pub enum PatternWithoutRange {
    Literal(PatternLiteral),
    Identifier(PatternIdentifier),
    // Wildcard,
    Rest(Token),
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
            PatternWithoutRange::Literal(a) => a.to_rust(p),
            PatternWithoutRange::Identifier(a) => {
                if let Some(t) = &a.t_ref {
                    if p.no_first_before {
                        p.no_first_before = false;
                        t.to_rs_one(p);
                    } else {
                        t.to_rust(p);
                    }
                }
                if let Some(t) = &a.t_mut {
                    if p.no_first_before {
                        p.no_first_before = false;
                        t.to_rs_one(p);
                    } else {
                        t.to_rust(p);
                    }
                }
                if p.no_first_before {
                    p.no_first_before = false;
                    a.name.to_rs_one(p);
                } else {
                    a.name.to_rust(p);
                }
                if let Some((t, pat)) = &a.pattern {
                    t.to_rust(p);
                    pat.to_rust(p);
                }
            }
            PatternWithoutRange::Rest(t) => {
                if p.no_first_before {
                    p.no_first_before = false;
                    t.to_rs_one(p);
                } else {
                    t.to_rust(p);
                }
            }
            PatternWithoutRange::Obsolete(a) => {
                a.to_rust(p);
            }
            PatternWithoutRange::Reference(a) => {
                if p.no_first_before {
                    p.no_first_before = false;
                    a.t_ref.to_rs_one(p);
                } else {
                    a.t_ref.to_rust(p);
                }
                if let Some(t) = &a.t_mut {
                    t.to_rust(p);
                }
                a.pattern_without_range.to_rust(p);
            }
            PatternWithoutRange::Struct(a) => {
                a.path.to_rust(p);
                a.t_brace_open.to_rust(p);
                for it in &a.items {
                    it.to_rust(p);
                }
                a.t_brace_close.to_rust(p);
            }
            PatternWithoutRange::TupleStruct(a) => {
                a.path.to_rust(p);
                a.t_paren_open.to_rust(p);
                for (it, t) in &a.items {
                    it.to_rust(p);
                    if let Some(t) = t {
                        t.to_rust(p);
                    }
                }
                a.t_paren_close.to_rust(p);
            }
            PatternWithoutRange::Tuple(a) => {
                if p.no_first_before {
                    p.no_first_before = false;
                    a.t_paren_open.to_rs_one(p);
                } else {
                    a.t_paren_open.to_rust(p);
                }
                for (pat, t) in &a.items {
                    pat.to_rust(p);
                    if let Some(t) = t {
                        t.to_rust(p);
                    }
                }
                a.t_paren_close.to_rust(p);
            }
            PatternWithoutRange::Grouped(a) => {
                if p.no_first_before {
                    p.no_first_before = false;
                    a.t_paren_open.to_rs_one(p);
                } else {
                    a.t_paren_open.to_rust(p);
                }
                a.pattern.to_rust(p);
                a.t_paren_close.to_rust(p);
            }
            PatternWithoutRange::Slice(a) => {
                if p.no_first_before {
                    p.no_first_before = false;
                    a.t_square_open.to_rs_one(p);
                } else {
                    a.t_square_open.to_rust(p);
                }
                for (it, t) in &a.items {
                    it.to_rust(p);
                    if let Some(t) = t {
                        t.to_rust(p);
                    }
                }
                a.t_square_close.to_rust(p);
            }
            PatternWithoutRange::Path(a) => match a {
                PatternPath::Path(b) => b.to_rust(p),
                PatternPath::QualifiedPath(b) => b.to_rust(p),
            },
            PatternWithoutRange::MacroCall(a) => a.to_rust(p),
        }
    }
}
#[derive(Debug)]
pub struct PatternLiteral {
    pub name: Token,

    pub t_minus: Option<Token>,
}
impl ToLang for PatternLiteral {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(t) = &s.t_minus {
            if p.no_first_before {
                p.no_first_before = false;
                t.to_rs_one(p);
            } else {
                t.to_rust(p);
            }
        }
        if p.no_first_before {
            p.no_first_before = false;
            s.name.to_rs_one(p);
        } else {
            s.name.to_rust(p);
        }
    }
}
#[derive(Debug, Default)]
pub struct PatternIdentifier {
    pub name: Token,
    pub pattern: Option<(Token, Box<PatternNoTopAlt>)>,

    pub t_ref: Option<Token>,
    pub t_mut: Option<Token>,
}
#[derive(Debug)]
pub struct PatternReference {
    pub pattern_without_range: Box<PatternWithoutRange>,

    pub t_ref: Token,
    pub t_mut: Option<Token>,
}

#[derive(Debug, Default)]
pub struct PatternStruct {
    pub path: PathInExpr,
    pub items: Vec<PatternStructItem>,

    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
#[derive(Debug)]
pub enum PatternStructItem {
    Field(PatternStructField),
    Etc(Token),
}
impl Default for PatternStructItem {
    fn default() -> Self {
        PatternStructItem::Etc(Token::default())
    }
}
impl ToLang for PatternStructItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            PatternStructItem::Field(f) => {
                f.outer_attrs.to_rust(p);
                match &f.payload {
                    PatternStructFieldPayload::A(a) => {
                        a.t_tuple_index.to_rust(p);
                        a.t_colon.to_rust(p);
                        a.pattern.to_rust(p);
                    }
                    PatternStructFieldPayload::B(b) => {
                        b.name.to_rust(p);
                        b.t_colon.to_rust(p);
                        b.pattern.to_rust(p);
                    }
                    PatternStructFieldPayload::C(c) => {
                        if let Some(t) = &c.t_ref {
                            t.to_rust(p);
                        }
                        if let Some(t) = &c.t_mut {
                            t.to_rust(p);
                        }
                        c.name.to_rust(p);
                    }
                }
                if let Some(t) = &f.t_comma {
                    t.to_rust(p);
                }
            }
            PatternStructItem::Etc(t) => {
                t.to_rust(p);
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct PatternStructField {
    pub outer_attrs: Attrs,
    pub payload: PatternStructFieldPayload,
    pub t_comma: Option<Token>,
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
    pub pattern: Pattern,
    pub t_tuple_index: Token,
    pub t_colon: Token,
}
#[derive(Debug)]
pub struct PatternStructFieldB {
    pub name: Token,
    pub pattern: Pattern,
    pub t_colon: Token,
}
#[derive(Debug, Default)]
pub struct PatternStructFieldC {
    pub name: Token,

    pub t_ref: Option<Token>,
    pub t_mut: Option<Token>,
}

#[derive(Debug, Default)]
pub struct PatternTupleStruct {
    pub path: PathInExpr,
    pub items: Vec<(Pattern, Option<Token>)>,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
#[derive(Debug)]
pub struct PatternTuple {
    pub items: Vec<(Pattern, Option<Token>)>,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
#[derive(Debug)]
pub struct PatternGrouped {
    pub pattern: Box<Pattern>,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
#[derive(Debug)]
pub struct PatternSlice {
    pub items: Vec<(Pattern, Option<Token>)>,

    pub t_square_open: Token,
    pub t_square_close: Token,
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

    pub t_sep: Token,
}
impl ToLang for PatternRange {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(start) = &s.start {
            start.to_rust(p);
        }
        if p.no_first_before {
            p.no_first_before = false;
            s.t_sep.to_rs_one(p);
        } else {
            s.t_sep.to_rust(p);
        }
        if let Some(end) = &s.end {
            end.to_rust(p);
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
            PatternRangeBound::Literal(a) => a.to_rust(p),
            PatternRangeBound::Path(a) => a.to_rust(p),
            PatternRangeBound::QualifiedPath(a) => a.to_rust(p),
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
        s.payload.to_rust(p);
    }
}
#[derive(Debug)]
pub enum TypePayload {
    TypeNoBounds(Box<TypeNoBounds>),
    ImplTrait(TypeImplTrait),
    TraitObject(TypeTraitObject),
}
impl Default for TypePayload {
    fn default() -> Self {
        TypePayload::TraitObject(TypeTraitObject::default())
    }
}
impl ToLang for TypePayload {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            TypePayload::TypeNoBounds(a) => a.to_rust(p),
            TypePayload::ImplTrait(a) => {
                a.t_impl.to_rust(p);
                a.bounds.to_rust(p);
            }
            TypePayload::TraitObject(a) => {
                a.t_dyn.to_rust(p);
                a.bounds.to_rust(p);
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeNoBounds {
    Parenthesized(TypeParenthesized),               // ( Type )
    ImplTraitOneBound(TypeImplTraitOneBound),       // impl TraitBound
    TraitObjectOneBound(TypeTraitObjectOneBound),   // dyn TraitBound
    Tuple(TypeTuple),                               // () | ( (Type ,)+ Type? )
    Optional(TypeOptional),                         // ?Type
    RawPointer(TypeRawPointer),                     // *(mut | const) TypeNoBounds
    Reference(TypeReference),                       // [&|&&] Lifetime? mut? TypeNoBounds
    Array(TypeArray),                               // {{ Type ; Expression }}
    Slice(TypeSlice),                               // {{ Type }}
    TypePath(TypePath),                             // TypePathSegment(..TypePathSegment)*
    QualifiedPathInType(QualifiedPathInType),       // [ Type.(TypePath)? ]..(TypePathSegment)+
    Never(Token),                                   // !
    BareFunction(Box<TypeBareFunction>),            // ForLifetimes? Qualifers func(Param?) ReturnType
    MacroCall(MacroCall),                           // SimplePath,, DelimTokenTree
}
impl Default for TypeNoBounds {
    fn default() -> Self {
        TypeNoBounds::Never(Token::default())
    }
}
impl ToLang for TypeNoBounds {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            TypeNoBounds::Parenthesized(a) => {
                a.t_paren_open.to_rust(p);
                a.type_.to_rust(p);
                a.t_paren_close.to_rust(p);
            }
            TypeNoBounds::ImplTraitOneBound(a) => {
                a.t_impl.to_rust(p);
                a.bound.to_rust(p);
            }
            TypeNoBounds::TraitObjectOneBound(a) => {
                a.t_dyn.to_rust(p);
                a.bound.to_rust(p);
            }
            TypeNoBounds::Tuple(a) => {
                a.t_paren_open.to_rust(p);
                for (it, t) in &a.items {
                    it.to_rust(p);
                    if let Some(t) = t {
                        t.to_rust(p);
                    }
                }
                a.t_paren_close.to_rust(p);
            }
            TypeNoBounds::Optional(a) => {
                a.t_sep.to_rs_before(p);
                p.push_raw("Option<");
                a.type_.to_rust(p);
                p.push_raw(">");
            }
            TypeNoBounds::RawPointer(a) => {
                a.t_pointer.to_rust(p);
                a.t_mut_const.to_rust(p);
                a.type_no_bounds.to_rust(p);
            }
            TypeNoBounds::Reference(a) => {
                a.t_ref.to_rust(p);
                if let Some(a) = &a.lifetime {
                    a.to_rust(p);
                }
                if let Some(t) = &a.t_mut {
                    t.to_rust(p);
                }
                a.type_no_bounds.to_rust(p);
            }
            TypeNoBounds::Array(a) => {
                a.t_double_brace_open.to_rust(p);
                a.type_.to_rust(p);
                a.t_semicolon.to_rust(p);
                a.expr.to_rust(p);
                a.t_double_brace_close.to_rust(p);
            }
            TypeNoBounds::Slice(a) => {
                a.t_double_brace_open.to_rust(p);
                a.type_.to_rust(p);
                a.t_double_brace_close.to_rust(p);
            }
            TypeNoBounds::TypePath(a) => a.to_rust(p),
            TypeNoBounds::QualifiedPathInType(a) => a.to_rust(p),
            TypeNoBounds::Never(t) => t.to_rust(p),
            TypeNoBounds::BareFunction(a) => a.to_rust(p),
            TypeNoBounds::MacroCall(a) => a.to_rust(p),
        }
    }
}

#[derive(Debug, Default)]
pub struct TypeImplTrait {
    pub bounds: TypeParamBounds,

    pub t_impl: Token,
}

#[derive(Debug, Default)]
pub struct TypeTraitObject {
    pub bounds: TypeParamBounds,

    pub t_dyn: Token,
}

#[derive(Debug)]
pub struct TypeParenthesized {
    pub type_: Type,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}

#[derive(Debug)]
pub struct TypeImplTraitOneBound {
    pub bound: TraitBound,

    pub t_impl: Token,
}

#[derive(Debug)]
pub struct TypeTraitObjectOneBound {
    pub bound: TraitBound,

    pub t_dyn: Token,
}

#[derive(Debug)]
pub struct TypeTuple {
    pub items: Vec<(Type, Option<Token>)>,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}

#[derive(Debug)]
pub struct TypeOptional {
    pub type_: Type,

    pub t_sep: Token,
}

#[derive(Debug)]
pub struct TypeRawPointer {
    pub type_no_bounds: Box<TypeNoBounds>,

    pub t_pointer: Token,
    pub t_mut_const: Token,
}

#[derive(Debug, Default)]
pub struct TypeReference {
    pub lifetime: Option<Lifetime>,
    pub type_no_bounds: Box<TypeNoBounds>,

    pub t_ref: Token,
    pub t_mut: Option<Token>,
}

#[derive(Debug)]
pub struct TypeArray {
    pub type_: Type,
    pub expr: Expr,

    pub t_double_brace_open: Token,
    pub t_double_brace_close: Token,
    pub t_semicolon: Token,
}

#[derive(Debug)]
pub struct TypeSlice {
    pub type_: Type,

    pub t_double_brace_open: Token,
    pub t_double_brace_close: Token,
}

#[derive(Debug, Default)]
pub struct TypeBareFunction {
    pub for_lifetimes: Option<ForLifetimes>,
    pub qualifier: FuncTypeQualifier,
    pub params: Vec<MaybeNamedParam>,
    pub return_type: Option<Type>,
    pub variadics: Option<MaybeNamedParamVariadics>,

    pub t_func: Option<Token>,
    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
impl ToLang for TypeBareFunction {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(for_) = &s.for_lifetimes {
            for_.to_rust(p);
        }

        if let Some(t) = &s.qualifier.t_unsafe {
            t.to_rust(p);
        }
        if let Some((extern_, abi)) = &s.qualifier.t_extern {
            extern_.to_rust(p);
            if let Some(abi) = abi {
                abi.to_rust(p);
            }
        }

        if let Some(t) = &s.t_func {
            t.to_rust(p);
        }

        s.t_paren_open.to_rust(p);
        for it in &s.params {
            it.outer_attrs.to_rust(p);
            if let Some(name) = &it.name {
                name.to_rust(p);
                p.push_raw(": ");
            }
            it.type_.to_rust(p);
            if let Some(t) = &it.t_comma {
                t.to_rust(p);
            }
        }
        if let Some(v) = &s.variadics {
            v.outer_attrs.to_rust(p);
            v.t_var.to_rust(p);
        }
        s.t_paren_close.to_rust(p);

        if let Some(return_type) = &s.return_type {
            p.push_raw(" ->");
            return_type.to_rust(p);
        }
    }
}
#[derive(Debug, Default)]
pub struct FuncTypeQualifier {
    pub t_unsafe: Option<Token>,
    pub t_extern: Option<(Token, Option<Token>)>,
}
#[derive(Debug, Default)]
pub struct MaybeNamedParam {
    pub outer_attrs: Attrs,
    pub name: Option<Token>,
    pub type_: Type,
    pub t_comma: Option<Token>,
}
#[derive(Debug)]
pub struct MaybeNamedParamVariadics {
    pub outer_attrs: Attrs,

    pub t_var: Token,
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
            Stmt::Item(a) => a.to_rust(p),
            Stmt::Let(a) => a.to_rust(p),
            Stmt::Expr(a) => {
                a.to_rust(p);
                if matches!(a.payload, ExprPayload::WithoutBlock(_)) {
                    p.push_raw(";");
                }
            }
            Stmt::Value(a) => a.to_rust(p),
        }
    }
}

#[derive(Debug, Default)]
pub struct StmtLet {
    pub pattern: Pattern,
    pub short: Option<(Token, Option<Token>)>,

    pub outer_attrs: Attrs,
    pub type_: Option<Type>,
    pub expr: Option<Expr>,
    pub else_: Option<(Token, BlockExpr)>,

    pub t_eq: Token,
    pub t_first_in_pattern: Option<Token>,
    pub t_first_in_expr: Option<Token>,
}
impl ToLang for StmtLet {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.outer_attrs.to_rust(p);

        if let Some((name, _)) = &s.short {
            name.to_rs_before(p);
        } else {
            if let Some(t) = &s.t_first_in_expr {
                t.to_rs_before(p);
            }
        }

        // NOTE not considering the case when "let" is followed by new line
        p.push_raw("let ");
        if let Some((name, t_mut)) = &s.short {
            if let Some(t) = t_mut {
                t.to_rs_one(p);
                t.to_rs_before(p);
            }
            name.to_rs_one(p);
        } else {
            p.no_first_before = true;
            s.pattern.to_rust(p);
            p.no_first_before = false;
        }

        if let Some(type_) = &s.type_ {
            p.push_raw(":");
            type_.to_rust(p);
        }
        if let Some(expr) = &s.expr {
            s.t_eq.to_rust(p);

            if let Some(t) = &s.t_first_in_pattern {
                t.to_rs_before(p);
            }

            if s.short.is_some() {
                expr.to_rust(p);
            } else {
                p.no_first_before = true;
                expr.to_rust(p);
                p.no_first_before = false;
            }

            if let Some((t, else_)) = &s.else_ {
                t.to_rust(p);
                else_.to_rust(p);
            }
        }
        p.push_raw(";");
    }
}

#[derive(Debug)]
pub struct StmtValue {
    pub outer_attrs: Attrs,
    pub expr: Expr,
    pub t_sep: Token,
}
impl ToLang for StmtValue {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.outer_attrs.to_rust(p);
        s.t_sep.to_rs_before(p);

        p.no_first_before = true;
        s.expr.to_rust(p);
        p.no_first_before = false;
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

        s.outer_attrs.to_rust(p);
        match &s.payload {
            ExprPayload::WithBlock(a) => a.to_rust(p),
            ExprPayload::WithoutBlock(a) => a.to_rust(p),
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
            Token::default(),
        ))))
    }
}

#[derive(Debug, Default)]
pub struct BlockExpr {
    pub inner_attrs: Attrs,
    pub stmts: Vec<Stmt>,

    pub t_brace_open: Token,
    pub t_brace_close: Token,
    pub t_first_after_brace: Token,
}
impl ToLang for BlockExpr {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_brace_open.to_rs_one(p);
        } else {
            s.t_brace_open.to_rust(p);
        }
        s.inner_attrs.to_rust(p);
        for it in &s.stmts {
            it.to_rust(p);
        }
        s.t_brace_close.to_rust(p);
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
            ExprWithBlock::BlockExpr(a) => a.to_rust(p),
            ExprWithBlock::AsyncBlock(a) => a.to_rust(p),
            ExprWithBlock::UnsafeBlock(a) => a.to_rust(p),
            ExprWithBlock::ConstBlock(a) => a.to_rust(p),
            ExprWithBlock::Loop(a) => a.to_rust(p),
            ExprWithBlock::If(a) => a.to_rust(p),
            ExprWithBlock::Match(a) => a.to_rust(p),
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprAsyncBlock {
    pub block: BlockExpr,

    pub t_async: Token,
    pub t_move: Option<Token>,
}
impl ToLang for ExprAsyncBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_async.to_rs_one(p);
        } else {
            s.t_async.to_rust(p);
        }
        if let Some(t) = &s.t_move {
            p.push_raw(" ");
            t.to_rust(p);
        }
        s.block.to_rust(p);
    }
}
#[derive(Debug)]
pub struct ExprUnsafeBlock {
    pub block: BlockExpr,

    pub t_unsafe: Token,
}
impl ToLang for ExprUnsafeBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_unsafe.to_rs_one(p);
        } else {
            s.t_unsafe.to_rust(p);
        }
        s.block.to_rust(p);
    }
}
#[derive(Debug)]
pub struct ExprConstBlock {
    pub block: BlockExpr,

    pub t_const: Token,
}
impl ToLang for ExprConstBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_const.to_rs_one(p);
        } else {
            s.t_const.to_rust(p);
        }
        s.block.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct ExprLoop {
    pub label: Option<(Token, Token)>,
    pub body: BlockExpr,
    pub payload: ExprLoopPayload,

    pub t_for: Token,
}
impl ToLang for ExprLoop {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some((t_label, t_colon)) = &s.label {
            if p.no_first_before {
                p.no_first_before = false;
                t_label.to_rs_one(p);
            } else {
                t_label.to_rust(p);
            }
            t_colon.to_rust(p);
        }
        if matches!(&s.payload, ExprLoopPayload::Block) {
            s.body.to_rust(p);
            return;
        }

        if p.no_first_before {
            p.no_first_before = false;
            s.t_for.to_rs_one(p);
        } else {
            s.t_for.to_rust(p);
        }
        match &s.payload {
            ExprLoopPayload::Loop => {
            }
            ExprLoopPayload::While(a) => {
                a.conditions.to_rust(p);
            }
            ExprLoopPayload::In(a) => {
                a.pattern.to_rust(p);
                a.t_in.to_rust(p);
                a.in_expr.to_rust(p);
            }
            ExprLoopPayload::Block => {}
        }
        s.body.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub enum ExprLoopPayload {
    #[default]
    Loop,
    While(ExprLoopWhile),
    In(ExprLoopIn),
    Block,
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

    pub t_in: Token,
}

#[derive(Debug, Default)]
pub struct Conditions {
    pub items: Vec<(ConditionItem, Option<Token>)>,
}
impl ToLang for Conditions {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        for (it, t) in &s.items {
            it.to_rust(p);
            if let Some(t) = t {
                // for cases such as "if a -> b, c { }"
                let has_blank = t.tokens_before
                    .iter()
                    .any(|it| it.code == TokenCode::Blank);
                if !has_blank {
                    p.push_raw(" ");
                }
                t.to_rust(p);
            }
        }
    }
}

#[derive(Debug)]
pub enum ConditionItem {
    Expr(Expr),
    LetChain(LetChainCondition),
}
impl Default for ConditionItem {
    fn default() -> Self {
        ConditionItem::Expr(Expr::default())
    }
}
impl ToLang for ConditionItem {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match &s {
            ConditionItem::Expr(expr) => {
                expr.to_rust(p);
            }
            ConditionItem::LetChain(a) => {
                a.outer_attrs.to_rust(p);

                a.t_first_in_expr.to_rs_before(p);
                p.push_raw("let ");

                p.no_first_before = true;
                a.pattern.to_rust(p);
                p.no_first_before = false;

                a.t_eq.to_rust(p);

                a.t_first_in_pattern.to_rs_before(p);
                p.no_first_before = true;
                a.scrutinee.to_rust(p);
                p.no_first_before = false;
            }
        }
    }
}
#[derive(Debug)]
pub struct LetChainCondition {
    pub outer_attrs: Attrs,
    pub pattern: Pattern,
    pub scrutinee: Expr,
    pub t_eq: Token,
    pub t_first_in_expr: Token,
    pub t_first_in_pattern: Token,
}

#[derive(Debug, Default)]
pub struct ExprIf {
    pub conditions: Conditions,
    pub body: BlockExpr,
    pub else_: Option<(Token, ExprIfElse)>,

    pub t_if: Token,
}
impl ToLang for ExprIf {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_if.to_rs_one(p);
        } else {
            s.t_if.to_rust(p);
        }
        s.conditions.to_rust(p);
        s.body.to_rust(p);

        if let Some(else_) = &s.else_ {
            else_.0.to_rust(p);
            match &else_.1 {
                ExprIfElse::BlockExpr(a) => a.to_rust(p),
                ExprIfElse::If(a) => a.to_rust(p),
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

    pub t_match: Token,
    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
impl ToLang for ExprMatch {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_match.to_rs_one(p);
        } else {
            s.t_match.to_rust(p);
        }
        s.expr.to_rust(p);

        s.t_brace_open.to_rust(p);
        s.inner_attrs.to_rust(p);
        for it in &s.arms {
            it.arm.to_rust(p);

            let has_blank = it.t_sep.tokens_before
                .iter()
                .any(|it| it.code == TokenCode::Blank);
            if !has_blank {
                p.push_raw(" ");
            }
            it.t_sep.to_rust(p);

            it.expr.to_rust(p);
            if matches!(&it.expr.payload, ExprPayload::WithoutBlock(_)) {
                p.push_raw(",");
            }
        }
        s.t_brace_close.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct ExprMatchItem {
    pub arm: ExprMatchArm,
    pub expr: Expr,
    pub t_sep: Token,
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

        s.outer_attrs.to_rust(p);
        s.pattern.to_rust(p);
        if let Some(a) = &s.guard {
            a.t_if.to_rust(p);
            a.conditions.to_rust(p);
        }
    }
}
#[derive(Debug)]
pub struct ExprMatchArmGuard {
    pub conditions: Conditions,

    pub t_if: Token,
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
    Debug(ExprDebug),
    MacroCall(MacroCall),
}
impl Default for ExprWithoutBlock {
    fn default() -> Self {
        ExprWithoutBlock::Literal(ExprLiteral(Token::default()))
    }
}
impl ToLang for ExprWithoutBlock {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match s {
            ExprWithoutBlock::Literal(a) => a.to_rust(p),
            ExprWithoutBlock::Path(a) => a.to_rust(p),
            ExprWithoutBlock::Operator(a) => a.to_rust(p),
            ExprWithoutBlock::Grouped(a) => a.to_rust(p),
            ExprWithoutBlock::Array(a) => a.to_rust(p),
            ExprWithoutBlock::Await(a) => a.to_rust(p),
            ExprWithoutBlock::Index(a) => a.to_rust(p),
            ExprWithoutBlock::Tuple(a) => a.to_rust(p),
            ExprWithoutBlock::TupleIndexing(a) => a.to_rust(p),
            ExprWithoutBlock::Struct(a) => a.to_rust(p),
            ExprWithoutBlock::Call(a) => a.to_rust(p),
            ExprWithoutBlock::MethodCall(a) => a.to_rust(p),
            ExprWithoutBlock::Field(a) => a.to_rust(p),
            ExprWithoutBlock::Closure(a) => a.to_rust(p),
            ExprWithoutBlock::Continue(a) => a.to_rust(p),
            ExprWithoutBlock::Break(a) => a.to_rust(p),
            ExprWithoutBlock::Range(a) => a.to_rust(p),
            ExprWithoutBlock::Return(a) => a.to_rust(p),
            ExprWithoutBlock::Debug(a) => a.to_rust(p),
            ExprWithoutBlock::MacroCall(a) => a.to_rust(p),
        }
    }
}

#[derive(Debug)]
pub struct ExprLiteral(pub Token);
impl ToLang for ExprLiteral {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.0.to_rs_one(p);
        } else {
            s.0.to_rust(p);
        }
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
            ExprPath::Path(a) => a.to_rust(p),
            ExprPath::QualifiedPath(a) => a.to_rust(p),
        }
    }
}

#[derive(Debug)]
pub enum ExprOperator {
    Borrow(ExprBorrow),
    Dereference(ExprDereference),
    ErrorPropagation(ExprErrorPropagation),
    IncDec(ExprIncDec),
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
                if p.no_first_before {
                    p.no_first_before = false;
                    a.t_ref.to_rs_one(p);
                } else {
                    a.t_ref.to_rust(p);
                }
                if let Some(t) = &a.t_raw {
                    t.to_rust(p);
                }
                if let Some(t) = &a.t_mut {
                    t.to_rust(p);
                }
                a.expr.to_rust(p);
            }
            ExprOperator::Dereference(a) => {
                if p.no_first_before {
                    p.no_first_before = false;
                    a.t_sep.to_rs_one(p);
                } else {
                    a.t_sep.to_rust(p);
                }
                a.expr.to_rust(p);
            }
            ExprOperator::ErrorPropagation(a) => {
                a.expr.to_rust(p);
                a.t_sep.to_rust(p);
            }
            ExprOperator::IncDec(a) => {
                a.expr.to_rust(p);
                if a.inc {
                    p.push_raw(" +");
                } else {
                    p.push_raw(" -");
                }
                p.push_raw("= 1");
            }
            ExprOperator::Negation(a) => {
                if p.no_first_before {
                    p.no_first_before = false;
                    a.t_sep.to_rs_one(p);
                } else {
                    a.t_sep.to_rust(p);
                }
                a.expr.to_rust(p);
            }
            ExprOperator::ArithmeticOrLogical(a) => {
                a.left.to_rust(p);
                a.op.to_rust(p);
                a.right.to_rust(p);
            }
            ExprOperator::Comparison(a) => {
                a.left.to_rust(p);
                a.op.to_rust(p);
                a.right.to_rust(p);
            }
            ExprOperator::LazyBoolean(a) => {
                a.left.to_rust(p);
                a.op.to_rust(p);
                a.right.to_rust(p);
            }
            ExprOperator::TypeCast(a) => {
                a.expr.to_rust(p);
                a.t_as.to_rust(p);
                a.type_no_bounds.to_rust(p);
            }
            ExprOperator::Assignment(a) => {
                a.left.to_rust(p);
                a.op.to_rust(p);
                a.right.to_rust(p);
            }
            ExprOperator::CompoundAssignment(a) => {
                a.left.to_rust(p);
                a.op.to_rust(p);
                a.right.to_rust(p);
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct ExprBorrow {
    pub expr: Expr,

    pub t_ref: Token,
    pub t_raw: Option<Token>,
    pub t_mut: Option<Token>,
}
#[derive(Debug)]
pub struct ExprDereference {
    pub expr: Expr,

    pub t_sep: Token,
}
#[derive(Debug)]
pub struct ExprErrorPropagation {
    pub expr: Expr,

    pub t_sep: Token,
}
#[derive(Debug)]
pub struct ExprIncDec {
    pub expr: Expr,

    pub inc: bool,
}
#[derive(Debug)]
pub struct ExprNegation {
    pub expr: Expr,

    pub t_sep: Token,
}
#[derive(Debug)]
pub struct ExprArithmeticOrLogical {
    pub left: Expr,
    pub op: Token, // must be TokenCode::Op(+ ~ * / % & | ^ << >>)
    pub right: Expr,
}
#[derive(Debug)]
pub struct ExprComparison {
    pub left: Expr,
    pub op: Token, // must be TokenCode::Op(== != > < >= <=)
    pub right: Expr,
}
#[derive(Debug)]
pub struct ExprLazyBoolean {
    pub left: Expr,
    pub op: Token, // must be TokenCode::Op(&& ||)
    pub right: Expr,
}
#[derive(Debug)]
pub struct ExprTypeCast {
    pub expr: Expr,
    pub type_no_bounds: TypeNoBounds,

    pub t_as: Token,
}
#[derive(Debug)]
pub struct ExprAssignment {
    pub left: Expr,
    pub op: Token, // must be TokenCode::Op(=)
    pub right: Expr,
}
#[derive(Debug)]
pub struct ExprCompoundAssignment {
    pub left: Expr,
    pub op: Token, // must be TokenCode::Op(+= ~= *= /= %= &= |= ^= <<= >>=)
    pub right: Expr,
}

#[derive(Debug)]
pub struct ExprGrouped {
    pub expr: Expr,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
impl ToLang for ExprGrouped {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_paren_open.to_rs_one(p);
        } else {
            s.t_paren_open.to_rust(p);
        }
        s.expr.to_rust(p);
        s.t_paren_close.to_rust(p);
    }
}
#[derive(Debug)]
pub struct ExprArray {
    pub payload: ExprArrayPayload,

    pub t_double_brace_open: Token,
    pub t_double_brace_close: Token,
}
impl ToLang for ExprArray {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_double_brace_open.to_rs_one(p);
        } else {
            s.t_double_brace_open.to_rust(p);
        }
        match &s.payload {
            ExprArrayPayload::One(a) => {
                a.value.to_rust(p);
                a.t_semicolon.to_rust(p);
                a.len.to_rust(p);
            }
            ExprArrayPayload::Many(a) => {
                for (it, t) in &a.0 {
                    it.to_rust(p);
                    if let Some(t) = t {
                        t.to_rust(p);
                    }
                }
            }
        }
        s.t_double_brace_close.to_rust(p);
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

    pub t_semicolon: Token,
}
#[derive(Debug)]
pub struct ExprArrayMany(pub Vec<(Expr, Option<Token>)>);

#[derive(Debug)]
pub struct ExprAwait {
    pub expr: Expr,

    pub t_dot: Token,
    pub t_await: Token,
}
impl ToLang for ExprAwait {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.expr.to_rust(p);
        s.t_dot.to_rust(p);
        s.t_await.to_rust(p);
    }
}

#[derive(Debug)]
pub struct ExprIndex {
    pub left: Expr,
    pub right: Expr,

    pub t_double_brace_open: Token,
    pub t_double_brace_close: Token,
}
impl ToLang for ExprIndex {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.left.to_rust(p);
        s.t_double_brace_open.to_rust(p);
        s.right.to_rust(p);
        s.t_double_brace_close.to_rust(p);
    }
}
#[derive(Debug)]
pub struct ExprTuple {
    pub items: Vec<(Expr, Option<Token>)>,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
impl ToLang for ExprTuple {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_paren_open.to_rs_one(p);
        } else {
            s.t_paren_open.to_rust(p);
        }
        for (it, t) in &s.items {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
        }
        s.t_paren_close.to_rust(p);
    }
}
#[derive(Debug)]
pub struct ExprTupleIndexing {
    pub left: Expr,
    pub index: Token,

    pub t_dot: Token,
}
impl ToLang for ExprTupleIndexing {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.left.to_rust(p);
        s.t_dot.to_rust(p);
        s.index.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct ExprStruct {
    pub path: PathInExpr,
    pub fields: Vec<(ExprStructField, Option<Token>)>,
    pub base: Option<(Token, Option<Expr>)>,

    pub t_brace_open: Token,
    pub t_brace_close: Token,
}
impl ToLang for ExprStruct {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.path.to_rust(p);
        s.t_brace_open.to_rust(p);
        for (it, t) in &s.fields {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
        }
        if let Some((t, expr)) = &s.base {
            t.to_rust(p);
            if let Some(expr) = expr {
                expr.to_rust(p);
            } else {
                p.push_raw("Default::default()")
            }
        }
        s.t_brace_close.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct ExprStructField {
    pub outer_attrs: Attrs,
    pub name: Token,
    pub expr: Option<(Token, Expr)>,
}
impl ToLang for ExprStructField {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.outer_attrs.to_rust(p);
        s.name.to_rust(p);
        if let Some((t, expr)) = &s.expr {
            t.to_rust(p);
            expr.to_rust(p);
        }
    }
}

#[derive(Debug)]
pub struct ExprCall {
    pub left: Expr,
    pub params: Vec<(Expr, Option<Token>)>,

    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
impl ToLang for ExprCall {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.left.to_rust(p);
        s.t_paren_open.to_rust(p);
        for (it, t) in &s.params {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
        }
        s.t_paren_close.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct ExprMethodCall {
    pub left: Expr,
    pub path_expr_segment: PathExprSegment,
    pub params: Vec<(Expr, Option<Token>)>,

    pub t_dot: Token,
    pub t_paren_open: Token,
    pub t_paren_close: Token,
}
impl ToLang for ExprMethodCall {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.left.to_rust(p);
        s.t_dot.to_rust(p);
        s.path_expr_segment.to_rust(p);
        s.t_paren_open.to_rust(p);
        for (it, t) in &s.params {
            it.to_rust(p);
            if let Some(t) = t {
                t.to_rust(p);
            }
        }
        s.t_paren_close.to_rust(p);
    }
}
#[derive(Debug)]
pub struct ExprField {
    pub left: Expr,
    pub name: Token,

    pub t_dot: Token,
}
impl ToLang for ExprField {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        s.left.to_rust(p);
        s.t_dot.to_rust(p);
        s.name.to_rust(p);
    }
}

#[derive(Debug, Default)]
pub struct ExprClosure {
    pub params: Vec<(FuncParamOptionalType, Option<Token>)>,
    pub return_type: Option<TypeNoBounds>,
    pub expr: Expr,

    pub t_async: Option<Token>,
    pub t_move: Option<Token>,
    pub t_delim_open: Token,
    pub t_delim_close: Token,
}
impl ToLang for ExprClosure {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
        } else {
            s.t_delim_open.to_rs_before(p);
        }

        let mut is_first = true;
        // qualifier
        if let Some(t) = &s.t_async {
            if is_first {
                t.to_rs_one(p);
                is_first = false;
            } else {
                t.to_rust(p);
            }
        }
        if let Some(t) = &s.t_move {
            if is_first {
                t.to_rs_one(p);
                is_first = false;
            } else {
                t.to_rust(p);
            }
        }

        if !is_first {
            p.push_raw(" ");
        }
        s.t_delim_open.to_rs_one(p);
        for (i, (it, t)) in s.params.iter().enumerate() {
            it.outer_attrs.to_rust(p);
            if let Some(pat) = &it.pat {
                pat.to_rust(p);
            } else {
                if i == 0 {
                    if s.t_async.is_some() || s.t_move.is_some() {
                        // NOTE don't consider the case when the first param is on another line
                        // let need = it.name.tokens_before.iter()
                        //     .any(|it| {
                        //         it.code == TokenCode::Blank && it.span.line != it.span.end_line
                        //     });
                        // if need {
                        //     it.name.to_rust(p);
                        // } else {
                        //     it.name.to_rs_one(p);
                        // }
                        it.name.to_rs_one(p);
                    } else {
                        it.name.to_rust(p);
                    }
                } else {
                    it.name.to_rust(p);
                }
            }
            if let Some((t, type_)) = &it.type_ {
                if let Some(t) = t {
                    t.to_rust(p);
                } else {
                    p.push_raw(":");
                }
                type_.to_rust(p);
            }
            if let Some(t) = t {
                t.to_rust(p);
            }
        }
        s.t_delim_close.to_rust(p);

        if let Some(a) = &s.return_type {
            p.push_raw(" ->");
            a.to_rust(p);
        }
        s.expr.to_rust(p);
    }
}
#[derive(Debug, Default)]
pub struct FuncParamOptionalType {
    pub outer_attrs: Attrs,
    pub name: Token,
    pub pat: Option<PatternNoTopAlt>,
    pub type_: Option<(Option<Token>, Type)>,
}

#[derive(Debug, Default)]
pub struct ExprContinue {
    pub label: Option<Token>,

    pub t_continue: Token,
}
impl ToLang for ExprContinue {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_continue.to_rs_one(p);
        } else {
            s.t_continue.to_rust(p);
        }
        if let Some(a) = &s.label {
            a.to_rust(p);
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprBreak {
    pub label: Option<Token>,
    pub expr: Option<Expr>,

    pub t_break: Token,
}
impl ToLang for ExprBreak {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_break.to_rs_one(p);
        } else {
            s.t_break.to_rust(p);
        }
        if let Some(a) = &s.label {
            a.to_rust(p);
        }
        if let Some(expr) = &s.expr {
            expr.to_rust(p);
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprReturn {
    pub expr: Option<Expr>,

    pub t_return: Token,
}
impl ToLang for ExprReturn {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
            s.t_return.to_rs_one(p);
        } else {
            s.t_return.to_rust(p);
        }
        if let Some(expr) = &s.expr {
            expr.to_rust(p);
        }
    }
}
#[derive(Debug, Default)]
pub struct ExprDebug {
    pub expr: Expr,

    pub t_sep: Token,
}
impl ToLang for ExprDebug {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if p.no_first_before {
            p.no_first_before = false;
        } else {
            s.t_sep.to_rs_before(p);
        }
        p.push_raw(r#"eprintln!("{:?}", "#);
        s.expr.to_rust(p);
        p.push_raw(")");
    }
}
#[derive(Debug, Default)]
pub struct ExprRange {
    pub start: Option<Expr>,
    pub end: Option<Expr>,

    pub t_sep: Token,
}
impl ToLang for ExprRange {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if let Some(t) = &s.start {
            t.to_rust(p);
        }
        if p.no_first_before {
            p.no_first_before = false;
            s.t_sep.to_rs_one(p);
        } else {
            s.t_sep.to_rust(p);
        }
        if let Some(t) = &s.end {
            t.to_rust(p);
        }
    }
}
