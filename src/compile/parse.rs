use super::*;
use crate::T;
use serde::Serialize;
use std::cell::Cell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error;
use std::fmt;
use std::fs;
use std::io;
use std::mem;

pub fn compile_string(data: String) -> io::Result<Parse> {
    let mut l = Lex::new();
    if l.lex(data).is_err() {
        return Err(io::Error::new(io::ErrorKind::Other, "lex error"));
    }

    let mut p = Parse::new(l);
    if p.parse().is_err() {
        return Err(io::Error::new(io::ErrorKind::Other, "parse error"));
    }

    Ok(p)
}

pub fn compile_file(filepath: &str) -> io::Result<Parse> {
    let data = fs::read_to_string(&filepath)?;
    compile_string(data)
}

// --------------------------------------------------------------------------------
pub struct Parse {
    pkg: Package,

    tokens: Vec<Token>,
    end_tokens: Vec<Token>,
    index: Cell<usize>,

    stop_on_brace_open: Cell<bool>,
    op_map: HashMap<TokenCode, u8>,

    magic: bool, // for auto completion
}

impl Parse {
    pub fn new(lex: Lex) -> Parse {
        return Parse {
            pkg: Package::default(),

            tokens: lex.tokens,
            end_tokens: lex.end_tokens,
            index: Cell::new(0),

            stop_on_brace_open: Cell::new(false),
            op_map: build_operator_precedence_map(),

            magic: false,
        };

        // see https://doc.rust-lang.org/reference/expressions.html
        fn build_operator_precedence_map() -> HashMap<TokenCode, u8> {
            let mut m: HashMap<TokenCode, u8> = HashMap::new();
            let mut p = 100;

            p -= 1;
            m.insert(T![*], p);
            m.insert(T![/], p);
            m.insert(T![%], p);

            p -= 1;
            m.insert(T![+], p);
            m.insert(T![~], p);

            p -= 1;
            m.insert(T![<<], p);
            m.insert(T![>>], p);

            p -= 1;
            m.insert(T![&], p);

            p -= 1;
            m.insert(T![^], p);

            p -= 1;
            m.insert(T![|], p);

            p -= 1;
            m.insert(T![==], p);
            m.insert(T![!=], p);
            m.insert(T![<], p);
            m.insert(T![>], p);
            m.insert(T![<=], p);
            m.insert(T![>=], p);

            p -= 1;
            m.insert(T![&&], p);

            p -= 1;
            m.insert(T![||], p);

            p -= 1;
            m.insert(T!["``"], p);
            m.insert(T!["``="], p);

            m
        }
    }

    #[allow(dead_code)]
    pub fn set_magic(&mut self) {
        self.magic = true;
    }

    pub fn get_pkg(self) -> Package {
        self.pkg
    }
    pub fn get_pkg_ref(&self) -> &Package {
        &self.pkg
    }

    pub fn get_lang_formatter(&self) -> LangFormatter<'_> {
        let s = self;
        LangFormatter::new(&s.tokens, &s.end_tokens)
    }

    pub fn to_rust(self) -> Result<ToRust, ParseError> {
        let s = self;
        let mut p = LangFormatter::new(&s.tokens, &s.end_tokens);
        s.pkg.to_rust(&mut p);
        Ok(p.to_rust())
    }

    pub fn to_u(self) -> String {
        let s = self;
        let mut p = LangFormatter::new(&s.tokens, &s.end_tokens);
        s.pkg.to_u(&mut p);
        p.buf()
    }

    #[allow(dead_code)]
    pub fn print_to_end(&self) {
        let s = self;
        println!("{:?}", &s.tokens[s.index.get()..]);
    }

    fn plusplus(&self) {
        let s = self;
        s.index.set(s.index.get() + 1);
    }
    fn minusminus(&self) {
        let s = self;
        s.index.set(s.index.get() - 1);
    }
    fn inc(&self, n: usize) {
        let s = self;
        s.index.set(s.index.get() + n);
    }
    fn dec(&self, n: usize) {
        let s = self;
        s.index.set(s.index.get() - n);
    }

    fn has_more(&self) -> bool {
        let s = self;
        s.index.get() < s.tokens.len()
    }
    fn current(&self) -> &Token {
        let s = self;
        &s.tokens[s.index.get()]
    }
    fn nth(&self, n: usize) -> &Token {
        let s = self;
        &s.tokens[s.index.get() + n]
    }

    fn panic(&self, token: &Token, a: &str) -> ParseError {
        // panic!("{}:{}: {}", token.span.line, token.span.column, a);

        if token.span.column == std::usize::MAX {
            ParseError {
                line: token.span.line,
                column: 1,
                width: 100,
                text: a.to_string(),
            }
        } else {
            ParseError {
                line: token.span.line,
                column: token.span.column,
                width: token.span.width,
                text: a.to_string(),
            }
        }
    }

    fn expect(&self, code: TokenCode) -> ParseResult<()> {
        let s = self;

        let token = s.current();
        if token.code != code {
            return Err(s.panic(&token, &format!("expected {:?} but got {:?}", code, token)));
        }
        s.plusplus();
        Ok(())
    }

    fn skip_semicolons(&self) {
        let s = self;
        while s.has_more() {
            if !matches!(s.current().code, T![;]) {
                break;
            }
            s.plusplus();
        }
    }

    // when encounters ( [ or {{, fast-forward to ) ] or }}
    fn skip_pair(&self, start_index: usize, token: &Token) -> ParseResult<usize> {
        let s = self;

        let delim_open;
        let delim_close;
        match token.code {
            T!["("] => {
                delim_open = Pair::ParenOpen;
                delim_close = Pair::ParenClose;
            }
            T!["["] => {
                delim_open = Pair::SquareOpen;
                delim_close = Pair::SquareClose;
            }
            T!["{"] => {
                delim_open = Pair::BraceOpen;
                delim_close = Pair::BraceClose;
            }
            T!["{{"] => {
                delim_open = Pair::DoubleBraceOpen;
                delim_close = Pair::DoubleBraceClose;
            }
            T!["#["] => {
                delim_open = Pair::OuterAttr;
                delim_close = Pair::SquareClose;
            }
            T!["#!["] => {
                delim_open = Pair::InnerAttr;
                delim_close = Pair::SquareClose;
            }
            _ => return Err(s.panic(token, &format!("illegal token {:?} in skip_pair", token))),
        }

        let mut start = start_index;
        let mut delimiter_count = 1;
        while start < s.tokens.len() {
            let token = &s.tokens[start];
            start += 1;
            if let TokenCode::Pair(pair) = &token.code {
                if *pair == delim_open {
                    delimiter_count += 1;
                    continue;
                }
                if *pair == delim_close {
                    delimiter_count -= 1;
                    if delimiter_count == 0 {
                        break;
                    }
                }
            }
        }
        if delimiter_count > 0 {
            return Err(s.panic(token, "unclosed delimiter"));
        }

        Ok(start)
    }

    // --------------------------------------------------------------------------------
    // main
    pub fn parse(&mut self) -> Result<(), Vec<ParseError>> {
        let s = self;

        if s.tokens.is_empty() {
            return Ok(());
        }
        let u_pair = UPair {
            start: 0,
            end: s.tokens.len(),
        };

        // possible shebang
        let token = s.current();
        let shebang = if let TokenCode::Shebang = &token.code {
            s.plusplus();
            Some(token.clone())
        } else {
            None
        };

        let end_tokens = s.end_tokens.clone();
        if !s.has_more() {
            s.pkg = Package {
                shebang,
                end_tokens,
                u_pair,
                ..Default::default()
            };
            return Ok(());
        }

        // possible inner attrs
        let inner_attrs = match s.parse_inner_attrs() {
            Ok(ok) => ok,
            Err(err) => {
                return Err(vec![err]);
            }
        };

        // items
        let mut items = Vec::new();
        while s.has_more() {
            s.skip_semicolons();
            if !s.has_more() {
                break;
            }

            let token = s.current();
            match s.parse_stmt() {
                Err(err) => {
                    return Err(vec![err]);
                }
                Ok(ok) => {
                    if let Stmt::Item(item) = ok {
                        items.push(item);
                    } else {
                        let err = s.panic(&token, "expected Item");
                        return Err(vec![err]);
                    }
                }
            }
        }

        s.pkg = Package {
            shebang,
            inner_attrs,
            items,
            end_tokens,
            u_pair,
        };
        Ok(())
    }

    fn parse_block_expr(&self) -> ParseResult<BlockExpr> {
        let s = self;

        let mut r = BlockExpr::default();

        r.t_brace_open = s.current().clone();
        s.expect(T!["{"])?;
        r.t_first_after_brace = s.current().clone();

        r.inner_attrs = s.parse_inner_attrs()?;

        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                r.t_brace_close = token.clone();
                s.plusplus();
                break;
            }
            let stmt = s.parse_stmt()?;
            r.stmts.push(stmt);
            s.skip_semicolons();
            if !s.has_more() {
                return Err(s.panic(&token, "expected } for ending block expr"));
            }
        }

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    // item, let, expr
    fn parse_stmt(&self) -> ParseResult<Stmt> {
        let s = self;

        // parse item's outer_attrs
        let outer_attrs = s.parse_outer_attrs()?;

        // parse item payload
        s.skip_semicolons();
        if !s.has_more() {
            s.minusminus();
            return Err(s.panic(&s.current(), "expected Stmt"));
        }

        // < Expr
        // can end with }, i.e. if true { < 1 } else { < 0 }
        if matches!(&s.current().code, T![<]) {
            s.plusplus();
            let expr = s.parse_expr()?;
            return Ok(Stmt::Value(StmtValue { outer_attrs, expr }));
        }

        // get_first_special_token
        let first_special_token = s.get_first_special_token()?;

        if first_special_token.is_none() {
            // parse expr
            let expr = s.parse_expr()?;
            s.expect(T![;])?;

            if let ExprPayload::WithoutBlock(b) = expr.payload {
                if let ExprWithoutBlock::MacroCall(call) = *b {
                    return Ok(Stmt::Item(Item {
                        outer_attrs,
                        vis: Visibility::default(),
                        payload: ItemPayload::MacroCall(call),
                    }));
                } else {
                    return Ok(Stmt::Expr(Expr {
                        outer_attrs: expr.outer_attrs,
                        payload: ExprPayload::WithoutBlock(b),
                    }));
                }
            } else {
                return Ok(Stmt::Expr(expr));
            }
        }

        let stmt: Stmt;
        let special_token = first_special_token.unwrap();
        match &special_token.code {
            T![:=] => {
                let mut a = s.parse_let()?;
                a.outer_attrs = outer_attrs;
                stmt = Stmt::Let(a);
            }
            // Expr -> Pattern (let Type)? (else)?
            T![->] => {
                let mut a = StmtLet::default();
                a.span_eq = special_token.span;
                a.outer_attrs = outer_attrs;
                a.expr = Some(s.parse_expr()?);
                s.expect(T![->])?;
                a.pattern = s.parse_pattern()?;
                if matches!(&s.current().code, T![let]) {
                    s.plusplus();
                    a.type_ = Some(s.parse_type()?);
                }
                if matches!(&s.current().code, T![else]) {
                    let span_else = s.current().span;
                    a.span_else = Some(span_else);
                    s.plusplus();
                    a.else_ = Some(s.parse_block_expr()?);
                } else {
                    s.expect(T![;])?;
                }
                stmt = Stmt::Let(a);
            }
            TokenCode::Keyword(keyword) => match keyword {
                Keyword::Use => {
                    let token = s.current();
                    s.expect(T![use])?;
                    let vis = s.parse_vis()?;
                    let mut a = s.parse_use()?;
                    a.t_use = token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis,
                        payload: ItemPayload::Use(a),
                    });
                }
                Keyword::Extern => {
                    if matches!(s.nth(1).code, T![crate]) {
                        let a = s.parse_crate()?;
                        stmt = Stmt::Item(Item {
                            outer_attrs,
                            vis: Visibility::default(),
                            payload: ItemPayload::Crate(a),
                        });
                    } else {
                        s.expect(T![extern])?;
                        let mut a = s.parse_extern()?;
                        a.span_extern = special_token.span;
                        stmt = Stmt::Item(Item {
                            outer_attrs,
                            vis: Visibility::default(),
                            payload: ItemPayload::Extern(a),
                        });
                    }
                }
                Keyword::Const => {
                    let n = s.parse_identifier_pub()?;
                    s.expect(T![const])?;
                    let mut a = s.parse_const()?;
                    a.name = n.name;
                    a.t_const = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Const(a),
                    });
                }
                Keyword::Enum => {
                    let n = s.parse_identifier_pub_generics()?;
                    s.expect(T![enum])?;
                    let mut a = s.parse_enum()?;
                    a.name = n.name;
                    a.generics = n.generics;
                    a.span_enum = special_token.span;
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Enum(a),
                    });
                }
                Keyword::Func => {
                    let n = s.parse_identifier_pub_generics()?;

                    // self param
                    let mut self_param = None;
                    if matches!(s.current().code, T!["("]) {
                        self_param = Some(s.parse_func_self_param()?);
                    }
                    s.expect(T![func])?;

                    let mut a = s.parse_func()?;
                    a.name = n.name;
                    a.generics = n.generics;
                    a.self_param = self_param;
                    a.t_func = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Func(Box::new(a)),
                    });
                }
                Keyword::Impl => {
                    let left = s.parse_impl_left()?;
                    s.expect(T![impl])?;
                    let mut a = s.parse_impl()?;
                    a.type_ = left.type_;
                    a.generics = left.impl_generics;
                    a.span_impl = special_token.span;
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: Visibility::default(),
                        payload: ItemPayload::Impl(a),
                    });
                }
                Keyword::Trait => {
                    let n = s.parse_identifier_pub_generics()?;
                    s.expect(T![trait])?;
                    let mut a = s.parse_trait()?;
                    a.name = n.name;
                    a.generics = n.generics;
                    a.span_trait = special_token.span;
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Trait(a),
                    });
                }
                Keyword::Macro => {
                    let n = s.parse_identifier_pub()?;
                    s.expect(T![macro])?;
                    let mut a = s.parse_macro()?;
                    a.name = n.name;
                    a.span_macro = special_token.span;
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Macro(a),
                    });
                }
                Keyword::Mod => {
                    let n = s.parse_identifier_pub()?;
                    s.expect(T![mod])?;
                    let mut a = s.parse_mod()?;
                    a.name = n.name;
                    a.t_mod = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Mod(a),
                    });
                }
                Keyword::Static => {
                    let n = s.parse_identifier_pub()?;
                    s.expect(T![static])?;
                    let mut a = s.parse_static()?;
                    a.name = n.name;
                    a.t_static = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Static(a),
                    });
                }
                Keyword::Struct => {
                    let n = s.parse_identifier_pub_generics()?;
                    s.expect(T![struct])?;
                    let mut a = s.parse_struct()?;
                    a.name = n.name;
                    a.generics = n.generics;
                    a.t_struct = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Struct(a),
                    });
                }
                Keyword::Union => {
                    let n = s.parse_identifier_pub_generics()?;
                    s.expect(T![union])?;
                    let mut a = s.parse_union()?;
                    a.name = n.name;
                    a.generics = n.generics;
                    a.span_union = special_token.span;
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::Union(a),
                    });
                }
                Keyword::Type => {
                    let n = s.parse_identifier_pub_generics()?;
                    s.expect(T![type])?;
                    let mut a = s.parse_type_alias()?;
                    a.name = n.name;
                    a.generics = n.generics;
                    a.t_type = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        vis: n.vis,
                        payload: ItemPayload::TypeAlias(a),
                    });
                }
                _ => {
                    unreachable!("not a valid first_special_token");
                }
            },
            _ => {
                unreachable!("not a valid first_special_token");
            }
        }

        Ok(stmt)
    }

    fn get_first_special_token(&self) -> ParseResult<Option<&Token>> {
        let s = self;

        // get the tokens from after identifier to the first semicolon(;) or
        // opening brace({), skipping pairs(parens, etc.) in the process
        let mut first_special_token: Option<&Token> = None;
        let mut start = s.index.get();
        while start < s.tokens.len() {
            let token = &s.tokens[start];
            start += 1;
            match &token.code {
                T![;] => {
                    break;
                }
                T!["}"] => {
                    break;
                }

                TokenCode::Pair(_) => {
                    start = s.skip_pair(start, token)?;
                }
                T![:=] | T![->] => {
                    first_special_token = Some(token);
                    break;
                }

                T![,,]
                | T![=]
                | T![+=]
                | T![~=]
                | T![*=]
                | T![/=]
                | T![%=]
                | T![&=]
                | T![|=]
                | T![^=]
                | T![<<=]
                | T![>>=] => {
                    break;
                }

                T![if] | T![for] | T![match] => {
                    break;
                }

                TokenCode::Keyword(keyword) => {
                    if start >= 2 {
                        let prev_token = &s.tokens[start - 2];
                        match prev_token.code {
                            T![.] | T![..] => {
                                // skip variables used in third-party code,
                                // like a.trait or a..trait
                                continue;
                            }
                            _ => (),
                        }
                    }

                    match keyword {
                        Keyword::Const => {
                            if start >= 2 {
                                // for case like that:
                                // ret r as *const T as *const c-void
                                let last_token = &s.tokens[start - 2];
                                if matches!(&last_token.code, T![*]) {
                                    continue;
                                }
                            }
                            let next_token = &s.tokens[start];
                            if !matches!(&next_token.code, T!["{"]) {
                                first_special_token = Some(token);
                                break;
                            }
                        }
                        Keyword::Use
                        | Keyword::Extern
                        | Keyword::Enum
                        | Keyword::Func
                        | Keyword::Impl
                        | Keyword::Let
                        | Keyword::Trait
                        | Keyword::Macro
                        | Keyword::Mod
                        | Keyword::Static
                        | Keyword::Struct
                        | Keyword::Type
                        | Keyword::Union => {
                            first_special_token = Some(token);
                            break;
                        }
                        // NOTE impl trait are not allowed outside of function and
                        // inherent method return types
                        // for implementing complicated types that involve function pointers,
                        // wrap them in parens
                        _ => {}
                    }
                }

                _ => (),
            }
        }

        Ok(first_special_token)
    }

    // --------------------------------------------------------------------------------
    // parse lhs of first special token

    fn parse_vis(&self) -> ParseResult<Visibility> {
        let s = self;

        // default
        let plus_token = s.current();
        match &plus_token.code {
            T![+] => {
                s.plusplus();
            }
            T![^] => {
                s.plusplus();
                return Ok(Visibility {
                    payload: VisibilityPayload::Crate,
                });
            }
            _ => {
                return Ok(Visibility::default());
            }
        }

        // +
        if !matches!(s.current().code, T!["("]) {
            return pub_vis();
        };

        // the token after '+('
        let token = s.nth(1);
        // + ()
        if matches!(token.code, T![")"]) {
            return pub_vis();
        }

        // +(sth)
        if matches!(s.nth(2).code, T![")"]) {
            s.inc(3);
            let r = match token.code {
                T![self] => Visibility {
                    payload: VisibilityPayload::Private(true),
                },
                T![super] => Visibility {
                    payload: VisibilityPayload::Super,
                },
                T![crate] => Visibility {
                    payload: VisibilityPayload::Crate,
                },
                _ => {
                    // not part of +()
                    s.dec(3);
                    Visibility {
                        payload: VisibilityPayload::Pub,
                    }
                }
            };
            return Ok(r);
        }

        // +(in some-path)
        if matches!(token.code, T![in]) {
            s.inc(2);
            let items = s.parse_path_expr_segments()?;
            let mut path = SimplePath::default();
            for item in items {
                if !item.generic_args.items.is_empty() {
                    return Err(s.panic(token, "pub can't contain generic params"));
                }
                let result = SimplePathSegment::try_from(item.name);
                let Ok(a) = result else {
                    return Err(s.panic(token, "pub can't contain Self"));
                };
                path.items.push(a);
            }
            s.plusplus();
            let r = Visibility {
                payload: VisibilityPayload::SimplePath(path),
            };
            return Ok(r);
        }

        return pub_vis();

        fn pub_vis() -> ParseResult<Visibility> {
            return Ok(Visibility {
                payload: VisibilityPayload::Pub,
            });
        }
    }

    fn parse_identifier_pub(&self) -> ParseResult<IdentifierPlus> {
        let s = self;

        let mut r = IdentifierPlus::default();

        let token = s.current();
        r.name = token.clone();
        s.plusplus();

        // expect identifier
        let TokenCode::Identifier(identifier) = &token.code else {
            return Err(s.panic(token, &format!("expected identifier, but got {:?}", token)));
        };
        s.check_identifier(identifier, &token)?;

        r.vis = s.parse_vis()?;

        Ok(r)
    }

    fn parse_identifier_pub_generics(&self) -> ParseResult<IdentifierPlusGenerics> {
        let s = self;

        let mut r = IdentifierPlusGenerics::default();

        let a = s.parse_identifier_pub()?;
        r.name = a.name;
        r.vis = a.vis;

        // possible [
        let mut generic_span = None;
        let token = s.current();
        if matches!(token.code, T!["["]) {
            generic_span = Some(token.span);
            s.plusplus();
        }

        if let Some(span) = generic_span {
            let mut generics = s.parse_generics()?;
            generics.span_square_open = span;
            r.generics = Some(generics);
        }

        Ok(r)
    }

    fn check_identifier(&self, a: &Identifier, token: &Token) -> ParseResult<()> {
        let s = self;
        let id = &a.id;

        if id.is_empty() {
            return Ok(());
        }
        let chars: Vec<char> = id.chars().collect();
        if !chars[0].is_uppercase() {
            return Ok(());
        }
        if chars.ends_with(&['-', '-', 'r']) {
            return Ok(());
        }
        let list = chars.split(|c| *c == '-');
        for word in list {
            if word.is_empty() {
                continue;
            }
            match word[0] {
                '0'..='9' => return Err(s.panic(token, "forbid '-' followed by digit")),
                _ => {}
            }
        }
        Ok(())
    }

    // Type [Generics]? impl[unsafe] Trait where { Where } {}
    fn parse_impl_left(&self) -> ParseResult<ImplLeft> {
        let s = self;

        let mut r = ImplLeft::default();

        // if the type contains special keywords(like impl),
        // wrap the type in parens, otherwise it's a syntax error
        // NOTE BUT, it appears that won't happen

        // if Type is parenthesized, unwrap it
        // let type_ = s.parse_type()?;
        // if let Type::TypeNoBounds(a) = type_ {
        //     if let TypeNoBounds::Parenthesized(p) = *a {
        //         r.type_ = p.type_;
        //     } else {
        //         r.type_ = Type::TypeNoBounds(a);
        //     }
        // } else {
        //     r.type_ = type_;
        // }
        r.type_ = s.parse_type()?;

        let token = s.current();
        if matches!(&token.code, T![:]) {
            s.plusplus();
            s.expect(T!["["])?;
            let mut a = s.parse_generics()?;
            a.span_square_open = token.span;
            r.impl_generics = Some(a);
        }

        Ok(r)
    }

    // after [
    fn parse_generics(&self) -> ParseResult<Generics> {
        let s = self;

        let mut r = Generics::default();

        let token = s.current();
        if matches!(&token.code, T!["]"]) {
            return Err(s.panic(token, "generic params can't be empty"));
        }

        while s.has_more() {
            let param = parse_generic_param(s)?;
            r.params.push(param);

            let token = s.current();
            s.plusplus();
            match &token.code {
                T!["]"] => {
                    r.span_square_close = token.span;
                    break;
                }
                T![,] => (),
                _ => {
                    return Err(s.panic(
                        token,
                        &format!("illegal token {:?} for generic params", token),
                    ));
                }
            }
        }

        return Ok(r);

        fn parse_generic_param(s: &Parse) -> ParseResult<GenericParam> {
            let mut r = GenericParam::default();

            while s.has_more() {
                let token = s.current();
                match &token.code {
                    T!["]"] | T![,] => {
                        break;
                    }
                    T!["#["] => {
                        s.plusplus();
                        r.outer_attrs = s.parse_outer_attrs()?;
                    }
                    TokenCode::Lifetime(_) => {
                        s.plusplus();
                        let mut c = s.parse_lifetime_bounds()?;
                        c.lifetime = token.clone();
                        r.payload = GenericParamPayload::Lifetime(c);
                    }
                    TokenCode::Identifier(identifier) => {
                        s.plusplus();
                        let name = identifier.clone();

                        let token = s.current();
                        if matches!(token.code, T![const]) {
                            s.plusplus();
                            let mut c = parse_const_param(s)?;
                            c.span_const = token.span;
                            c.name = name;
                            r.payload = GenericParamPayload::Const(c);
                        } else {
                            let mut c = parse_type_param(s)?;
                            c.name = name;
                            r.payload = GenericParamPayload::Type(c);
                        }
                    }
                    _ => {
                        return Err(s.panic(
                            token,
                            &format!("illegal token {:?} for generic params", token),
                        ));
                    }
                }
            }

            Ok(r)
        }

        fn parse_type_param(s: &Parse) -> ParseResult<TypeParam> {
            let mut r = TypeParam::default();

            let token = s.current();
            match &token.code {
                T!["]"] | T![,] => (),
                T![=] => {
                    s.plusplus();
                    r.type_ = Some(s.parse_type()?);
                }
                _ => {
                    r.bounds = s.parse_type_param_bounds()?;

                    let token = s.current();
                    match &token.code {
                        T!["]"] | T![,] => (),
                        T![=] => {
                            s.plusplus();
                            r.type_ = Some(s.parse_type()?);
                        }
                        _ => {
                            return Err(s.panic(
                                token,
                                &format!("illegal token {:?} for type param", token),
                            ));
                        }
                    }
                }
            }

            Ok(r)
        }

        fn parse_const_param(s: &Parse) -> ParseResult<ConstParam> {
            let mut r = ConstParam::default();

            r.type_ = s.parse_type()?;
            let token = s.current();
            if token.code == T![=] {
                s.plusplus();
                let expr = s.parse_expr()?;
                r.expr = Some(expr);
            }
            let token = s.current();
            if !matches!(&token.code, T!["]"] | T![,]) {
                return Err(s.panic(token, &format!("expected ] or , but got {:?}", token)));
            }

            Ok(r)
        }
    }

    // --------------------------------------------------------------------------------
    // bounds

    fn parse_lifetime_bounds(&self) -> ParseResult<LifetimeParam> {
        let s = self;

        let mut r = LifetimeParam::default();

        let mut expect_plus = false;
        while s.has_more() {
            let token = s.current();
            match &token.code {
                T!["]"] | T![,] | T![;] => {
                    break;
                }
                TokenCode::Lifetime(_) => {
                    if expect_plus {
                        return Err(s.panic(
                            token,
                            &format!("not expected lifetime but got lifetime '{:?}", token),
                        ));
                    }
                    r.lifetime_bounds.push(Lifetime {
                        name: token.clone(),
                    });
                    expect_plus = true;
                    s.plusplus();
                }
                T![+] => {
                    if !expect_plus {
                        return Err(s.panic(token, "expected lifetime but got +"));
                    }
                    expect_plus = false;
                    s.plusplus();
                }
                _ => {
                    return Err(s.panic(
                        token,
                        &format!("illegal token {:?} for lifetime bounds", token),
                    ));
                }
            }
        }

        Ok(r)
    }

    fn parse_type_param_bounds(&self) -> ParseResult<TypeParamBounds> {
        let s = self;

        let mut r = TypeParamBounds::default();

        while s.has_more() {
            let token = s.current();

            if let TokenCode::Lifetime(_) = &token.code {
                r.items.push(TypeParamBound::Lifetime(Lifetime {
                    name: token.clone(),
                }));
                s.plusplus();
            } else if matches!(&token.code, T![use]) {
                let span_use = token.span;
                s.plusplus();
                let token = s.current();
                let span_square_open = token.span;
                s.expect(T!["["])?;
                let mut items = Vec::new();
                while s.has_more() {
                    let token = s.current();
                    s.plusplus();
                    match &token.code {
                        TokenCode::Lifetime(_) => {
                            items.push(UseBoundGenericArg::Lifetime(Lifetime {
                                name: token.clone(),
                            }));
                        }
                        TokenCode::Identifier(identifier) => {
                            items.push(UseBoundGenericArg::Id(identifier.clone()));
                        }
                        T![Self] => {
                            items.push(UseBoundGenericArg::Self_(token.span));
                        }
                        T![,] => {
                            // skip check for now
                        }
                        T!["]"] => {
                            let span_square_close = token.span;
                            let use_bound = UseBound {
                                items,
                                span_use,
                                span_square_open,
                                span_square_close,
                            };
                            r.items.push(TypeParamBound::UseBound(use_bound));
                            break;
                        }
                        _ => {
                            return Err(s.panic(
                                token,
                                &format!("illegal token {:?} for type param bounds", token),
                            ));
                        }
                    }
                }
            } else {
                let mut bound = TraitBound::default();
                // possible ?
                if matches!(&token.code, T![?]) {
                    bound.span_unsized = Some(token.span);
                    bound.unsized_ = true;
                    s.plusplus();
                }
                // possible ForLifetimes
                let token = s.current();
                if matches!(&token.code, T![for]) {
                    s.plusplus();
                    let mut f = s.parse_for_lifetimes()?;
                    f.span_for = token.span;
                    bound.for_lifetimes = Some(f);
                }
                bound.type_path = s.parse_type_path()?;
                r.items.push(TypeParamBound::TraitBound(bound));
            }

            let token = s.current();
            if !matches!(&token.code, T![+]) {
                break;
            }
            s.plusplus();
        }

        Ok(r)
    }

    fn parse_trait_bound(&self) -> ParseResult<TraitBound> {
        let s = self;
        let mut r = TraitBound::default();

        let mut need_paren = false;
        let token = s.current();
        if matches!(&token.code, T!["("]) {
            need_paren = true;
            s.plusplus();
        }

        let token = s.current();
        if matches!(&token.code, T![?]) {
            r.span_unsized = Some(token.span);
            r.unsized_ = true;
            s.plusplus();
        }

        let token = s.current();
        if matches!(&token.code, T![for]) {
            s.plusplus();
            let mut f = s.parse_for_lifetimes()?;
            f.span_for = token.span;
            r.for_lifetimes = Some(f);
        }

        r.type_path = s.parse_type_path()?;

        if need_paren {
            s.expect(T![")"])?;
        }

        Ok(r)
    }

    fn parse_for_lifetimes(&self) -> ParseResult<ForLifetimes> {
        let s = self;
        let mut r = ForLifetimes::default();

        let token = s.current();
        s.expect(T!["["])?;
        r.for_ = s.parse_generics()?;
        r.for_.span_square_open = token.span;

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    // items
    fn parse_mod(&self) -> ParseResult<Mod> {
        let s = self;

        let mut r = Mod::default();

        if matches!(&s.current().code, T![;]) {
            return Ok(r);
        }

        let t_brace_open = s.current().clone();
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;

        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                r.t_braces = Some((t_brace_open, token.clone()));
                s.plusplus();
                break;
            }
            let stmt = s.parse_stmt()?;
            let Stmt::Item(item) = stmt else {
                return Err(s.panic(token, "expected Item"));
            };
            r.items.push(item);

            s.skip_semicolons();
            if !s.has_more() {
                return Err(s.panic(&s.tokens[s.tokens.len() - 1], "expected } for ending mod"));
            }
        }

        Ok(r)
    }

    fn parse_crate(&self) -> ParseResult<Crate> {
        let s = self;

        let mut r = Crate::default();

        r.t_extern = s.current().clone();
        s.plusplus();

        // crate
        s.plusplus();

        r.name = s.current().clone();
        if !matches!(&r.name.code, TokenCode::Identifier(_)) {
            return Err(s.panic(&r.name,
                &format!("expected identifier after 'crate', but got {:?}", r.name)));
        }
        s.plusplus();

        let t_as = s.current();
        if matches!(&t_as.code, T![as]) {
            s.plusplus();
            let token = s.current();
            if !matches!(&token.code, TokenCode::Identifier(_)) {
                return Err(s.panic(&token,
                    &format!("expected identifier after 'as', but got {:?}", token)));
            }
            r.alias = Some((t_as.clone(), token.clone()));
            s.plusplus();
        }
        s.expect(T![;])?;

        return Ok(r);
    }

    fn parse_use(&self) -> ParseResult<Use> {
        let s = self;

        let mut r = Use::default();
        r.item = parse_use_item(s)?;
        return Ok(r);

        fn parse_use_item(s: &Parse) -> ParseResult<UseItem> {
            let mut r = UseItem::default();

            let token = s.current();
            if matches!(&token.code, T!["{"]) {
                r.sub = Some(parse_use_item_sub(s)?);
                return Ok(r);
            }

            // identifier
            while s.has_more() {
                let token = s.current();
                if matches!(&token.code, T![as]) {
                    s.plusplus();
                    r.alias = Some((token.clone(), s.current().clone()));
                    s.plusplus(); // identifier
                    return Ok(r);
                }
                if matches!(&token.code, T![*]) {
                    s.plusplus();
                    // needed this, because there's no auto-added ';' after '*'
                    r.paths.push(token.clone());
                    return Ok(r);
                }
                if matches!(&token.code, T![;] | T![,] | T!["}"]) {
                    return Ok(r);
                }
                if matches!(&token.code, T!["{"]) {
                    r.sub = Some(parse_use_item_sub(s)?);
                    return Ok(r);
                }
                s.plusplus();
                r.paths.push(token.clone());
            }
            // unreachable
            return Ok(r);
        }

        fn parse_use_item_sub(s: &Parse) -> ParseResult<UseItemSub> {
            let mut r = UseItemSub::default();

            let token = s.current();
            r.t_brace_open = token.clone();
            s.plusplus();

            let token = s.current();
            // so that you can type {.. to trigger auto-completion
            r.leading_sep = if matches!(token.code, T![..]) {
                s.plusplus();
                if s.magic { true } else { false }
            } else {
                false
            };

            while s.has_more() {
                let it = parse_use_item(s)?;

                s.minusminus();
                let before_token = s.current();
                s.plusplus();
                let token = s.current();
                // add ',' after '*' if the next token after '*' is not at the same line
                if matches!(&before_token.code, T![*]) &&
                            token.span.line > before_token.span.line {
                    let span = before_token.span;
                    let t = Token {
                        code: T![,],
                        span: Span {
                            column: span.column + 1,
                            end_column: span.end_column + 1,
                            ..span
                        },
                        tokens_before: Vec::new(),
                        u: ",".to_owned(),
                    };
                    r.items.push((it, Some(t)));
                    if matches!(&token.code, T!["}"]) {
                        r.t_brace_close = token.clone();
                        s.plusplus();
                        return Ok(r);
                    }
                    continue;
                }

                s.plusplus();
                match &token.code {
                    T![,] => {
                        r.items.push((it, Some(token.clone())));
                        let token = s.current();
                        if matches!(&token.code, T!["}"]) {
                            r.t_brace_close = token.clone();
                            s.plusplus();
                            return Ok(r);
                        }
                    }
                    T![;] => {
                        let mut t = token.clone();
                        t.code = T![,];
                        r.items.push((it, Some(t)));
                        let token = s.current();
                        if matches!(&token.code, T!["}"]) {
                            r.t_brace_close = token.clone();
                            s.plusplus();
                            return Ok(r);
                        }
                    }
                    T!["}"] => {
                        r.items.push((it, None));
                        r.t_brace_close = token.clone();
                        return Ok(r);
                    }
                    _ => {}
                }
            }

            return Ok(r);
        }
    }

    fn parse_extern(&self) -> ParseResult<ExternalBlock> {
        let s = self;

        let mut r = ExternalBlock::default();

        // possible [unsafe]
        let token = s.current();
        if matches!(&token.code, T!["["]) {
            r.unsafe_ = true;
            s.plusplus();
            let token = s.current();
            r.span_unsafe = Some(token.span);
            if !matches!(&token.code, T![unsafe]) {
                return Err(s.panic(token, &format!("expected unsafe but got {:?}", token)));
            }
            s.plusplus();
            s.expect(T!["]"])?;
        }

        // possible string literal, i.e. "C"
        let token = s.current();
        if let TokenCode::Literal(Literal {
            payload: LiteralPayload::String(a),
            ..
        }) = &token.code
        {
            r.span_abi = Some(token.span);
            r.extern_ = Some(a.clone());
            s.plusplus();
        } else {
            r.extern_ = None;
        }

        // items
        r.span_brace_open = s.current().span;
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                r.span_brace_close = token.span;
                s.plusplus();
                break;
            }
            let token = s.current();
            let stmt = s.parse_stmt()?;
            match stmt {
                Stmt::Item(item) => match item.payload {
                    ItemPayload::Func(a) => {
                        r.items.push(ExternalBlockItem {
                            outer_attrs: item.outer_attrs,
                            payload: ExternalBlockItemPayload::Func(*a),
                            vis: item.vis,
                        });
                    }
                    ItemPayload::Static(a) => {
                        r.items.push(ExternalBlockItem {
                            outer_attrs: item.outer_attrs,
                            payload: ExternalBlockItemPayload::Static(a),
                            vis: item.vis,
                        });
                    }
                    ItemPayload::MacroCall(a) => {
                        r.items.push(ExternalBlockItem {
                            outer_attrs: item.outer_attrs,
                            payload: ExternalBlockItemPayload::MacroCall(a),
                            vis: item.vis,
                        });
                    }
                    _ => return Err(s.panic(token, "illegal extern item")),
                },
                _ => return Err(s.panic(token, "illegal extern item")),
            }

            s.skip_semicolons();
            if !s.has_more() {
                return Err(s.panic(&s.tokens[s.tokens.len() - 1], "expected }"));
            }
        }

        Ok(r)
    }

    fn parse_func(&self) -> ParseResult<Func> {
        let s = self;

        let mut r = Func::default();

        // qualifier
        let token = s.current();
        if matches!(&token.code, T!["["]) {
            s.plusplus();
            while s.has_more() {
                let token = s.current();
                s.plusplus();
                match &token.code {
                    T![async] => {
                        if r.qualifier.t_async.is_some() {
                            return Err(s.panic(token, "already set async"));
                        }
                        r.qualifier.t_async = Some(token.clone());
                    }
                    T![const] => {
                        if r.qualifier.t_const.is_some() {
                            return Err(s.panic(token, "already set const"));
                        }
                        r.qualifier.t_const = Some(token.clone());
                    }
                    T![unsafe] => {
                        if r.qualifier.t_unsafe.is_some() {
                            return Err(s.panic(token, "already set unsafe"));
                        }
                        r.qualifier.t_unsafe = Some(token.clone());
                    }
                    T![extern] => {
                        if r.qualifier.t_extern.is_some() {
                            return Err(s.panic(token, "already set extern"));
                        }
                        let next_token = s.current();
                        if let TokenCode::Literal(Literal {
                            payload: LiteralPayload::String(_),
                            ..
                        }) = &next_token.code
                        {
                            r.qualifier.t_extern = Some((
                                token.clone(),
                                Some(next_token.clone()),
                            ));
                            s.plusplus();
                        } else {
                            r.qualifier.t_extern = Some((token.clone(), None));
                        }
                    }
                    _ => {
                        if let TokenCode::Identifier(ident) = &token.code {
                            if ident.id == "safe" {
                                if r.qualifier.t_unsafe.is_some() {
                                    return Err(s.panic(token, "already set safe"));
                                }
                                r.qualifier.t_unsafe = Some(token.clone());
                            } else if ident.id == "no-s" {
                                r.qualifier.t_no_s = Some(token.clone());
                            }
                        } else {
                            return Err(s.panic(
                                token,
                                &format!("illegal token {:?} in func qualifier", token),
                            ));
                        }
                    }
                }

                let token = s.current();
                s.plusplus();
                match &token.code {
                    T![,] => (),
                    T!["]"] => {
                        break;
                    }
                    _ => {
                        return Err(s.panic(token, &format!("expected , or ] but got {:?}", token)));
                    }
                }
            }
        }

        // params
        r.t_paren_open = s.current().clone();
        s.expect(T!["("])?;
        r.t_first_after_paren = s.current().clone();
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T![")"]) {
                r.t_paren_close = token.clone();
                s.plusplus();
                break;
            }

            let mut param = FuncParam::default();
            param.outer_attrs = s.parse_outer_attrs()?;

            // variadics
            let token = s.current();
            if matches!(&token.code, T![...]) {
                let t_var = token.clone();
                s.plusplus();
                let token = s.current();
                let t_comma = if matches!(&token.code, T![,]) {
                    s.plusplus();
                    Some(token.clone())
                } else {
                    None
                };
                r.variadics = Some(FuncVariadics {
                    outer_attrs: param.outer_attrs,
                    t_var,
                    t_comma,
                });
                r.t_paren_close = s.current().clone();
                s.expect(T![")"])?;
                break;
            }

            let token = s.current();
            match &token.code {
                TokenCode::Identifier(_) => {
                    param.name = token.clone();
                    s.plusplus();
                    let token = s.current();
                    if matches!(&token.code, T![:]) {
                        return Err(s.panic(
                            token,
                            &format!("don't add ':' after param name '{:?}'", param.name),
                        ));
                    }
                }
                T![:] => {
                    s.plusplus();
                    param.pat = Some(s.parse_pattern()?);
                    s.expect(T![:])?;
                }
                _ => {
                    return Err(s.panic(
                        token,
                        &format!("expected identifier or ':' but got {:?}", token),
                    ));
                }
            }
            param.type_ = s.parse_type()?;
            let token = s.current();
            s.plusplus();
            match &token.code {
                T![")"] => {
                    r.t_paren_close = token.clone();
                    r.params.push(param);
                    break;
                }
                T![,] => {
                    param.t_comma = Some(token.clone());
                    r.params.push(param);
                }
                T![;] => {
                    let mut t = token.clone();
                    t.code = T![,];
                    param.t_comma = Some(t);
                    r.params.push(param);
                }
                _ => {
                    return Err(s.panic(token, &format!("expected , or ) but got {:?}", token)));
                }
            }
        }

        let mut span_where = None;
        let token = s.current();
        match &token.code {
            T![;] => {
                s.plusplus();
                return Ok(r);
            }
            T![where] => {
                s.plusplus();
                span_where = Some(token.span);
            }
            T!["{"] => (),
            _ => {
                if let TokenCode::Literal(lit) = &token.code &&
                        let LiteralPayload::String(_)= lit.payload {
                    r.t_arrow = Some(token.clone());
                    s.plusplus();
                }
                // return type
                r.return_type = Some(s.parse_type()?);
                let token = s.current();
                if matches!(&token.code, T![where]) {
                    s.plusplus();
                    span_where = Some(token.span);
                }
            }
        }

        // where
        if let Some(span) = span_where {
            let mut w = s.parse_where()?;
            w.span_where = span;
            r.where_ = Some(w);
        }

        let token = s.current();
        s.plusplus();
        match &token.code {
            T![;] => {
                return Ok(r);
            }
            T!["{"] => {
                // body
                s.minusminus();
                let mut a = s.parse_block_expr()?;
                a.t_brace_open = token.clone();
                r.body = Some(a);
            }
            _ => {
                return Err(s.panic(token, &format!("expected ; or {{ but got {:?}", token)));
            }
        }

        Ok(r)
    }

    // (&), (&'a), (&'a mut), (&mut), (), (mut), (Type) (mut Type)
    fn parse_func_self_param(&self) -> ParseResult<SelfParam> {
        let s = self;

        let mut r = SelfParam::default();

        s.plusplus();
        r.outer_attrs = s.parse_outer_attrs()?;

        let mut short = SelfParamShort::default();

        let token = s.current();
        if matches!(token.code, T![&]) {
            short.t_ref = Some(token.clone());
            match &s.nth(1).code {
                // (&)
                T![")"] => {
                    r.payload = SelfParamPayload::Short(short);
                    s.inc(2);
                }
                TokenCode::Lifetime(_) => match &s.nth(2).code {
                    // (&'a)
                    T![")"] => {
                        short.label = Some(s.nth(1).clone());
                        r.payload = SelfParamPayload::Short(short);
                        s.inc(3);
                    }
                    // (&'a mut)
                    T![mut] => {
                        if matches!(&s.nth(3).code, T![")"]) {
                            short.label = Some(s.nth(1).clone());
                            short.t_mut = Some(s.nth(2).clone());
                            r.payload = SelfParamPayload::Short(short);
                            s.inc(4);
                        }
                    }
                    // (Type)
                    _ => {
                        let typ = SelfParamType {
                            typ: s.parse_type()?,
                            t_mut: None,
                        };
                        r.payload = SelfParamPayload::Type(typ);
                        s.expect(T![")"])?;
                    }
                },
                // (&mut)
                T![mut] => {
                    let a_token = s.nth(2);
                    if matches!(&a_token.code, T![")"]) {
                        short.t_mut = Some(s.nth(1).clone());
                        r.payload = SelfParamPayload::Short(short);
                        s.inc(3);
                    } else {
                        return Err(s.panic(a_token,
                            &format!("expected ')' after '(&mut' but got {:?}", a_token)));
                    }
                }
                // (Type)
                _ => {
                    let typ = SelfParamType {
                        typ: s.parse_type()?,
                        t_mut: None,
                    };
                    r.payload = SelfParamPayload::Type(typ);
                    s.expect(T![")"])?;
                }
            }
        } else if matches!(token.code, T![")"]) {
            // ()
            r.payload = SelfParamPayload::Short(short);
            s.plusplus();
        } else {
            // (mut) or (mut Type)
            let mut t_mut = None;
            if matches!(token.code, T![mut]) {
                t_mut = Some(token.clone());
                s.plusplus();
            }
            if matches!(s.current().code, T![")"]) {
                s.plusplus();
                short.t_mut = t_mut;
                r.payload = SelfParamPayload::Short(short);
            } else {
                let typ = SelfParamType {
                    typ: s.parse_type()?,
                    t_mut,
                };
                r.payload = SelfParamPayload::Type(typ);
                s.expect(T![")"])?;
            }
        }

        Ok(r)
    }

    fn parse_type_alias(&self) -> ParseResult<TypeAlias> {
        let s = self;

        let mut r = TypeAlias::default();

        // possible where clause
        let token = s.current();
        if matches!(&token.code, T![where]) {
            s.plusplus();
            let mut where_ = s.parse_where()?;
            where_.span_where = token.span;
            r.where_ = Some(where_);
        }

        let token = s.current();
        if matches!(&token.code, T![;]) {
            return Ok(r);
        }

        // possible TypeParamBounds
        if !matches!(&token.code, T![=]) {
            r.bounds = s.parse_type_param_bounds()?;
        }

        // possible type
        let token = s.current();
        if matches!(&token.code, T![=]) {
            let t_eq = token.clone();
            s.plusplus();
            let type_ = s.parse_type()?;

            // possible where clause
            let mut type_where = None;
            let token = s.current();
            if matches!(&token.code, T![where]) {
                s.plusplus();
                let mut where_ = s.parse_where()?;
                where_.span_where = token.span;
                type_where = Some(where_);
            }
            r.rhs = Some((t_eq, type_, type_where));
        }
        s.expect(T![;])?;

        Ok(r)
    }

    fn parse_struct(&self) -> ParseResult<Struct> {
        let s = self;

        let mut r = Struct::default();
        let mut has_where = false;

        while s.has_more() {
            let token = s.current();
            match &token.code {
                T![;] => {
                    break;
                }
                T!["{"] => {
                    r.t_brace_open = Some(token.clone());
                    s.plusplus();
                    let fields = s.parse_struct_fields()?;
                    r.t_brace_close = Some(s.current().clone());
                    s.expect(T!["}"])?;
                    r.payload = Some(StructPayload::Struct(fields));
                    s.expect(T![;])?;
                    break;
                }
                T!["("] => {
                    r.t_paren_open = Some(token.clone());
                    if has_where {
                        return Err(
                            s.panic(token, "where clause should be after parens in struct tuple")
                        );
                    }
                    s.plusplus();
                    let fields = s.parse_tuple_fields()?;
                    r.t_paren_close = Some(s.current().clone());
                    s.expect(T![")"])?;
                    r.payload = Some(StructPayload::Tuple(fields));
                    let token = s.current();
                    match &token.code {
                        T![;] => {
                            break;
                        }
                        T![where] => {
                            s.plusplus();
                            r.where_ = Some(s.parse_where()?);
                            s.expect(T![;])?;
                            break;
                        }
                        _ => {
                            return Err(s.panic(
                                token,
                                &format!("expected ; or where but got {:?}", token),
                            ));
                        }
                    }
                }
                T![where] => {
                    if has_where {
                        return Err(s.panic(token, "already has a where clause"));
                    }
                    s.plusplus();
                    let mut w = s.parse_where()?;
                    w.span_where = token.span;
                    r.where_ = Some(w);
                    has_where = true;
                }
                _ => {
                    return Err(s.panic(
                        token,
                        &format!("expected ; {{ ( or where but got {:?}", token),
                    ));
                }
            }
        }

        Ok(r)
    }
    fn parse_struct_fields(&self) -> ParseResult<Vec<StructField>> {
        let s = self;

        let mut r: Vec<StructField> = Vec::new();

        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                break;
            }

            let mut field = StructField::default();
            field.outer_attrs = s.parse_outer_attrs()?;

            let token = s.current();
            if let TokenCode::Identifier(identifier) = &token.code {
                field.name = identifier.clone();
                s.plusplus();
            } else {
                return Err(s.panic(token, &format!("expected identifier but got {:?}", token)));
            }

            field.vis = s.parse_vis()?;

            field.type_ = s.parse_type()?;
            s.expect(T![;])?;
            r.push(field);
        }

        Ok(r)
    }
    fn parse_tuple_fields(&self) -> ParseResult<Vec<TupleField>> {
        let s = self;

        let mut r: Vec<TupleField> = Vec::new();

        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T![")"]) {
                break;
            }

            let mut field = TupleField::default();
            field.outer_attrs = s.parse_outer_attrs()?;

            field.vis = s.parse_vis()?;

            field.type_ = s.parse_type()?;
            r.push(field);

            let token = s.current();
            s.plusplus();
            match &token.code {
                T![,] | T![;] => (),
                T![")"] => {
                    s.minusminus();
                    break;
                }
                _ => (),
            }
        }

        Ok(r)
    }

    fn parse_enum(&self) -> ParseResult<Enum> {
        let s = self;

        let mut r = Enum::default();
        let mut has_where = false;

        while s.has_more() {
            let token = s.current();
            match &token.code {
                T![;] => {
                    break;
                }
                T!["{"] => {
                    r.span_brace_open = Some(token.span);
                    s.plusplus();
                    r.items = parse_enum_items(s)?;
                    r.span_brace_close = Some(s.current().span);
                    s.expect(T!["}"])?;
                    s.expect(T![;])?;
                    break;
                }
                T![where] => {
                    if has_where {
                        return Err(s.panic(token, "already has a where clause"));
                    }
                    s.plusplus();
                    let mut w = s.parse_where()?;
                    w.span_where = token.span;
                    r.where_ = Some(w);
                    has_where = true;
                }
                _ => {
                    return Err(s.panic(
                        token,
                        &format!("expected ; {{ or where but got {:?}", token),
                    ));
                }
            }
        }

        return Ok(r);

        fn parse_enum_items(s: &Parse) -> ParseResult<Vec<EnumItem>> {
            let mut r: Vec<EnumItem> = Vec::new();

            while s.has_more() {
                let token = s.current();
                if matches!(&token.code, T!["}"]) {
                    break;
                }

                let mut field = EnumItem::default();
                field.outer_attrs = s.parse_outer_attrs()?;

                let token = s.current();
                if let TokenCode::Identifier(identifier) = &token.code {
                    field.name = identifier.clone();
                    s.plusplus();
                } else {
                    return Err(s.panic(token, &format!("expected identifier but got {:?}", token)));
                }

                // field.vis = s.parse_vis()?;

                let token = s.current();
                match &token.code {
                    T!["{"] => {
                        field.span_brace_open = Some(token.span);
                        s.plusplus();
                        let fields = s.parse_struct_fields()?;
                        field.span_brace_close = Some(s.current().span);
                        s.expect(T!["}"])?;
                        field.payload = EnumItemPayload::Struct(fields);
                    }
                    T!["("] => {
                        field.span_paren_open = Some(token.span);
                        s.plusplus();
                        let fields = s.parse_tuple_fields()?;
                        field.span_paren_close = Some(s.current().span);
                        s.expect(T![")"])?;
                        field.payload = EnumItemPayload::Tuple(fields);
                    }
                    _ => (),
                }

                let token = s.current();
                if matches!(token.code, T![=]) {
                    field.span_eq = Some(token.span);
                    s.plusplus();
                    let expr = s.parse_expr()?;
                    field.discriminant = Some(expr);
                }

                s.expect(T![;])?;
                r.push(field);
            }

            Ok(r)
        }
    }

    fn parse_union(&self) -> ParseResult<Union> {
        let s = self;

        let mut r = Union::default();
        let mut has_where = false;

        while s.has_more() {
            let token = s.current();
            match &token.code {
                T![;] => {
                    break;
                }
                T!["{"] => {
                    r.span_brace_open = Some(token.span);
                    s.plusplus();
                    r.fields = s.parse_struct_fields()?;
                    r.span_brace_close = Some(s.current().span);
                    s.expect(T!["}"])?;
                    s.expect(T![;])?;
                    break;
                }
                T![where] => {
                    if has_where {
                        return Err(s.panic(token, "already has a where clause"));
                    }
                    s.plusplus();
                    let mut w = s.parse_where()?;
                    w.span_where = token.span;
                    r.where_ = Some(w);
                    has_where = true;
                }
                _ => {
                    return Err(s.panic(
                        token,
                        &format!("expected ; {{ or where but got {:?}", token),
                    ));
                }
            }
        }

        Ok(r)
    }

    fn parse_const(&self) -> ParseResult<Const> {
        let s = self;

        let mut r = Const::default();

        r.type_ = s.parse_type()?;

        let token = s.current();
        if matches!(&token.code, T![;]) {
            return Ok(r);
        }

        s.expect(T![=])?;

        let t_eq = token.clone();
        let expr = s.parse_expr()?;
        r.expr = Some((t_eq, expr));
        s.expect(T![;])?;

        Ok(r)
    }

    fn parse_static(&self) -> ParseResult<Static> {
        let s = self;

        let mut r = Static::default();

        let token = s.current();
        if matches!(&token.code, T!["["]) {
            s.plusplus();
            let token = s.current();
            if let TokenCode::Identifier(ident) = &token.code && ident.id == "safe" {
                r.t_safe = Some(token.clone());
                s.plusplus();
            } else {
                return Err(s.panic(token, &format!("illegal token {:?} for static item", token)));
            }
            s.expect(T!["]"])?;
        }

        let token = s.current();
        if matches!(&token.code, T![mut]) {
            r.t_mut = Some(token.clone());
            s.plusplus();
        }

        r.type_ = s.parse_type()?;

        let token = s.current();
        if matches!(&token.code, T![=]) {
            let t_eq = token.clone();
            s.expect(T![=])?;
            let expr = s.parse_expr()?;
            r.expr = Some((t_eq, expr));
        }
        s.expect(T![;])?;

        Ok(r)
    }

    fn parse_trait(&self) -> ParseResult<Trait> {
        let s = self;

        let mut r = Trait::default();

        // possible [unsafe]
        let token = s.current();
        if matches!(&token.code, T!["["]) {
            s.plusplus();
            let token = s.current();
            r.span_unsafe = Some(token.span);
            if !matches!(&token.code, T![unsafe]) {
                return Err(s.panic(token, &format!("expected unsafe, but got {:?}", token)));
            }
            r.unsafe_ = true;
            s.plusplus();
            s.expect(T!["]"])?;
        }

        // possible TypeParamBounds
        let token = s.current();
        if !matches!(&token.code, T![where] | T!["{"]) {
            r.bounds = s.parse_type_param_bounds()?;
        }

        // possible where clause
        let token = s.current();
        if matches!(&token.code, T![where]) {
            s.plusplus();
            let mut w = s.parse_where()?;
            w.span_where = token.span;
            r.where_ = Some(w);
        }

        r.span_brace_open = s.current().span;
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                r.span_brace_close = token.span;
                s.plusplus();
                break;
            }
            let token = s.current();
            let stmt = s.parse_stmt()?;
            match stmt {
                Stmt::Item(item) => match item.payload {
                    ItemPayload::Func(f) => {
                        let a = AssociatedItem {
                            outer_attrs: item.outer_attrs,
                            vis: item.vis,
                            payload: AssociatedItemPayload::Func(*f),
                        };
                        r.items.push(a);
                    }
                    ItemPayload::Const(f) => {
                        let a = AssociatedItem {
                            outer_attrs: item.outer_attrs,
                            vis: item.vis,
                            payload: AssociatedItemPayload::Const(f),
                        };
                        r.items.push(a);
                    }
                    ItemPayload::TypeAlias(f) => {
                        let a = AssociatedItem {
                            outer_attrs: item.outer_attrs,
                            vis: item.vis,
                            payload: AssociatedItemPayload::TypeAlias(f),
                        };
                        r.items.push(a);
                    }
                    ItemPayload::MacroCall(f) => {
                        let a = AssociatedItem {
                            outer_attrs: item.outer_attrs,
                            vis: item.vis,
                            payload: AssociatedItemPayload::MacroCall(f),
                        };
                        r.items.push(a);
                    }
                    _ => {
                        return Err(s.panic(token, "illegal trait item"));
                    }
                },
                Stmt::Let(_) | Stmt::Expr(_) | Stmt::Value(_) => {
                    return Err(s.panic(token, "illegal trait item"));
                }
            }

            s.skip_semicolons();
            if !s.has_more() {
                return Err(s.panic(&s.tokens[s.tokens.len() - 1], "expected }"));
            }
        }

        Ok(r)
    }

    fn parse_impl(&self) -> ParseResult<Impl> {
        let s = self;

        let mut r = Impl::default();

        let mut impl_trait = ImplTrait::default();
        // possible [unsafe]
        let token = s.current();
        if matches!(&token.code, T!["["]) {
            s.plusplus();
            let token = s.current();
            impl_trait.span_unsafe = token.span;
            if !matches!(&token.code, T![unsafe]) {
                return Err(s.panic(token, &format!("expected unsafe, but got {:?}", token)));
            }
            impl_trait.unsafe_ = true;
            s.plusplus();
            s.expect(T!["]"])?;
        }

        // possible TypePath
        if !matches!(&s.current().code, T![where] | T!["{"]) {
            impl_trait.type_path = s.parse_type_path()?;
            r.payload = ImplPayload::Trait(impl_trait);
        } else {
            r.payload = ImplPayload::Static;
        }

        // possible where clause
        let token = s.current();
        if matches!(&token.code, T![where]) {
            s.plusplus();
            let mut w = s.parse_where()?;
            w.span_where = token.span;
            r.where_ = Some(w);
        }

        r.span_brace_open = s.current().span;
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                r.span_brace_close = s.current().span;
                s.plusplus();
                break;
            }
            let stmt = s.parse_stmt()?;
            if let Stmt::Item(item) = stmt {
                let payload = match item.payload {
                    ItemPayload::Func(f) => AssociatedItemPayload::Func(*f),
                    ItemPayload::Const(f) => AssociatedItemPayload::Const(f),
                    ItemPayload::TypeAlias(f) => AssociatedItemPayload::TypeAlias(f),
                    ItemPayload::MacroCall(f) => AssociatedItemPayload::MacroCall(f),
                    _ => {
                        return Err(s.panic(&token, "illegal impl item"));
                    }
                };
                r.items.push(AssociatedItem {
                    outer_attrs: item.outer_attrs,
                    vis: item.vis,
                    payload,
                });
            } else {
                return Err(s.panic(&token, "illegal impl item"));
            }

            s.skip_semicolons();
            if !s.has_more() {
                return Err(s.panic(&token, "expected }"));
            }
        }

        Ok(r)
    }

    fn parse_macro(&self) -> ParseResult<Macro> {
        let s = self;

        let mut r = Macro::default();

        r.span_brace_open = s.current().span;
        s.expect(T!["{"])?;

        let mut brace_count = 1;
        while s.has_more() {
            let token = s.current();
            s.plusplus();
            match &token.code {
                T!["}"] => {
                    brace_count -= 1;
                    if brace_count == 0 {
                        r.span_brace_close = token.span;
                        break;
                    }
                    r.tokens.push(token.clone());
                }
                T!["{"] => {
                    brace_count += 1;
                    r.tokens.push(token.clone());
                }
                T![;] => {
                    // don't add semicolons in macro definitions
                    if !token.is_added_semicolon() {
                        r.tokens.push(token.clone());
                    }
                }
                _ => {
                    r.tokens.push(token.clone());
                }
            }
        }

        Ok(r)
    }

    // after ,,
    fn parse_macro_call(&self, path: SimplePath, span: Span) -> ParseResult<MacroCall> {
        let s = self;

        let mut r = MacroCall::default();
        r.path = path;
        r.span_macro_call = span;

        let token = s.current();
        s.plusplus();

        match &token.code {
            T!["("] => {
                r.span_delim_open = token.span;
                // (expr, expr, ...)
                let (exprs, span_paren_close) = parse_exprs(s)?;
                r.body = MacroCallBody::Expr(exprs);
                r.span_delim_close = span_paren_close;
            }
            T!["["] => {
                // [stmt] [p] [s]
                let mut delim = 0;
                let mut is_stmt = false;
                let token = s.current();
                let TokenCode::Identifier(identifier) = &token.code else {
                    return Err(s.panic(
                        token,
                        &format!("expected stmt, p, or s, but got {:?}", token),
                    ));
                };
                match identifier.id.as_ref() {
                    "stmt" => {
                        is_stmt = true;
                    }
                    "p" => {
                        delim = 1;
                    }
                    "s" => {
                        delim = 2;
                    }
                    _ => {
                        return Err(s.panic(
                            token,
                            &format!("expected stmt, p, or s, but got {:?}", identifier),
                        ));
                    }
                }
                s.plusplus();
                s.expect(T!["]"])?;
                let token = s.current();
                s.expect(T!["{"])?;
                r.span_delim_open = token.span;

                if is_stmt {
                    s.minusminus();
                    let block = s.parse_block_expr()?;
                    // TODO
                    // r.span_delim_close = block.span_brace_close;
                    r.body = MacroCallBody::Stmt(block.stmts);
                } else {
                    r.delim = delim;
                    // { tokens }
                    let tokens = parse_tokens(s)?;
                    s.minusminus();
                    r.span_delim_close = s.current().span;
                    s.plusplus();
                    r.body = MacroCallBody::Token(tokens);
                }
            }
            T!["{"] => {
                r.span_delim_open = token.span;
                // { tokens }
                let tokens = parse_tokens(s)?;
                s.minusminus();
                r.span_delim_close = s.current().span;
                s.plusplus();
                r.body = MacroCallBody::Token(tokens);
            }
            _ => {
                return Err(s.panic(
                    token,
                    &format!("expected ( or [ or {{, but got {:?}", token),
                ));
            }
        }

        return Ok(r);

        fn parse_exprs(s: &Parse) -> ParseResult<(Vec<Expr>, Span)> {
            let mut r = Vec::new();
            let mut span_paren_close = Span::default();

            let mut expect_expr = true;
            while s.has_more() {
                let token = s.current();
                match &token.code {
                    T![")"] => {
                        span_paren_close = token.span;
                        s.plusplus();
                        break;
                    }
                    T![,] | T![;] => {
                        if expect_expr {
                            return Err(
                                s.panic(token, &format!("expected expr but got {:?}", token))
                            );
                        }
                        s.plusplus();
                        expect_expr = true;
                    }
                    _ => {
                        if !expect_expr {
                            return Err(
                                s.panic(token, &format!("expected ) or , but got {:?}", token))
                            );
                        }
                        r.push(s.parse_expr()?);
                        expect_expr = false;
                    }
                }
            }

            Ok((r, span_paren_close))
        }

        fn parse_tokens(s: &Parse) -> ParseResult<Vec<Token>> {
            let mut r = Vec::new();

            let mut count = 1;
            while s.has_more() {
                let token = s.current();
                r.push(token.clone());
                s.plusplus();
                match &token.code {
                    T!["}"] => {
                        count -= 1;
                        if count == 0 {
                            r.pop();
                            break;
                        }
                    }
                    T!["{"] => {
                        count += 1;
                    }
                    T![;] => {
                        // don't add semicolons in raw macro calls
                        if token.is_added_semicolon() {
                            r.pop();
                        }
                    }
                    _ => {}
                }
            }

            Ok(r)
        }
    }

    // --------------------------------------------------------------------------------
    // stmts

    // after continue
    fn parse_continue(&self) -> ParseResult<ExprContinue> {
        let s = self;

        let mut r = ExprContinue::default();

        let token = s.current();
        if matches!(&token.code, T![;]) {
            return Ok(r);
        }
        if let TokenCode::Lifetime(_) = &token.code {
            r.label = Some(token.clone());
            r.span_label = Some(token.span);
            s.plusplus();
        } else {
            return Err(s.panic(token, &format!("expected lifetime, but got {:?}", token)));
        }

        Ok(r)
    }

    // after break
    fn parse_break(&self) -> ParseResult<ExprBreak> {
        let s = self;

        let mut r = ExprBreak::default();

        let token = s.current();
        if matches!(&token.code, T![;]) {
            return Ok(r);
        }

        // parse label
        if let TokenCode::Lifetime(_) = &token.code {
            s.plusplus();
            r.label = Some(token.clone());
            r.span_label = Some(token.span);
        }

        let token = s.current();
        if matches!(&token.code, T![;]) {
            return Ok(r);
        }

        // parse expr
        r.expr = Some(s.parse_expr()?);

        Ok(r)
    }

    // after ret
    fn parse_ret(&self) -> ParseResult<ExprReturn> {
        let s = self;

        let mut r = ExprReturn::default();

        let token = s.current();
        if matches!(&token.code, T![;]) {
            return Ok(r);
        }

        // parse expr
        r.expr = Some(s.parse_expr()?);

        Ok(r)
    }

    // after async
    fn parse_async(&self) -> ParseResult<ExprAsyncBlock> {
        let s = self;

        let mut r = ExprAsyncBlock::default();

        let token = s.current();
        if matches!(&token.code, T!["["]) {
            s.plusplus();
            r.span_move = Some(s.current().span);
            s.expect(T![move])?;
            s.expect(T!["]"])?;
            r.move_ = true;
        }

        r.block = s.parse_block_expr()?;

        Ok(r)
    }

    // after for
    fn parse_for(&self) -> ParseResult<ExprLoop> {
        let s = self;

        let mut r = ExprLoop::default();

        // for {
        let token = s.current();
        if matches!(&token.code, T!["{"]) {
            r.payload = ExprLoopPayload::Loop;
            r.body = s.parse_block_expr()?;
            return Ok(r);
        }

        // used to indicate it's a for in expression
        let fake_in_token = Token{
            code: TokenCode::Keyword(Keyword::In),
            span: Span::default(),
            tokens_before: Vec::new(),
            u: String::new(),
        };

        // get the tokens from after for to the first opening brace({) or semicolon(;),
        // skipping pairs(parens, etc.) in the process
        let mut first_special_token: Option<&Token> = None;
        if matches!(&token.code, T![:]) {
            first_special_token = Some(&fake_in_token);
            s.plusplus();
        } else {
            let mut start = s.index.get();
            while start < s.tokens.len() {
                let token = &s.tokens[start];
                start += 1;
                match &token.code {
                    T!["{"] => break,

                    T![;] if s.magic => break,

                    TokenCode::Pair(_) => {
                        start = s.skip_pair(start, token)?;
                    }
                    T![->] | T![in] => {
                        first_special_token = Some(token);
                        break;
                    }

                    _ => (),
                }
            }
        }

        if let Some(special_token) = first_special_token {
            match &special_token.code {
                // for Expr -> Patterns {
                T![->] => {
                    let conditions = s.parse_conditions()?;
                    r.payload = ExprLoopPayload::While(ExprLoopWhile { conditions });
                }
                // for Pattern in Expr {
                // for identifter, identifier in Expr {
                T![in] => {
                    let pattern = s.parse_pattern()?;
                    let token = s.current();
                    if !matches!(&token.code, T![in]) {
                        return Err(s.panic(
                            s.current(),
                            &format!("expected 'in' but got '{:?}'", special_token),
                        ));
                    }
                    s.plusplus();

                    s.stop_on_brace_open.set(true);
                    let mut in_expr = s.parse_expr()?;
                    s.stop_on_brace_open.set(false);

                    if let ExprPayload::WithoutBlock(a) = in_expr.payload {
                        // unwrap outer paren in for expression
                        in_expr = if let ExprWithoutBlock::Grouped(b) = *a {
                            if b.inner_attrs.items.is_empty() {
                                Expr {
                                    outer_attrs: in_expr.outer_attrs,
                                    payload: b.expr.payload,
                                }
                            } else {
                                Expr {
                                    outer_attrs: in_expr.outer_attrs,
                                    payload: ExprPayload::WithoutBlock(Box::new(
                                        ExprWithoutBlock::Grouped(b),
                                    )),
                                }
                            }
                        } else {
                            Expr {
                                outer_attrs: in_expr.outer_attrs,
                                payload: ExprPayload::WithoutBlock(a),
                            }
                        };
                    }

                    r.payload = ExprLoopPayload::In(ExprLoopIn {
                        pattern,
                        in_expr,
                        span_in: token.span,
                    });
                }
                _ => {
                    return Err(s.panic(
                        s.current(),
                        &format!("illegal special token {:?}", special_token),
                    ));
                }
            }
        } else {
            // for Expr {
            let conditions = s.parse_conditions()?;
            r.payload = ExprLoopPayload::While(ExprLoopWhile { conditions });
        }

        // for auto completion
        if s.magic && matches!(&s.current().code, T![;]) {
            return Ok(r);
        }

        r.body = s.parse_block_expr()?;

        Ok(r)
    }

    fn parse_conditions(&self) -> ParseResult<Conditions> {
        let s = self;

        let mut r = Vec::new();
        loop {
            let outer_attrs = s.parse_outer_attrs()?;

            s.stop_on_brace_open.set(true);
            let expr = s.parse_expr()?;
            s.stop_on_brace_open.set(false);

            if !matches!(&s.current().code, T![->]) {
                r.push(LetChainCondition::Expr(expr));
            } else {
                s.plusplus();
                let pattern = s.parse_match_arm_patterns_stop()?;
                let a = LetChainConditionItem {
                    outer_attrs,
                    pattern,
                    scrutinee: expr,
                };
                r.push(LetChainCondition::Condition(a));
            }

            let token = s.current();
            match &token.code {
                T![,] => {
                    s.plusplus();
                    continue;
                }
                // : for match arm
                T!["{"] | T![:] => {
                    break;
                }
                _ => {
                    return Err(
                        s.panic(token, &format!("expected , or {{ or : but got {:?}", token))
                    );
                }
            }
        }

        if r.len() == 1 && matches!(r[0], LetChainCondition::Expr(_)) {
            let a = mem::take(&mut r[0]);
            if let LetChainCondition::Expr(b) = a {
                return Ok(Conditions::Expr(b));
            }
        }
        return Ok(Conditions::LetChain(r));
    }

    // after if
    fn parse_if(&self) -> ParseResult<ExprIf> {
        let s = self;

        let mut r = ExprIf::default();

        r.conditions = s.parse_conditions()?;

        // for auto completion
        if s.magic && matches!(&s.current().code, T![;]) {
            return Ok(r);
        }
        r.body = s.parse_block_expr()?;

        let token = s.current();
        if let T![else] = &token.code {
            r.span_else = Some(token.span);
            s.plusplus();
            let token = s.current();
            s.plusplus();
            match &token.code {
                T![if] => {
                    let mut a = s.parse_if()?;
                    a.span_if = token.span;
                    r.else_ = Some(ExprIfElse::If(Box::new(a)));
                }
                T!["{"] => {
                    s.minusminus();
                    let a = s.parse_block_expr()?;
                    r.else_ = Some(ExprIfElse::BlockExpr(a));
                }
                _ => {
                    return Err(s.panic(token, &format!("expected if or {{ but got {:?}", token)));
                }
            }
        }

        Ok(r)
    }

    // after match
    fn parse_match(&self) -> ParseResult<ExprMatch> {
        let s = self;

        let mut r = ExprMatch::default();

        s.stop_on_brace_open.set(true);
        r.expr = s.parse_expr()?;
        s.stop_on_brace_open.set(false);

        // for auto completion
        if s.magic && matches!(&s.current().code, T![;]) {
            return Ok(r);
        }

        r.span_brace_open = s.current().span;
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;

        while s.has_more() {
            let token = s.current();
            match &token.code {
                T!["}"] => {
                    r.span_brace_close = token.span;
                    s.plusplus();
                    break;
                }
                _ => {
                    r.arms.push(parse_match_arm(s)?);
                }
            }
        }

        return Ok(r);

        fn parse_match_arm(s: &Parse) -> ParseResult<ExprMatchItem> {
            let mut r = ExprMatchItem::default();

            r.arm.outer_attrs = s.parse_outer_attrs()?;

            // patterns
            r.arm.pattern = s.parse_pattern()?;

            // guard
            let token = s.current();
            if matches!(&token.code, T![if]) {
                s.plusplus();
                r.arm.guard = Some(ExprMatchArmGuard {
                    conditions: s.parse_conditions()?,
                    span_if: token.span,
                });
            }

            // for auto completion
            if s.magic && matches!(&s.current().code, T![;]) {
                s.plusplus();
                return Ok(r);
            }
            s.expect(T![:])?;

            // TODO
            // expr
            if matches!(&s.current().code, T!["}"] | T![:]) {
                // add block expr
                let block = BlockExpr::default();
                r.expr = Expr {
                    outer_attrs: Attrs::default(),
                    payload: ExprPayload::WithBlock(Box::new(ExprWithBlock::BlockExpr(block))),
                }
            } else {
                r.expr = s.parse_expr()?;
                s.skip_semicolons();
            }

            Ok(r)
        }
    }

    // NAME mut? Type? := [;|Expr]?
    fn parse_let(&self) -> ParseResult<StmtLet> {
        let s = self;

        let token = s.current();
        let TokenCode::Identifier(identifier) = &token.code else {
            return Err(s.panic(token, &format!("expected identifier, but got {:?}", token)));
        };
        s.plusplus();

        let token = s.current();
        let mut_ = if matches!(token.code, T![mut]) {
            s.plusplus();
            true
        } else {
            false
        };

        let mut a = StmtLet::default();
        a.pattern = Pattern {
            items: vec![PatternNoTopAlt::WithoutRange(
                PatternWithoutRange::Identifier(PatternIdentifier {
                    mut_,
                    ref_: false,
                    name: identifier.clone(),
                    pattern: None,

                    span_ref: None,
                    span_mut: Some(token.span),
                    span_at: None,
                }),
            )],
        };

        let token = s.current();
        if !matches!(token.code, T![:=]) {
            a.type_ = Some(s.parse_type()?);
        }
        a.span_eq = token.span;
        s.expect(T![:=])?;

        let token = s.current();
        if matches!(token.code, T![;]) {
            return Ok(a);
        }
        a.expr = Some(s.parse_expr()?);
        s.expect(T![;])?;
        Ok(a)
    }

    // --------------------------------------------------------------------------------
    // exprs

    fn parse_expr(&self) -> ParseResult<Expr> {
        let s = self;

        let mut r = Expr::default();
        r.outer_attrs = s.parse_outer_attrs()?;

        let mut list: Vec<ExprOp> = Vec::new();

        let expr = s.parse_binary_expr()?;
        list.push(ExprOp::Expr(expr));

        while s.has_more() {
            let token = s.current();
            match &token.code {
                // Expr op Expr
                T![+]
                | T![~]
                | T![*]
                | T![/]
                | T![%]
                | T![&]
                | T![|]
                | T![^]
                | T![<<]
                | T![>>]
                | T![==]
                | T![!=]
                | T![>]
                | T![<]
                | T![>=]
                | T![<=]
                | T![&&]
                | T![||]
                | T!["``"]
                | T!["``="] => {
                    s.plusplus();
                    list.push(ExprOp::Op(token.clone()));
                    let expr = s.parse_binary_expr()?;
                    list.push(ExprOp::Expr(expr));
                }

                _ => {
                    r.payload = to_op_expr(s, list)?.payload;
                    break;
                }
            }
        }

        return Ok(r);

        #[derive(Debug)]
        enum ExprOp {
            Expr(Expr),
            Op(Token),
        }
        impl Default for ExprOp {
            fn default() -> Self {
                ExprOp::Op(Token::default())
            }
        }

        /*
        translate expr list to an expr tree
        see http://csis.pace.edu/~wolf/CS122/infix-postfix.htm

        1. Print operands as they arrive.

        2. If the stack is empty or contains a left parenthesis on top, push the incoming operator onto the stack.

        3. If the incoming symbol is a left parenthesis, push it on the stack.

        4. If the incoming symbol is a right parenthesis, pop the stack and print the operators
        until you see a left parenthesis. Discard the pair of parentheses.

        5. If the incoming symbol has higher precedence than the top of the stack, push it on the stack.

        6. If the incoming symbol has equal precedence with the top of the stack, use association.
        If the association is left to right, pop and print the top of the stack and then push the incoming operator.
        If the association is right to left, push the incoming operator.

        7. If the incoming symbol has lower precedence than the symbol on the top of the stack, pop the stack
        and print the top operator. Then test the incoming operator against the new top of stack.

        8. At the end of the expression, pop and print all operators on the stack. (No parentheses should remain.)
        */
        fn to_op_expr(s: &Parse, list: Vec<ExprOp>) -> ParseResult<Expr> {
            let mut list = list;

            /* note
                - the first item must be an expr
                - list.len() must be odd
            */

            // special case when list has only one element
            if list.len() == 1 {
                let expr_op = mem::take(&mut list[0]);
                if let ExprOp::Expr(expr) = expr_op {
                    return Ok(Expr {
                        outer_attrs: Attrs::default(),
                        payload: expr.payload,
                    });
                }
            }

            let mut op_stack: Vec<Token> = Vec::new();
            let mut r_list: Vec<ExprOp> = Vec::new();

            // from infix to postfix
            for item in list {
                match item {
                    ExprOp::Expr(expr) => {
                        r_list.push(ExprOp::Expr(expr));
                    }
                    ExprOp::Op(token) => loop {
                        if op_stack.is_empty() {
                            op_stack.push(token);
                            break;
                        }
                        let top = op_stack.last().unwrap();
                        let top_precedence = *s.op_map.get(&top.code).unwrap();
                        let current_precedence = *s.op_map.get(&token.code).unwrap();
                        if current_precedence > top_precedence {
                            op_stack.push(token);
                            break;
                        } else if current_precedence < top_precedence {
                            r_list.push(ExprOp::Op(op_stack.pop().unwrap()));
                        } else {
                            r_list.push(ExprOp::Op(op_stack.pop().unwrap()));
                            op_stack.push(token);
                            break;
                        }
                    },
                }
            }
            for op in op_stack.into_iter().rev().collect::<Vec<Token>>() {
                r_list.push(ExprOp::Op(op));
            }

            // build expr
            // algorithm:
            //      find next operator
            //      search backward to find the first(rhs) and second(lhs) expr
            //      combine them to make a new expr
            //      store the new expr in lhs' position
            //      iterate...
            //      in the end, the first element in the list is the wanted expr
            for i in 0..r_list.len() {
                let item = mem::take(&mut r_list[i]);
                if let ExprOp::Op(op) = item {
                    let mut right_option = None;
                    let mut left_option = None;
                    let mut index = i;
                    loop {
                        index -= 1;
                        if matches!(r_list[index], ExprOp::Expr(_)) {
                            let e = mem::take(&mut r_list[index]);
                            if let ExprOp::Expr(expr) = e {
                                if right_option.is_none() {
                                    right_option = Some(expr);
                                } else {
                                    left_option = Some(expr);
                                    break;
                                }
                            }
                        }
                        if index == 0 {
                            break;
                        }
                    }

                    match op.code {
                        T![+]
                        | T![~]
                        | T![*]
                        | T![/]
                        | T![%]
                        | T![&]
                        | T![|]
                        | T![^]
                        | T![<<]
                        | T![>>] => {
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Operator(ExprOperator::ArithmeticOrLogical(
                                        ExprArithmeticOrLogical {
                                            op: op.code,
                                            left: left_option.unwrap(),
                                            right: right_option.unwrap(),
                                            span_op: op.span,
                                        },
                                    )),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }
                        T![==] | T![!=] | T![>] | T![<] | T![>=] | T![<=] => {
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Operator(ExprOperator::Comparison(
                                        ExprComparison {
                                            op: op.code,
                                            left: left_option.unwrap(),
                                            right: right_option.unwrap(),
                                            span_op: op.span,
                                        },
                                    )),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }
                        T![||] | T![&&] => {
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Operator(ExprOperator::LazyBoolean(
                                        ExprLazyBoolean {
                                            op: op.code,
                                            left: left_option.unwrap(),
                                            right: right_option.unwrap(),
                                            span_op: op.span,
                                        },
                                    )),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }

                        T!["``"] => {
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Range(ExprRange {
                                        start: left_option,
                                        inclusive: false,
                                        end: right_option,
                                        span: op.span,
                                    }),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }
                        T!["``="] => {
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Range(ExprRange {
                                        start: left_option,
                                        inclusive: true,
                                        end: right_option,
                                        span: op.span,
                                    }),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }
                        _ => {
                            unreachable!("not a valid op");
                        }
                    }
                } else {
                    r_list[i] = item;
                }
            }

            let item = mem::take(&mut r_list[0]);
            if let ExprOp::Expr(expr) = item {
                Ok(expr)
            } else {
                unreachable!("not an expr")
            }
        }
    }

    fn parse_binary_expr(&self) -> ParseResult<Expr> {
        let s = self;

        let mut r = Expr::default();

        let mut expr = s.parse_sub_expr()?;

        while s.has_more() {
            let token = s.current();
            s.plusplus();
            match &token.code {
                // Expr = Expr
                T![=] => {
                    let right = s.parse_expr()?;
                    r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                        ExprOperator::Assignment(ExprAssignment {
                            left: expr,
                            right,
                            span_op: token.span,
                        }),
                    )));
                    break;
                }
                // compound assignment
                T![+=]
                | T![~=]
                | T![*=]
                | T![/=]
                | T![%=]
                | T![&=]
                | T![|=]
                | T![^=]
                | T![<<=]
                | T![>>=] => {
                    let right = s.parse_expr()?;
                    r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                        ExprOperator::CompoundAssignment(ExprCompoundAssignment {
                            op: token.code.clone(),
                            left: expr,
                            right,
                            span_op: token.span,
                        }),
                    )));
                    break;
                }

                // Expr(CallParams?)
                T!["("] => {
                    let span_paren_open = token.span;
                    let mut params = Vec::new();
                    while s.has_more() {
                        let token = s.current();
                        match &token.code {
                            T![")"] => {
                                s.plusplus();
                                expr = Expr {
                                    outer_attrs: Attrs::default(),
                                    payload: ExprPayload::WithoutBlock(Box::new(
                                        ExprWithoutBlock::Call(ExprCall {
                                            left: expr,
                                            params,
                                            span_paren_open,
                                            span_paren_close: token.span,
                                        }),
                                    )),
                                };
                                break;
                            }
                            T![,] | T![;] => {
                                s.plusplus();
                            }
                            _ => {
                                params.push(s.parse_expr()?);
                            }
                        }
                    }
                }

                // Expr ``
                T!["``"] => {
                    if s.at_range_end(&s.current().code) {
                        expr = Expr {
                            outer_attrs: Attrs::default(),
                            payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Range(
                                ExprRange {
                                    start: Some(expr),
                                    inclusive: false,
                                    end: None,
                                    span: token.span,
                                },
                            ))),
                        };
                    } else {
                        s.minusminus();
                        r = expr;
                        break;
                    }
                }

                // Expr{{Expr}}
                T!["{{"] => {
                    let span_double_brace_open = token.span;
                    let right = s.parse_expr()?;
                    let span_double_brace_close = s.current().span;
                    s.expect(T!["}}"])?;
                    expr = Expr {
                        outer_attrs: Attrs::default(),
                        payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Index(
                            ExprIndex {
                                left: expr,
                                right,
                                span_double_brace_open,
                                span_double_brace_close,
                            },
                        ))),
                    };
                }
                // Expr?
                T![?] => {
                    expr = Expr {
                        outer_attrs: Attrs::default(),
                        payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                            ExprOperator::ErrorPropagation(ExprErrorPropagation {
                                expr,
                                span: token.span,
                            }),
                        ))),
                    };
                }
                // Expr++
                T![++] => {
                    expr = Expr {
                        outer_attrs: Attrs::default(),
                        payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                            ExprOperator::CompoundAssignment(ExprCompoundAssignment {
                                op: T![+=],
                                left: expr,
                                right: Expr {
                                    outer_attrs: Attrs::default(),
                                    payload: ExprPayload::WithoutBlock(Box::new(
                                        ExprWithoutBlock::Literal(ExprLiteral(Literal {
                                            payload: LiteralPayload::Int("1".to_string()),
                                            span: token.span,
                                            prefix: None,
                                            suffix: None,
                                        })),
                                    )),
                                },
                                span_op: token.span,
                            }),
                        ))),
                    };
                }
                // Expr~~
                T![~~] => {
                    expr = Expr {
                        outer_attrs: Attrs::default(),
                        payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                            ExprOperator::CompoundAssignment(ExprCompoundAssignment {
                                op: T![~=],
                                left: expr,
                                right: Expr {
                                    outer_attrs: Attrs::default(),
                                    payload: ExprPayload::WithoutBlock(Box::new(
                                        ExprWithoutBlock::Literal(ExprLiteral(Literal {
                                            payload: LiteralPayload::Int("1".to_string()),
                                            span: token.span,
                                            prefix: None,
                                            suffix: None,
                                        })),
                                    )),
                                },
                                span_op: token.span,
                            }),
                        ))),
                    };
                }
                // Expr as Type
                T![as] => {
                    let type_ = s.parse_type()?;
                    let type_no_bounds = if let TypePayload::TypeNoBounds(a) = type_.payload {
                        *a
                    } else {
                        unreachable!("not a TypeNoBounds")
                    };
                    expr = Expr {
                        outer_attrs: Attrs::default(),
                        payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                            ExprOperator::TypeCast(Box::new(ExprTypeCast {
                                expr,
                                type_no_bounds,
                                span_as: token.span,
                            })),
                        ))),
                    };
                }

                // Expr.await, Expr.TupleIndex, Expr.PathExprSegment(CallParams?)
                T![.] => {
                    let span_dot = token.span;
                    let token = s.current();
                    match &token.code {
                        T![await] => {
                            expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Await(ExprAwait {
                                        expr,
                                        span: token.span,
                                    }),
                                )),
                            };
                            s.plusplus();
                        }
                        TokenCode::Literal(Literal {
                            payload: LiteralPayload::Int(a),
                            ..
                        }) => {
                            expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::TupleIndexing(ExprTupleIndexing {
                                        left: expr,
                                        index: a.clone(),
                                        span_dot,
                                        span_index: token.span,
                                    }),
                                )),
                            };
                            s.plusplus();
                        }
                        T![super] | T![self] | T![Self] | T![crate] | TokenCode::Identifier(_) => {
                            let segment = s.parse_path_expr_segment()?;
                            if matches!(s.current().code, T!["("]) {
                                let mut a = parse_method_call_expr(s, segment)?;
                                a.left = expr;
                                a.span_dot = span_dot;
                                expr = Expr {
                                    outer_attrs: Attrs::default(),
                                    payload: ExprPayload::WithoutBlock(Box::new(
                                        ExprWithoutBlock::MethodCall(a),
                                    )),
                                };
                            } else {
                                let name = match segment.name {
                                    PathIdentSegment::Identifier(a) => a,
                                    _ => {
                                        return Err(s.panic(
                                            token,
                                            &format!("expected identifier but got {:?}", token),
                                        ));
                                    }
                                };
                                expr = Expr {
                                    outer_attrs: Attrs::default(),
                                    payload: ExprPayload::WithoutBlock(Box::new(
                                        ExprWithoutBlock::Field(ExprField {
                                            left: expr,
                                            name,
                                            span_dot,
                                        }),
                                    )),
                                };
                            }
                        }
                        _ => {
                            return Err(
                                s.panic(&token, &format!("illegal token {:?} after .", token))
                            );
                        }
                    }
                }

                // Expr non-expr-token
                _ => {
                    s.minusminus();
                    r = expr;
                    break;
                }
            }
        }

        return Ok(r);

        fn parse_method_call_expr(
            s: &Parse,
            segment: PathExprSegment,
        ) -> ParseResult<ExprMethodCall> {
            let mut r = ExprMethodCall::default();
            r.path_expr_segment = segment;
            r.span_paren_open = s.current().span;
            s.plusplus();

            let mut expect_expr = true;
            while s.has_more() {
                let token = s.current();
                match &token.code {
                    T![")"] => {
                        r.span_paren_close = token.span;
                        s.plusplus();
                        break;
                    }
                    T![,] | T![;] => {
                        if expect_expr {
                            return Err(
                                s.panic(&token, &format!("expected expr but got {:?}", token))
                            );
                        }
                        expect_expr = true;
                        s.plusplus();
                    }
                    _ => {
                        if !expect_expr {
                            return Err(s.panic(token, "not expected expr"));
                        }
                        expect_expr = false;
                        r.params.push(s.parse_expr()?);
                    }
                }
            }

            Ok(r)
        }
    }

    fn parse_sub_expr(&self) -> ParseResult<Expr> {
        let s = self;

        let mut r = Expr::default();
        r.outer_attrs = s.parse_outer_attrs()?;

        let token = s.current();
        s.plusplus();
        match &token.code {
            // literal
            TokenCode::Literal(literal) => {
                r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Literal(
                    ExprLiteral(literal.clone()),
                )));
            }

            // lifetime
            TokenCode::Lifetime(_) => {
                let t_label = token.clone();
                let t_colon = s.current().clone();
                s.expect(T![:])?;

                if matches!(s.current().code, T![for]) {
                    let span_for = s.current().span;
                    s.plusplus();
                    let mut for_ = s.parse_for()?;
                    for_.span_for = span_for;
                    for_.t_label = Some((t_label, t_colon));
                    for_.label = Some(token.clone());
                    r.payload =
                        ExprPayload::WithBlock(Box::new(ExprWithBlock::Loop(Box::new(for_))));
                } else {
                    let mut a = s.parse_block_expr()?;
                    a.label = Some((t_label, t_colon));
                    r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::BlockExpr(a)));
                }
            }

            // grouped or tuple
            T!["("] => {
                let span_paren_open = token.span;
                let inner_attrs = s.parse_inner_attrs()?;

                let mut list: Vec<Expr> = Vec::new();
                let mut is_tuple = false;
                while s.has_more() {
                    let token = s.current();
                    match &token.code {
                        T![")"] => {
                            let span_paren_close = token.span;
                            s.plusplus();
                            if list.len() == 1 && !is_tuple {
                                let expr = mem::take(&mut list[0]);
                                r.payload = ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Grouped(ExprGrouped {
                                        inner_attrs,
                                        expr,
                                        span_paren_open,
                                        span_paren_close,
                                    }),
                                ));
                            } else {
                                r.payload = ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Tuple(ExprTuple {
                                        inner_attrs,
                                        items: list,
                                        span_paren_open,
                                        span_paren_close,
                                    }),
                                ));
                            }
                            return Ok(r);
                        }
                        T![,] | T![;] => {
                            is_tuple = true;
                            s.plusplus();
                        }
                        _ => {
                            list.push(s.parse_expr()?);
                        }
                    }
                }
            }
            // array
            T!["{{"] => {
                let span_double_brace_open = token.span;
                let inner_attrs = s.parse_inner_attrs()?;
                // {{}}
                let token = s.current();
                if matches!(&token.code, T!["}}"]) {
                    s.plusplus();
                    r.payload =
                        ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Array(ExprArray {
                            inner_attrs,
                            payload: ExprArrayPayload::Many(ExprArrayMany(Vec::new())),
                            span_double_brace_open,
                            span_double_brace_close: token.span,
                        })));
                    return Ok(r);
                }

                let first_expr = s.parse_expr()?;
                let token = s.current();
                if matches!(&token.code, T![:]) {
                    // {{ Expr: Expr }}
                    let span_semicolon = token.span;
                    s.plusplus();
                    let len = s.parse_expr()?;
                    let span_double_brace_close = s.current().span;
                    s.expect(T!["}}"])?;
                    r.payload =
                        ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Array(ExprArray {
                            inner_attrs,
                            payload: ExprArrayPayload::One(ExprArrayOne {
                                value: first_expr,
                                len,
                                span_semicolon,
                            }),
                            span_double_brace_open,
                            span_double_brace_close,
                        })));
                    return Ok(r);
                }

                // {{ Expr([,;] Expr)* }}
                let mut items: Vec<Expr> = vec![first_expr];
                let mut expect_expr = false;
                while s.has_more() {
                    let token = s.current();
                    match &token.code {
                        T!["}}"] => {
                            s.plusplus();
                            r.payload = ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Array(ExprArray {
                                    inner_attrs,
                                    payload: ExprArrayPayload::Many(ExprArrayMany(items)),
                                    span_double_brace_open,
                                    span_double_brace_close: token.span,
                                }),
                            ));
                            return Ok(r);
                        }
                        T![,] | T![;] => {
                            if expect_expr {
                                return Err(
                                    s.panic(&token, &format!("expected expr but got {:?}", token))
                                );
                            }
                            expect_expr = true;
                            s.plusplus();
                        }
                        _ => {
                            if !expect_expr {
                                return Err(s.panic(
                                    &token,
                                    &format!("expected , ; or }} but got {:?}", token),
                                ));
                            }
                            expect_expr = false;
                            items.push(s.parse_expr()?);
                        }
                    }
                }
            }

            // qualified path in expression
            T!["["] => {
                let mut type_ = s.parse_qualifyed_path_type()?;
                type_.span_square_open = token.span;
                s.expect(T![..])?;
                let items = s.parse_path_expr_segments()?;
                r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Path(
                    ExprPath::QualifiedPath(QualifiedPathInExpr { type_, items }),
                )));
            }

            // path in expression or macro call(simple path followed by ,,)
            T![super] | T![self] | T![Self] | T![crate] | TokenCode::Identifier(_) => {
                s.minusminus();
                let items = s.parse_path_expr_segments()?;
                let token = s.current();
                if !matches!(&token.code, T![,,]) {
                    let path = PathInExpr { items };
                    match &token.code {
                        // struct expr
                        T!["{"] => {
                            let span_brace_open = token.span;
                            if s.stop_on_brace_open.get() {
                                r.payload = ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Path(ExprPath::Path(path)),
                                ));
                                s.stop_on_brace_open.set(false);
                                return Ok(r);
                            }

                            s.plusplus();
                            let mut a = ExprStructStruct::default();
                            a.path = path;
                            a.inner_attrs = s.parse_inner_attrs()?;
                            a.span_brace_open = span_brace_open;

                            let mut expect_field = true;
                            while s.has_more() {
                                let token = s.current();
                                match &token.code {
                                    T!["}"] => {
                                        a.span_brace_close = token.span;
                                        s.plusplus();
                                        break;
                                    }
                                    T![,] | T![;] => {
                                        if expect_field {
                                            return Err(s.panic(
                                                token,
                                                &format!("expected field but got {:?}", token),
                                            ));
                                        }
                                        expect_field = true;
                                        s.plusplus();
                                    }
                                    _ => {
                                        if !expect_field {
                                            return Err(s.panic(
                                                token,
                                                &format!("expected , or }} but got {:?}", token),
                                            ));
                                        }
                                        expect_field = false;
                                        let field = parse_struct_expr_field(s)?;
                                        a.fields.push(field);
                                    }
                                }
                            }
                            r.payload = ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Struct(ExprStruct::Struct(a)),
                            ));
                        }
                        // struct tuple expr
                        T!["("] => {
                            s.plusplus();
                            let mut a = ExprStructTuple::default();
                            a.path = path;
                            a.inner_attrs = s.parse_inner_attrs()?;
                            a.span_paren_open = token.span;

                            let mut expect_expr = true;
                            while s.has_more() {
                                let token = s.current();
                                match &token.code {
                                    T![")"] => {
                                        a.span_paren_close = token.span;
                                        s.plusplus();
                                        break;
                                    }
                                    T![,] | T![;] => {
                                        if expect_expr {
                                            return Err(s.panic(token, "expected expr but got ,"));
                                        }
                                        expect_expr = true;
                                        s.plusplus();
                                    }
                                    _ => {
                                        if !expect_expr {
                                            return Err(s.panic(
                                                token,
                                                &format!("expected , or ) but got {:?}", token),
                                            ));
                                        }
                                        expect_expr = false;
                                        a.exprs.push(s.parse_expr()?);
                                    }
                                }
                            }
                            r.payload = ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Struct(ExprStruct::Tuple(a)),
                            ));
                        }
                        _ => {
                            r.payload = ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Path(ExprPath::Path(path)),
                            ));
                        }
                    }
                    return Ok(r);
                }

                // macro call
                let mut path = SimplePath::default();
                for item in items {
                    if !item.generic_args.items.is_empty() {
                        return Err(s.panic(token, "macro call can't contain generic params"));
                    }
                    let result = SimplePathSegment::try_from(item.name);
                    if let Ok(a) = result {
                        path.items.push(a);
                    } else {
                        return Err(s.panic(token, "macro call can't contain Self"));
                    }
                }
                s.plusplus();
                let call = s.parse_macro_call(path, token.span)?;
                r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::MacroCall(call)));
            }

            T!["{"] => {
                s.minusminus();
                let a = s.parse_block_expr()?;
                r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::BlockExpr(a)));
            }
            TokenCode::Keyword(keyword) => {
                match keyword {
                    // with block
                    Keyword::Async => {
                        let mut a = s.parse_async()?;
                        a.span_async = token.span;
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::AsyncBlock(a)));
                    }
                    Keyword::Unsafe => {
                        let span_unsafe = token.span;
                        let a = s.parse_block_expr()?;
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::UnsafeBlock(
                            ExprUnsafeBlock {
                                block: a,
                                span_unsafe,
                            },
                        )));
                    }
                    Keyword::Const => {
                        let span_const = token.span;
                        let a = s.parse_block_expr()?;
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::ConstBlock(
                            ExprConstBlock {
                                block: a,
                                span_const,
                            },
                        )));
                    }
                    Keyword::For => {
                        let mut a = s.parse_for()?;
                        a.span_for = token.span;
                        r.payload =
                            ExprPayload::WithBlock(Box::new(ExprWithBlock::Loop(Box::new(a))));
                    }
                    Keyword::If => {
                        let mut a = s.parse_if()?;
                        a.span_if = token.span;
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::If(a)));
                    }
                    Keyword::Match => {
                        let mut a = s.parse_match()?;
                        a.span_match = token.span;
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::Match(a)));
                    }

                    // without block
                    Keyword::Break => {
                        let mut a = s.parse_break()?;
                        a.span_break = token.span;
                        r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Break(a)));
                    }
                    Keyword::Continue => {
                        let mut a = s.parse_continue()?;
                        a.span_continue = token.span;
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Continue(a)));
                    }
                    Keyword::Ret => {
                        let mut a = s.parse_ret()?;
                        a.span_return = token.span;
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Return(a)));
                    }

                    _ => {
                        return Err(
                            s.panic(token, &format!("illegal keyword {:?} for expr", token))
                        );
                    }
                }
            }

            TokenCode::Op(op) => {
                match op {
                    // ``, `` Expr
                    Op::TickTick => {
                        let mut range = ExprRange::default();
                        if !s.at_range_end(&s.current().code) {
                            range.end = Some(s.parse_expr()?);
                        }
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Range(range)));
                    }
                    // ``= Expr
                    Op::TickTickEq => {
                        let mut range = ExprRange::default();
                        range.inclusive = true;
                        range.end = Some(s.parse_expr()?);
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Range(range)));
                    }

                    // unary Expr
                    Op::BitAnd => {
                        let borrow = parse_borrow(s, &token, false)?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Operator(ExprOperator::Borrow(borrow)),
                        ));
                    }
                    Op::And => {
                        let borrow = parse_borrow(s, &token, true)?;

                        let mut outer_borrow = ExprBorrow::default();
                        let mut outer_span = token.span;
                        outer_span.width = 1;
                        outer_borrow.span_ref = outer_span;
                        outer_borrow.expr = Expr {
                            outer_attrs: Attrs::default(),
                            payload: ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Operator(ExprOperator::Borrow(borrow)),
                            )),
                        };
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Operator(ExprOperator::Borrow(outer_borrow)),
                        ));
                    }
                    Op::Star => {
                        let expr = s.parse_binary_expr()?;
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                                ExprOperator::Dereference(ExprDereference {
                                    expr,
                                    span: token.span,
                                }),
                            )));
                    }
                    Op::Sub => {
                        let expr = s.parse_binary_expr()?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Operator(ExprOperator::Negation(ExprNegation {
                                not: false,
                                expr,
                                span: token.span,
                            })),
                        ));
                    }
                    Op::Not => {
                        let expr = s.parse_binary_expr()?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Operator(ExprOperator::Negation(ExprNegation {
                                not: true,
                                expr,
                                span: token.span,
                            })),
                        ));
                    }

                    Op::BitOr => {
                        s.minusminus();
                        let a = s.parse_closure_expr()?;
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Closure(a)));
                    }
                    Op::Or => {
                        s.minusminus();
                        let a = s.parse_closure_expr()?;
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Closure(a)));
                    }

                    _ => {
                        return Err(
                            s.panic(token, &format!("illegal operator {:?} for expr", token))
                        );
                    }
                }
            }

            _ => {
                return Err(s.panic(token, &format!("illegal token {:?} for expr", token)));
            }
        }

        return Ok(r);

        fn parse_borrow(s: &Parse, token: &Token, is_inner: bool) -> ParseResult<ExprBorrow> {
            let mut borrow = ExprBorrow::default();
            let mut inner_span = token.span;
            if is_inner {
                inner_span.column += 1;
                inner_span.width = 1;
            }
            borrow.span_ref = inner_span;
            let token = s.current();
            if matches!(&token.code, T!["["]) {
                s.plusplus();
                let token = s.current();
                let TokenCode::Identifier(ident) = &token.code else {
                    return Err(
                        s.panic(token, &format!("illegal token {:?} for borrow expr", token))
                    );
                };
                if ident.id != "raw" {
                    return Err(
                        s.panic(token, &format!("illegal id {:?} for borrow expr", ident.id))
                    );
                }
                borrow.span_raw = Some(token.span);
                borrow.raw = true;
                s.plusplus();
                s.expect(T!["]"])?;
            }
            let token = s.current();
            if matches!(&token.code, T![mut]) {
                borrow.span_mut = Some(token.span);
                borrow.mut_ = true;
                s.plusplus();
            }
            borrow.expr = s.parse_binary_expr()?;
            return Ok(borrow);
        }

        fn parse_struct_expr_field(s: &Parse) -> ParseResult<ExprStructField> {
            let r;

            let token = s.current();
            s.plusplus();
            match &token.code {
                TokenCode::Identifier(identifier) => {
                    if matches!(&s.current().code, T![,] | T![;] | T!["}"]) {
                        r = ExprStructField::A(ExprStructFieldA {
                            name: identifier.clone(),
                        });
                    } else {
                        s.expect(T![:])?;
                        r = ExprStructField::B(ExprStructFieldB {
                            name: identifier.clone(),
                            expr: s.parse_expr()?,
                        });
                    }
                }
                TokenCode::Literal(Literal {
                    payload: LiteralPayload::Int(name),
                    ..
                }) => {
                    s.expect(T![:])?;
                    r = ExprStructField::C(ExprStructFieldC {
                        tuple_index: name.clone(),
                        expr: s.parse_expr()?,
                        span_index: token.span,
                    });
                }
                T![...] => {
                    let token = s.current();
                    if matches!(&token.code, T!["}"] | T![,] | T![;]) {
                        r = ExprStructField::Base(None, token.span);
                    } else {
                        r = ExprStructField::Base(Some(s.parse_expr()?), token.span);
                    }
                }
                _ => {
                    return Err(s.panic(
                        token,
                        &format!(
                            "expected identifier, int literal or ... but got {:?}",
                            token
                        ),
                    ));
                }
            }

            Ok(r)
        }
    }

    // range must reside inside () or {{}}
    fn at_range_end(&self, code: &TokenCode) -> bool {
        matches!(code, T![")"] | T!["}}"])
    }

    // |a, b, c| Expr
    // || Expr
    // |:Pattern: Type|: Type BlockExpr
    // |[move] a, b, c| Expr
    // |[async] a, b, c| Expr
    // |[async, move] a, b, c| Expr
    fn parse_closure_expr(&self) -> ParseResult<ExprClosure> {
        let s = self;

        let mut r = ExprClosure::default();

        // params
        let token = s.current();
        r.span_paren_open = token.span;
        s.plusplus();
        match token.code {
            T![|] => {
                while s.has_more() {
                    let token = s.current();
                    if matches!(token.code, T!["["]) {
                        s.plusplus();
                        let token = s.current();
                        if matches!(token.code, T![async]) {
                            r.async_ = true;
                            r.span_async = Some(token.span);
                            s.plusplus();
                            if matches!(s.current().code, T![,]) {
                                s.plusplus();
                            }
                        }
                        let token = s.current();
                        if matches!(token.code, T![move]) {
                            r.move_ = true;
                            r.span_move = Some(token.span);
                            s.plusplus();
                        }
                        s.expect(T!["]"])?;
                    }

                    let token = s.current();
                    if matches!(&token.code, T![|]) {
                        r.span_paren_close = token.span;
                        s.plusplus();
                        break;
                    }

                    let mut param = FuncParamOptionalType::default();
                    param.outer_attrs = s.parse_outer_attrs()?;

                    match &token.code {
                        TokenCode::Identifier(identifier) => {
                            param.name = identifier.clone();
                            s.plusplus();

                            let token = s.current();
                            match &token.code {
                                T![|] | T![,] => {}
                                _ => {
                                    if matches!(&token.code, T![:]) {
                                        return Err(s.panic(
                                            token,
                                            &format!("don't add ':' after param name '{:?}'",
                                                param.name),
                                        ));
                                    }
                                    param.type_ = Some(s.parse_type()?);
                                }
                            }
                        }
                        T![:] => {
                            s.plusplus();
                            param.pat = Some(s.parse_pattern()?);
                            s.expect(T![:])?;
                            param.type_ = Some(s.parse_type()?);
                        }
                        _ => {
                            return Err(s.panic(
                                token,
                                &format!("expected identifier or ':' but got {:?}", token),
                            ));
                        }
                    }

                    let token = s.current();
                    s.plusplus();
                    match &token.code {
                        T![|] => {
                            r.span_paren_close = token.span;
                            r.params.push(param);
                            break;
                        }
                        T![,] => {
                            r.params.push(param);
                        }
                        _ => {
                            return Err(s.panic(
                                token,
                                &format!("expected , or ) but got {:?}", token),
                            ));
                        }
                    }
                }
            }
            T![||] => {}
            _ => {
                return Err(s.panic(token, &format!("expected | or || but got {:?}", token)));
            }
        }

        let token = s.current();
        if matches!(&token.code, T![:]) {
            s.plusplus();
            // return type
            let type_ = s.parse_type()?;
            if let TypePayload::TypeNoBounds(a) = type_.payload {
                r.return_type = Some(*a);
            } else {
                return Err(s.panic(token, "expected TypeNoBounds"));
            }
            s.expect(T!["{"])?;
            s.minusminus();
        }
        r.expr = s.parse_expr()?;

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    fn parse_type(&self) -> ParseResult<Type> {
        let s = self;

        let start = s.index.get();
        let payload = s.parse_type_payload()?;
        let end = s.index.get();
        let r = Type {
            payload,
            u_pair: UPair{start, end},
        };
        return Ok(r);
    }
    fn parse_type_payload(&self) -> ParseResult<TypePayload> {
        let s = self;

        let token = s.current();
        s.plusplus();
        match &token.code {
            // parenthesized or tuple
            T!["("] => {
                let span_paren_open = token.span;
                let mut type_list: Vec<Type> = Vec::new();
                let mut is_tuple = false;
                while s.has_more() {
                    let token = s.current();
                    match &token.code {
                        T![")"] => {
                            s.plusplus();
                            if type_list.len() == 1 && !is_tuple {
                                let a = mem::take(&mut type_list[0]);
                                let payload = TypePayload::TypeNoBounds(Box::new(
                                    TypeNoBounds::Parenthesized(TypeParenthesized {
                                        type_: a,
                                        span_paren_open,
                                        span_paren_close: token.span,
                                    }),
                                ));
                                return Ok(payload);
                            } else {
                                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::Tuple(
                                    TypeTuple {
                                        items: type_list,
                                        span_paren_open,
                                        span_paren_close: token.span,
                                    },
                                )));
                                return Ok(payload);
                            }
                        }
                        T![,] => {
                            is_tuple = true;
                            s.plusplus();
                        }
                        _ => {
                            type_list.push(s.parse_type()?);
                        }
                    }
                }
            }
            // qualified path
            T!["["] => {
                let mut qualified_path_type = s.parse_qualifyed_path_type()?;
                qualified_path_type.span_square_open = token.span;
                s.expect(T![..])?;

                let a_type_path = s.parse_type_path()?;

                let payload = TypePayload::TypeNoBounds(Box::new(
                    TypeNoBounds::QualifiedPathInType(QualifiedPathInType {
                        type_: qualified_path_type,
                        items: a_type_path.items,
                    }),
                ));
                return Ok(payload);
            }
            // array or slice
            T!["{{"] => {
                let span_double_brace_open = token.span;
                let type_ = s.parse_type()?;
                let token = s.current();
                match &token.code {
                    T![:] => {
                        let span_semicolon = token.span;
                        s.plusplus();
                        let expr = s.parse_expr()?;
                        let span_double_brace_close = s.current().span;
                        s.expect(T!["}}"])?;
                        let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::Array(
                            TypeArray {
                                type_,
                                expr,
                                span_double_brace_open,
                                span_double_brace_close,
                                span_semicolon,
                            },
                        )));
                        return Ok(payload);
                    }
                    T!["}}"] => {
                        s.plusplus();
                        let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::Slice(
                            TypeSlice {
                                type_,
                                span_double_brace_open,
                                span_double_brace_close: token.span,
                            },
                        )));
                        return Ok(payload);
                    }
                    _ => {
                        return Err(
                            s.panic(token, &format!("expected ; or }} but got {:?}", token))
                        );
                    }
                }
            }
            // reference
            T![&] | T![&&] => {
                let span_ref = token.span;
                let double_ref = matches!(&token.code, T![&&]);

                let mut lifetime = None;
                let token = s.current();
                if let TokenCode::Lifetime(_) = &token.code {
                    lifetime = Some(Lifetime {
                        name: token.clone(),
                    });
                    s.plusplus();
                }
                let mut mut_ = false;
                let mut span_mut = None;
                let token = s.current();
                if matches!(&token.code, T![mut]) {
                    mut_ = true;
                    span_mut = Some(token.span);
                    s.plusplus();
                }
                let type_ = s.parse_type()?;
                if let TypePayload::TypeNoBounds(t) = type_.payload {
                    let no_bounds = Box::new(TypeNoBounds::Reference(TypeReference {
                        lifetime,
                        mut_,
                        type_no_bounds: t,
                        span_ref,
                        span_mut,
                    }));
                    if double_ref {
                        let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::Reference(
                            TypeReference {
                                lifetime: None,
                                mut_: false,
                                type_no_bounds: no_bounds,
                                span_ref,
                                span_mut,
                            },
                        )));
                        return Ok(payload);
                    } else {
                        let payload = TypePayload::TypeNoBounds(no_bounds);
                        return Ok(payload);
                    }
                } else {
                    return Err(s.panic(token, "expected TypeNoBounds"));
                }
            }
            // raw pointer
            T![*] => {
                let span_pointer = token.span;
                let mut mut_ = false;
                let token = s.current();
                s.plusplus();
                let span_mut;
                match &token.code {
                    T![const] => {
                        span_mut = token.span;
                    }
                    T![mut] => {
                        mut_ = true;
                        span_mut = token.span;
                    }
                    _ => {
                        return Err(
                            s.panic(token, &format!("expected const or mut but got {:?}", token))
                        );
                    }
                }
                let type_ = s.parse_type()?;
                if let TypePayload::TypeNoBounds(t) = type_.payload {
                    let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::RawPointer(
                        TypeRawPointer {
                            mut_,
                            type_no_bounds: t,
                            span_pointer,
                            span_mut,
                        },
                    )));
                    return Ok(payload);
                } else {
                    return Err(s.panic(token, "expected TypeNoBounds"));
                }
            }
            // never
            T![!] => {
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::Never(
                    token.span,
                )));
                return Ok(payload);
            }
            // impl trait or impl trait one bound
            T![impl] => {
                let next_token = s.current();
                if let TokenCode::Lifetime(_) = &next_token.code {
                    let bounds = s.parse_type_param_bounds()?;
                    let payload = TypePayload::ImplTrait(bounds, token.span);
                    return Ok(payload);
                }
                let bound = s.parse_trait_bound()?;
                if !matches!(s.current().code, T![+]) {
                    let payload = TypePayload::TypeNoBounds(Box::new(
                        TypeNoBounds::ImplTraitOneBound(TypeImplTraitOneBound {
                            bound,
                            span_impl: token.span,
                        }),
                    ));
                    return Ok(payload);
                }
                s.plusplus();
                let mut bounds = s.parse_type_param_bounds()?;
                bounds
                    .items
                    .insert(0, TypeParamBound::TraitBound(bound));
                let payload = TypePayload::ImplTrait(bounds, token.span);
                return Ok(payload);
            }
            // trait object or trait object one bound
            T![dyn] => {
                let next_token = s.current();
                if let TokenCode::Lifetime(_) = &next_token.code {
                    let bounds = s.parse_type_param_bounds()?;
                    let payload = TypePayload::TraitObject(TypeTraitObject {
                        bounds,
                        span: token.span,
                    });
                    return Ok(payload);
                }
                let bound = s.parse_trait_bound()?;
                if !matches!(s.current().code, T![+]) {
                    let payload = TypePayload::TypeNoBounds(Box::new(
                        TypeNoBounds::TraitObjectOneBound(TypeTraitObjectOneBound {
                            bound,
                            span_dyn: token.span,
                        }),
                    ));
                    return Ok(payload);
                }
                s.plusplus();
                let mut bounds = s.parse_type_param_bounds()?;
                bounds
                    .items
                    .insert(0, TypeParamBound::TraitBound(bound));
                let payload = TypePayload::TraitObject(TypeTraitObject {
                    bounds,
                    span: token.span,
                });
                return Ok(payload);
            }

            // bare function
            T![for] => {
                let mut lifetimes = s.parse_for_lifetimes()?;
                lifetimes.span_for = token.span;
                let span_func = s.current().span;
                s.expect(T![func])?;
                let mut f = s.parse_func_type(FuncType::Func(span_func))?;
                f.for_lifetimes = Some(lifetimes);
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::BareFunction(
                    Box::new(f),
                )));
                return Ok(payload);
            }
            T![fn] => {
                let f = s.parse_func_type(FuncType::Func(token.span))?;
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::BareFunction(
                    Box::new(f),
                )));
                return Ok(payload);
            }

            // type path
            T![Self] => {
                s.minusminus();
                let a = s.parse_type_path()?;
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::TypePath(a)));
                return Ok(payload);
            }

            // type path or macro call(simple path followed by ,,)
            T![super] | T![self] | T![crate] | TokenCode::Identifier(_) => {
                s.minusminus();
                let tp = s.parse_type_path()?;
                let token = s.current();
                if !matches!(token.code, T![,,]) {
                    let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::TypePath(tp)));
                    return Ok(payload);
                }

                let mut path = SimplePath::default();
                for item in tp.items {
                    let result = SimplePathSegment::try_from(item.name);
                    if let Ok(a) = result {
                        path.items.push(a);
                    } else {
                        return Err(s.panic(token, "macro call can't contain Self"));
                    }
                }
                s.plusplus();
                let call = s.parse_macro_call(path, token.span)?;
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::MacroCall(call)));
                return Ok(payload);
            }
            _ => {
                return Err(s.panic(token, &format!("illegal token {:?} for type", token)));
            }
        }

        unreachable!("not a type");
    }

    // (params) (: Type)?
    fn parse_func_type(&self, func_type: FuncType) -> ParseResult<TypeBareFunction> {
        let s = self;

        let mut r = TypeBareFunction::default();
        r.func_type = func_type;

        // qualifier
        let token = s.current();
        if matches!(&token.code, T!["["]) {
            s.plusplus();
            while s.has_more() {
                let token = s.current();
                s.plusplus();
                match &token.code {
                    T![unsafe] => {
                        if r.qualifier.unsafe_ {
                            return Err(s.panic(token, "already set unsafe"));
                        }
                        r.qualifier.unsafe_ = true;
                        r.qualifier.span_unsafe = Some(token.span);
                    }
                    T![extern] => {
                        if r.qualifier.extern_.is_some() {
                            return Err(s.panic(token, "already set extern"));
                        }
                        r.qualifier.span_extern = Some(token.span);
                        let token = s.current();
                        if let TokenCode::Literal(Literal {
                            payload: LiteralPayload::String(a),
                            ..
                        }) = &token.code
                        {
                            r.qualifier.span_abi = Some(token.span);
                            r.qualifier.extern_ = Some(Some(a.clone()));
                            s.plusplus();
                        } else {
                            r.qualifier.extern_ = Some(None);
                        }
                    }
                    _ => {
                        return Err(s.panic(
                            token,
                            &format!("expected unsafe or extern but got {:?}", token),
                        ));
                    }
                }

                let token = s.current();
                s.plusplus();
                match &token.code {
                    T![,] => (),
                    T!["]"] => {
                        break;
                    }
                    _ => {
                        return Err(s.panic(token, &format!("expected , or ] but got {:?}", token)));
                    }
                }
            }
        }

        // params
        r.span_paren_open = s.current().span;
        s.expect(T!["("])?;
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T![")"]) {
                r.span_paren_close = token.span;
                s.plusplus();
                break;
            }

            let mut param = MaybeNamedParam::default();
            param.outer_attrs = s.parse_outer_attrs()?;

            // variadics
            if matches!(&token.code, T![...]) {
                r.variadics = Some(MaybeNamedParamVariadics {
                    outer_attrs: param.outer_attrs,
                    span: token.span,
                });
                s.plusplus();
                r.span_paren_close = s.current().span;
                s.expect(T![")"])?;
                break;
            }

            // optional param name
            if let TokenCode::Identifier(identifier) = &token.code {
                let next_token = s.nth(1);

                let is_name = matches!(
                    &next_token.code,
                    TokenCode::Identifier(_)
                        | TokenCode::Keyword(_)
                        | T!["("]
                        | T!["{{"]
                        | T![*]
                        | T![!]
                        | T![&&]
                        | T![&]
                        | T![?],
                );

                if is_name {
                    param.name = Some(identifier.clone());
                    s.plusplus();
                }
            }

            // param type
            param.type_ = s.parse_type()?;
            let token = s.current();
            s.plusplus();
            match &token.code {
                T![")"] => {
                    r.span_paren_close = token.span;
                    r.params.push(param);
                    break;
                }
                T![,] | T![;] => {
                    r.params.push(param);
                }
                _ => {
                    return Err(s.panic(token, &format!("expected , or ) but got {:?}", token)));
                }
            }
        }

        let token = s.current();
        if matches!(token.code, T![:]) {
            s.plusplus();
            // return type
            let type_ = s.parse_type()?;
            if let TypePayload::TypeNoBounds(_) = type_.payload {
                r.return_type = Some(type_);
            } else {
                return Err(s.panic(token, "expected TypeNoBounds but got {:?}"));
            }
        }

        Ok(r)
    }

    fn parse_qualifyed_path_type(&self) -> ParseResult<QualifiedPathType> {
        let s = self;

        let mut r = QualifiedPathType::default();
        r.type_ = s.parse_type()?;
        if matches!(&s.current().code, T![as]) {
            s.plusplus();
            r.type_path = Some(s.parse_type_path()?);
        }
        r.span_square_close = s.current().span;
        s.expect(T!["]"])?;

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    fn parse_where(&self) -> ParseResult<Where> {
        let s = self;

        let mut r = Where::default();

        s.expect(T!["{"])?;

        while s.has_more() {
            let token = s.current();
            if matches!(token.code, T!["}"]) {
                s.plusplus();
                break;
            }
            if let TokenCode::Lifetime(_) = &token.code {
                s.plusplus();
                let bounds = s.parse_lifetime_bounds()?;
                let item = WhereItemLifetime {
                    lifetime: Lifetime {
                        name: token.clone(),
                    },
                    lifetime_bounds: bounds.lifetime_bounds,
                };
                r.items.push(WhereItem::Lifetime(item));
            } else {
                let mut item = WhereItemTypeBound::default();
                if matches!(token.code, T![for]) {
                    s.plusplus();
                    let mut f = s.parse_for_lifetimes()?;
                    f.span_for = token.span;
                    item.for_lifetimes = Some(f);
                }
                item.type_ = s.parse_type()?;
                item.bounds = s.parse_type_param_bounds()?;
                r.items.push(WhereItem::TypeBound(item));
            }

            let token = s.current();
            match &s.current().code {
                T!["}"] => {}
                T![;] => {
                    s.plusplus();
                }
                _ => {
                    return Err(s.panic(token, &format!("expected ; or }} but got {:?}", token)));
                }
            }
        }

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    // path

    fn parse_type_path(&self) -> ParseResult<TypePath> {
        let s = self;
        let mut r = TypePath::default();

        let mut expect_name = true;
        while s.has_more() {
            let token = s.current();
            match &token.code {
                T![super] | T![self] | T![Self] | T![crate] | TokenCode::Identifier(_) => {
                    if !expect_name {
                        break;
                    }
                    expect_name = false;
                    match &token.code {
                        T![super] => r.items.push(TypePathSegment {
                            name: PathIdentSegment::Super(token.span),
                            payload: None,
                        }),
                        T![self] => r.items.push(TypePathSegment {
                            name: PathIdentSegment::SelfValue(token.span),
                            payload: None,
                        }),
                        T![Self] => r.items.push(TypePathSegment {
                            name: PathIdentSegment::SelfType(token.span),
                            payload: None,
                        }),
                        T![crate] => r.items.push(TypePathSegment {
                            name: PathIdentSegment::Crate(token.span),
                            payload: None,
                        }),
                        TokenCode::Identifier(identifier) => r.items.push(TypePathSegment {
                            name: PathIdentSegment::Identifier(identifier.clone()),
                            payload: None,
                        }),
                        _ => {}
                    }

                    s.plusplus();
                    let token = s.current();
                    if matches!(&token.code, T!["("]) {
                        // it's safe to call parse_func_type, because the next token is (, not [
                        let f = s.parse_func_type(FuncType::Other)?;

                        let mut a = TypePathFunc {
                            inputs: Vec::new(),
                            return_type: None,
                            span_paren_open: token.span,
                            span_paren_close: f.span_paren_close,
                        };
                        for param in f.params {
                            a.inputs.push(param.type_);
                        }
                        a.return_type = f.return_type;
                        let last = r.items.pop().unwrap();
                        r.items.push(TypePathSegment {
                            name: last.name,
                            payload: Some(TypePathSegmentPayload::TypePathFunc(a)),
                        });
                    }
                }
                T![..] => {
                    if expect_name {
                        return Err(s.panic(&token, "expected name but got .."));
                    }
                    expect_name = true;
                    s.plusplus();
                }
                T!["["] => {
                    if expect_name {
                        return Err(s.panic(&token, "expected name but got ["));
                    }
                    s.plusplus();
                    let mut args = s.parse_generic_args()?;
                    args.span_square_open = token.span;
                    let last = r.items.pop().unwrap();
                    r.items.push(TypePathSegment {
                        name: last.name,
                        payload: Some(TypePathSegmentPayload::GenericArgs(args)),
                    });
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        Ok(r)
    }

    fn parse_generic_args(&self) -> ParseResult<GenericArgs> {
        let s = self;

        let mut expect_arg = true;
        let mut r = GenericArgs::default();
        while s.has_more() {
            let token = s.current();
            match &token.code {
                T![,] => {
                    if expect_arg {
                        return Err(s.panic(token, "expected arg but got ,"));
                    }
                    expect_arg = true;
                    s.plusplus();
                }
                T!["]"] => {
                    if r.items.is_empty() {
                        return Err(s.panic(token, "empty generic args"));
                    }
                    r.span_square_close = token.span;
                    s.plusplus();
                    break;
                }
                _ => {
                    if !expect_arg {
                        return Err(s.panic(token, &format!("expected , or ] but got {:?}", token)));
                    }
                    expect_arg = false;
                    r.items.push(parse_generic_arg(s)?);
                }
            }
        }

        return Ok(r);

        fn parse_generic_arg(s: &Parse) -> ParseResult<GenericArg> {
            let token = s.current();
            Ok(match &token.code {
                TokenCode::Lifetime(_) => {
                    s.plusplus();
                    GenericArg::Lifetime(Lifetime {
                        name: token.clone(),
                    })
                }
                T!["{"] => {
                    let b = s.parse_block_expr()?;
                    GenericArg::Const(GenericArgsConst::BlockExpr(b))
                }
                TokenCode::Literal(literal) => {
                    s.plusplus();
                    GenericArg::Const(GenericArgsConst::LiteralExpr(ExprLiteral(literal.clone())))
                }
                T![~] => {
                    s.plusplus();
                    let next_token = s.current();
                    if let TokenCode::Literal(literal) = &next_token.code {
                        s.plusplus();
                        GenericArg::Const(GenericArgsConst::MinusLiteralExpr(
                            ExprLiteral(literal.clone()),
                            token.span,
                        ))
                    } else {
                        return Err(s.panic(
                            &next_token,
                            &format!("expected literal but got {:?}", next_token),
                        ));
                    }
                }
                TokenCode::Identifier(identifier) => {
                    let token = s.nth(1);
                    if matches!(&token.code, T![=]) {
                        s.inc(2);
                        let type_ = s.parse_type()?;
                        GenericArg::Binding(GenericArgsBinding {
                            name: identifier.clone(),
                            type_,
                            span_eq: token.span,
                        })
                    } else if matches!(&token.code, T![super] | T![self] | T![Self] | T![crate])
                        || matches!(&token.code, TokenCode::Identifier(_a))
                    {
                        s.plusplus();
                        let bounds = s.parse_type_param_bounds()?;
                        GenericArg::Bounds(GenericArgsBounds {
                            name: identifier.clone(),
                            bounds,
                        })
                    } else {
                        let type_ = s.parse_type()?;
                        GenericArg::Type(type_)
                    }
                }
                _ => {
                    let type_ = s.parse_type()?;
                    GenericArg::Type(type_)
                }
            })
        }
    }

    fn parse_path_expr_segments(&self) -> ParseResult<Vec<PathExprSegment>> {
        let s = self;

        let mut r: Vec<PathExprSegment> = Vec::new();

        let mut expect_name = true;
        while s.has_more() {
            let token = s.current();
            match &token.code {
                T![super]
                | T![self]
                | T![Self]
                | T![crate]
                | T!["["]
                | TokenCode::Identifier(_) => {
                    if !expect_name {
                        return Err(
                            s.panic(&token, &format!("not expected name but got {:?}", token))
                        );
                    }
                    expect_name = false;
                    r.push(s.parse_path_expr_segment()?);
                }
                T![..] => {
                    if expect_name {
                        return Err(s.panic(&token, "expected name but got .."));
                    }
                    expect_name = true;
                    s.plusplus();
                }
                _ => {
                    if expect_name {
                        return Err(s.panic(&token, &format!("expected name but got {:?}", token)));
                    }
                    break;
                }
            }
        }

        Ok(r)
    }

    fn parse_path_expr_segment(&self) -> ParseResult<PathExprSegment> {
        let s = self;

        let mut r = PathExprSegment::default();

        let token = s.current();
        s.plusplus();
        match &token.code {
            T![super] => {
                r.name = PathIdentSegment::Super(token.span);
            }
            T![self] => {
                r.name = PathIdentSegment::SelfValue(token.span);
            }
            T![Self] => {
                r.name = PathIdentSegment::SelfType(token.span);
            }
            T![crate] => {
                r.name = PathIdentSegment::Crate(token.span);
            }
            TokenCode::Identifier(identifier) => {
                r.name = PathIdentSegment::Identifier(identifier.clone());
            }
            _ => {
                return Err(s.panic(
                    token,
                    &format!("illegal token {:?} in path_expr_segment", token),
                ));
            }
        }

        // possible generics
        let token = s.current();
        if matches!(&token.code, T!["["]) {
            s.plusplus();
            r.generic_args = s.parse_generic_args()?;
            r.generic_args.span_square_open = token.span;
        }

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    // pattern

    fn parse_match_arm_patterns_stop(&self) -> ParseResult<Pattern> {
        let s = self;

        // find the first } token, check if it is followed by a {,
        // skipping pairs(parens, etc.) in the process
        let mut stop_set = true;
        let mut start = s.index.get();
        while start < s.tokens.len() {
            let token = &s.tokens[start];
            start += 1;
            match &token.code {
                TokenCode::Pair(_) => {
                    let brace = matches!(&token.code, T!["{"]);
                    start = skip_all_pair(s, start, token)?;
                    if brace {
                        let token = &s.tokens[start];
                        if matches!(&token.code, T!["{"]) {
                            stop_set = false;
                        }
                        break;
                    }
                }
                T![;] if s.magic => break,
                _ => (),
            }
        }

        if stop_set {
            s.stop_on_brace_open.set(true);
        }

        let r = s.parse_pattern()?;

        if stop_set {
            s.stop_on_brace_open.set(false);
        }
        return Ok(r);

        fn skip_all_pair(s: &Parse, start_index: usize, token: &Token) -> ParseResult<usize> {
            let delim_open;
            let delim_close;
            match token.code {
                T!["("] => {
                    delim_open = Pair::ParenOpen;
                    delim_close = Pair::ParenClose;
                }
                T!["["] => {
                    delim_open = Pair::SquareOpen;
                    delim_close = Pair::SquareClose;
                }
                T!["{"] => {
                    delim_open = Pair::BraceOpen;
                    delim_close = Pair::BraceClose;
                }
                T!["{{"] => {
                    delim_open = Pair::DoubleBraceOpen;
                    delim_close = Pair::DoubleBraceClose;
                }
                T!["#["] => {
                    delim_open = Pair::OuterAttr;
                    delim_close = Pair::SquareClose;
                }
                T!["#!["] => {
                    delim_open = Pair::InnerAttr;
                    delim_close = Pair::SquareClose;
                }
                _ => {
                    return Err(s.panic(
                        token,
                        &format!("illegal token {:?} in skip_all_pair", token),
                    ));
                }
            }

            let mut start = start_index;
            let mut delimiter_count = 1;
            while start < s.tokens.len() {
                let token = &s.tokens[start];
                start += 1;
                if let TokenCode::Pair(pair) = &token.code {
                    if *pair == delim_open {
                        delimiter_count += 1;
                        continue;
                    }
                    if *pair == delim_close {
                        delimiter_count -= 1;
                        if delimiter_count == 0 {
                            break;
                        }
                    }
                }
            }
            if delimiter_count > 0 {
                return Err(s.panic(token, "unclosed delimiter"));
            }

            Ok(start)
        }
    }

    fn parse_pattern(&self) -> ParseResult<Pattern> {
        let s = self;

        let mut r = Pattern::default();

        let token = s.current();
        while s.has_more() {
            r.items.push(s.parse_pattern_no_top_alt()?);
            if !matches!(s.current().code, T![|]) {
                break;
            }
            s.plusplus();
        }

        if r.items.is_empty() {
            return Err(s.panic(token, &format!("expected pattern but got {:?}", token)));
        }

        Ok(r)
    }

    fn parse_pattern_no_top_alt(&self) -> ParseResult<PatternNoTopAlt> {
        let s = self;

        let token = s.current();
        s.plusplus();
        match &token.code {
            // literal
            T![~] => {
                let next_token = s.current();
                s.plusplus();

                let literal = match &next_token.code {
                    TokenCode::Literal(lit) => {
                        if !matches!(
                            lit.payload,
                            LiteralPayload::Int(_) | LiteralPayload::Float(_)
                        ) {
                            return Err(s.panic(
                                next_token,
                                &format!("expected int or float literal but got {:?}", next_token),
                            ));
                        }
                        lit.clone()
                    }
                    _ => {
                        return Err(s.panic(
                            next_token,
                            &format!("expected int or float literal but got {:?}", next_token),
                        ));
                    }
                };
                let pattern_literal = PatternLiteral {
                    name: literal,
                    minus: true,
                    span_minus: Some(token.span),
                };

                let token = s.current();
                match &token.code {
                    T!["``"] => {
                        s.plusplus();
                        let next_token = s.current();
                        if matches!(next_token.code, T![:]) {
                            return Ok(PatternNoTopAlt::WithRange(PatternRange {
                                start: Some(PatternRangeBound::Literal(pattern_literal)),
                                end: None,
                                inclusive: false,
                                span: token.span,
                            }));
                        }
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start: Some(PatternRangeBound::Literal(pattern_literal)),
                            end: Some(bound),
                            inclusive: false,
                            span: token.span,
                        }));
                    }
                    T!["``="] => {
                        s.plusplus();
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start: Some(PatternRangeBound::Literal(pattern_literal)),
                            end: Some(bound),
                            inclusive: true,
                            span: token.span,
                        }));
                    }
                    T![...] => {
                        s.plusplus();
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::Obsolete(PatternRange {
                                start: Some(PatternRangeBound::Literal(pattern_literal)),
                                end: Some(bound),
                                inclusive: true,
                                span: token.span,
                            }),
                        ));
                    }
                    _ => {
                        return Ok(PatternNoTopAlt::WithoutRange(PatternWithoutRange::Literal(
                            pattern_literal,
                        )));
                    }
                }
            }
            TokenCode::Literal(literal) => {
                let pattern_literal = PatternLiteral {
                    name: literal.clone(),
                    minus: false,
                    span_minus: None,
                };
                let token = s.current();
                match &token.code {
                    T!["``"] => {
                        s.plusplus();
                        let next_token = s.current();
                        if matches!(next_token.code, T![:]) {
                            return Ok(PatternNoTopAlt::WithRange(PatternRange {
                                start: Some(PatternRangeBound::Literal(pattern_literal)),
                                end: None,
                                inclusive: false,
                                span: token.span,
                            }));
                        }
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start: Some(PatternRangeBound::Literal(pattern_literal)),
                            end: Some(bound),
                            inclusive: false,
                            span: token.span,
                        }));
                    }
                    T!["``="] => {
                        s.plusplus();
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start: Some(PatternRangeBound::Literal(pattern_literal)),
                            end: Some(bound),
                            inclusive: true,
                            span: token.span,
                        }));
                    }
                    T![...] => {
                        s.plusplus();
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::Obsolete(PatternRange {
                                start: Some(PatternRangeBound::Literal(pattern_literal)),
                                end: Some(bound),
                                inclusive: true,
                                span: token.span,
                            }),
                        ));
                    }
                    _ => {
                        return Ok(PatternNoTopAlt::WithoutRange(PatternWithoutRange::Literal(
                            pattern_literal,
                        )));
                    }
                }
            }

            // identifier
            T![ref] => {
                let mut r = PatternIdentifier::default();
                r.ref_ = true;
                r.span_ref = Some(token.span);
                let token = s.current();
                if matches!(&token.code, T![mut]) {
                    r.span_mut = Some(token.span);
                    r.mut_ = true;
                    s.plusplus();
                }
                let token = s.current();
                s.plusplus();
                if let TokenCode::Identifier(identifier) = &token.code {
                    r.name = identifier.clone();
                } else {
                    return Err(s.panic(token, &format!("expected identifier but got {:?}", token)));
                }

                // possible @
                let token = s.current();
                if matches!(&token.code, T![@]) {
                    r.span_at = Some(token.span);
                    s.plusplus();
                    r.pattern = Some(Box::new(s.parse_pattern()?));
                }
                return Ok(PatternNoTopAlt::WithoutRange(
                    PatternWithoutRange::Identifier(r),
                ));
            }
            T![mut] => {
                let mut r = PatternIdentifier::default();
                r.span_mut = Some(token.span);
                r.mut_ = true;
                let token = s.current();
                s.plusplus();
                if let TokenCode::Identifier(identifier) = &token.code {
                    r.name = identifier.clone();
                } else {
                    return Err(s.panic(token, &format!("expected identifier but got {:?}", token)));
                }

                // possible @
                let token = s.current();
                if matches!(&token.code, T![@]) {
                    r.span_at = Some(token.span);
                    s.plusplus();
                    r.pattern = Some(Box::new(s.parse_pattern()?));
                }
                return Ok(PatternNoTopAlt::WithoutRange(
                    PatternWithoutRange::Identifier(r),
                ));
            }

            // rest
            T![...] => {
                return Ok(PatternNoTopAlt::WithoutRange(PatternWithoutRange::Rest(
                    token.span,
                )));
            }
            // `` RangePattern
            T!["``"] => {
                let bound = parse_range_pattern_bound(s)?;
                return Ok(PatternNoTopAlt::WithRange(PatternRange {
                    start: None,
                    end: Some(bound),
                    inclusive: false,
                    span: token.span,
                }));
            }
            // ``= RangePattern
            T!["``="] => {
                let bound = parse_range_pattern_bound(s)?;
                return Ok(PatternNoTopAlt::WithRange(PatternRange {
                    start: None,
                    end: Some(bound),
                    inclusive: true,
                    span: token.span,
                }));
            }

            // grouped or tuple
            T!["("] => {
                let span_paren_open = token.span;
                let mut list: Vec<Pattern> = Vec::new();
                let mut is_tuple = false;
                while s.has_more() {
                    let token = s.current();
                    match &token.code {
                        T![")"] => {
                            s.plusplus();
                            if list.len() == 1 && !is_tuple {
                                let a = mem::take(&mut list[0]);
                                if a.items.len() > 1 {
                                    return Ok(PatternNoTopAlt::WithoutRange(
                                        PatternWithoutRange::Grouped(PatternGrouped {
                                            pattern: Box::new(a),
                                            span_paren_open,
                                            span_paren_close: token.span,
                                        }),
                                    ));
                                }
                                if let PatternNoTopAlt::WithoutRange(PatternWithoutRange::Rest(
                                    rest,
                                )) = a.items[0]
                                {
                                    return Ok(PatternNoTopAlt::WithoutRange(
                                        PatternWithoutRange::Tuple(PatternTuple {
                                            payload: PatternTuplePayload::Rest(rest),
                                            span_paren_open,
                                            span_paren_close: token.span,
                                        }),
                                    ));
                                } else {
                                    return Ok(PatternNoTopAlt::WithoutRange(
                                        PatternWithoutRange::Grouped(PatternGrouped {
                                            pattern: Box::new(a),
                                            span_paren_open,
                                            span_paren_close: token.span,
                                        }),
                                    ));
                                }
                            } else {
                                return Ok(PatternNoTopAlt::WithoutRange(
                                    PatternWithoutRange::Tuple(PatternTuple {
                                        payload: PatternTuplePayload::Patterns(list),
                                        span_paren_open,
                                        span_paren_close: token.span,
                                    }),
                                ));
                            }
                        }
                        T![,] => {
                            is_tuple = true;
                            s.plusplus();
                        }
                        _ => {
                            list.push(s.parse_pattern()?);
                        }
                    }
                }
            }
            // slice
            T!["{{"] => {
                let next_token = s.current();
                if matches!(&next_token.code, T!["}}"]) {
                    return Ok(PatternNoTopAlt::WithoutRange(PatternWithoutRange::Slice(
                        PatternSlice {
                            items: Vec::new(),
                            span_square_open: token.span,
                            span_square_close: next_token.span,
                        },
                    )));
                }
                let mut items: Vec<Pattern> = Vec::new();
                while s.has_more() {
                    items.push(s.parse_pattern()?);
                    if !matches!(&s.current().code, T![,]) {
                        break;
                    }
                    s.plusplus();
                }
                let next_token = s.current();
                s.expect(T!["}}"])?;
                return Ok(PatternNoTopAlt::WithoutRange(PatternWithoutRange::Slice(
                    PatternSlice {
                        items,
                        span_square_open: token.span,
                        span_square_close: next_token.span,
                    },
                )));
            }
            // reference
            T![&] | T![&&] => {
                let multi_ref = matches!(&token.code, T![&&]);
                let span_ref = token.span;
                let mut span_mut = None;

                // possible mut
                let mut mut_ = false;
                let token = s.current();
                if matches!(&token.code, T![mut]) {
                    span_mut = Some(token.span);
                    mut_ = true;
                    s.plusplus();
                }

                let token = s.current();
                let mut pat = s.parse_pattern()?;
                let a = mem::take(&mut pat.items[0]);
                if let PatternNoTopAlt::WithoutRange(p) = a {
                    return Ok(PatternNoTopAlt::WithoutRange(
                        PatternWithoutRange::Reference(PatternReference {
                            multi_ref,
                            mut_,
                            pattern_without_range: Box::new(p),

                            span_ref,
                            span_mut,
                        }),
                    ));
                } else {
                    return Err(s.panic(token, "expected PatternWithoutRange"));
                }
            }

            // qualified path in expression
            T!["["] => {
                let mut type_ = s.parse_qualifyed_path_type()?;
                type_.span_square_open = token.span;
                s.expect(T![..])?;
                let items = s.parse_path_expr_segments()?;
                return Ok(PatternNoTopAlt::WithoutRange(PatternWithoutRange::Path(
                    PatternPath::QualifiedPath(QualifiedPathInExpr { type_, items }),
                )));
            }

            // path in expression or macro call(simple path followed by ,,)
            T![super] | T![self] | T![Self] | T![crate] | TokenCode::Identifier(_) => {
                let next_token = s.current();
                if matches!(&next_token.code, T![@]) {
                    // possible @
                    if let TokenCode::Identifier(identifier) = &token.code {
                        let mut a = PatternIdentifier::default();
                        a.name = identifier.clone();
                        a.span_at = Some(next_token.span);
                        s.plusplus();
                        a.pattern = Some(Box::new(s.parse_pattern()?));
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::Identifier(a),
                        ));
                    } else {
                        return Err(
                            s.panic(token, &format!("expected identifier but got {:?}", token))
                        );
                    }
                }

                s.minusminus();
                let items = s.parse_path_expr_segments()?;
                let token = s.current();
                match &token.code {
                    // macro call
                    T![,,] => {
                        let mut path = SimplePath::default();
                        for item in items {
                            if !item.generic_args.items.is_empty() {
                                return Err(
                                    s.panic(token, "macro call can't contain generic params")
                                );
                            }
                            let result = SimplePathSegment::try_from(item.name);
                            if let Ok(a) = result {
                                path.items.push(a);
                            } else {
                                return Err(s.panic(token, "macro call can't contain Self"));
                            }
                        }
                        s.plusplus();
                        let call = s.parse_macro_call(path, token.span)?;
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::MacroCall(call),
                        ));
                    }
                    // RangePattern ``
                    T!["``"] => {
                        s.plusplus();
                        let start = Some(PatternRangeBound::Path(PathInExpr { items }));
                        let next_token = s.current();
                        if matches!(next_token.code, T![:]) {
                            return Ok(PatternNoTopAlt::WithRange(PatternRange {
                                start,
                                end: None,
                                inclusive: false,
                                span: token.span,
                            }));
                        }
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start,
                            end: Some(bound),
                            inclusive: false,
                            span: token.span,
                        }));
                    }
                    // RangePattern ``= RangePattern
                    T!["``="] => {
                        s.plusplus();
                        let start = Some(PatternRangeBound::Path(PathInExpr { items }));
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start,
                            end: Some(bound),
                            inclusive: true,
                            span: token.span,
                        }));
                    }
                    // RangePattern ... RangePattern
                    T![...] => {
                        s.plusplus();
                        let start = Some(PatternRangeBound::Path(PathInExpr { items }));
                        let bound = parse_range_pattern_bound(s)?;
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::Obsolete(PatternRange {
                                start,
                                end: Some(bound),
                                inclusive: true,
                                span: token.span,
                            }),
                        ));
                    }
                    _ => {
                        let path = PathInExpr { items };
                        match &token.code {
                            T!["{"] => {
                                if s.stop_on_brace_open.get() {
                                    s.stop_on_brace_open.set(false);
                                    return Ok(PatternNoTopAlt::WithoutRange(
                                        PatternWithoutRange::Path(PatternPath::Path(path)),
                                    ));
                                }

                                s.plusplus();
                                let mut a = parse_struct_pattern(s)?;
                                a.path = path;
                                a.span_brace_open = token.span;
                                return Ok(PatternNoTopAlt::WithoutRange(
                                    PatternWithoutRange::Struct(a),
                                ));
                            }
                            T!["("] => {
                                s.plusplus();
                                let mut a = parse_tuple_struct_pattern(s)?;
                                a.path = path;
                                a.span_paren_open = token.span;
                                return Ok(PatternNoTopAlt::WithoutRange(
                                    PatternWithoutRange::TupleStruct(a),
                                ));
                            }
                            _ => {
                                return Ok(PatternNoTopAlt::WithoutRange(
                                    PatternWithoutRange::Path(PatternPath::Path(path)),
                                ));
                            }
                        }
                    }
                }
            }
            _ => {
                return Err(s.panic(token, &format!("illegal token {:?} for pattern", token)));
            }
        }

        unreachable!("not a pattern");

        // after ``=, `` or ...
        // only one PatternNoTopAlt is allowed
        fn parse_range_pattern_bound(s: &Parse) -> ParseResult<PatternRangeBound> {
            let mut pattern = s.parse_pattern()?;
            let pat = mem::take(&mut pattern.items[0]);
            Ok(match pat {
                PatternNoTopAlt::WithoutRange(PatternWithoutRange::Literal(a)) => {
                    PatternRangeBound::Literal(a)
                }
                PatternNoTopAlt::WithoutRange(PatternWithoutRange::Path(PatternPath::Path(p))) => {
                    PatternRangeBound::Path(p)
                }
                PatternNoTopAlt::WithoutRange(PatternWithoutRange::Path(
                    PatternPath::QualifiedPath(p),
                )) => PatternRangeBound::QualifiedPath(p),
                _ => {
                    unreachable!("not a pattern range bound");
                }
            })
        }

        // after {
        fn parse_struct_pattern(s: &Parse) -> ParseResult<PatternStruct> {
            let mut r = PatternStruct::default();

            let mut expect_field = true;
            while s.has_more() {
                let token = s.current();
                match &token.code {
                    T!["}"] => {
                        r.span_brace_close = token.span;
                        s.plusplus();
                        break;
                    }
                    T![,] | T![;] => {
                        if expect_field {
                            return Err(
                                s.panic(token, &format!("expected field but got {:?}", token))
                            );
                        }
                        expect_field = true;
                        s.plusplus();
                    }
                    _ => {
                        if !expect_field {
                            return Err(
                                s.panic(token, &format!("expected , or }} but got {:?}", token))
                            );
                        }
                        expect_field = false;
                        r.items.push(parse_struct_pattern_item(s)?);
                    }
                }
            }

            return Ok(r);

            fn parse_struct_pattern_item(s: &Parse) -> ParseResult<PatternStructItem> {
                let r;

                let outer_attrs = s.parse_outer_attrs()?;

                let token = s.current();
                s.plusplus();
                match &token.code {
                    T![...] => {
                        r = PatternStructItem::Etc(outer_attrs, token.span);
                    }
                    TokenCode::Literal(Literal {
                        payload: LiteralPayload::Int(name),
                        ..
                    }) => {
                        s.expect(T![:])?;
                        let pattern = s.parse_pattern()?;
                        r = PatternStructItem::Field(PatternStructField {
                            outer_attrs,
                            payload: PatternStructFieldPayload::A(PatternStructFieldA {
                                tuple_index: name.clone(),
                                pattern,
                                span_tuple_index: token.span,
                            }),
                        });
                    }
                    TokenCode::Identifier(identifier) => {
                        if matches!(&s.current().code, T![:]) {
                            s.plusplus();
                            let pattern = s.parse_pattern()?;
                            r = PatternStructItem::Field(PatternStructField {
                                outer_attrs,
                                payload: PatternStructFieldPayload::B(PatternStructFieldB {
                                    name: identifier.clone(),
                                    pattern,
                                }),
                            });
                        } else {
                            let mut c = PatternStructFieldC::default();
                            c.name = identifier.clone();
                            r = PatternStructItem::Field(PatternStructField {
                                outer_attrs,
                                payload: PatternStructFieldPayload::C(c),
                            });
                        }
                    }
                    T![ref] => {
                        let mut c = PatternStructFieldC::default();
                        c.span_ref = Some(token.span);
                        c.ref_ = true;
                        let token = s.current();
                        if matches!(&token.code, T![mut]) {
                            c.span_mut = Some(token.span);
                            c.mut_ = true;
                            s.plusplus();
                        }
                        let token = s.current();
                        if let TokenCode::Identifier(identifier) = &token.code {
                            c.name = identifier.clone();
                            s.plusplus();
                        } else {
                            return Err(s.panic(
                                &token,
                                &format!("expected identifier but got {:?}", token),
                            ));
                        }
                        r = PatternStructItem::Field(PatternStructField {
                            outer_attrs,
                            payload: PatternStructFieldPayload::C(c),
                        });
                    }
                    T![mut] => {
                        let mut c = PatternStructFieldC::default();
                        c.span_mut = Some(token.span);
                        c.mut_ = true;
                        let token = s.current();
                        if let TokenCode::Identifier(identifier) = &token.code {
                            c.name = identifier.clone();
                            s.plusplus();
                        } else {
                            return Err(s.panic(
                                &token,
                                &format!("expected identifier but got {:?}", token),
                            ));
                        }
                        r = PatternStructItem::Field(PatternStructField {
                            outer_attrs,
                            payload: PatternStructFieldPayload::C(c),
                        });
                    }
                    _ => {
                        return Err(
                            s.panic(&token, &format!("illegal token {:?} for pattern", token))
                        );
                    }
                }

                Ok(r)
            }
        }

        // after (
        fn parse_tuple_struct_pattern(s: &Parse) -> ParseResult<PatternTupleStruct> {
            let mut r = PatternTupleStruct::default();

            let mut expect_pattern = true;
            while s.has_more() {
                let token = s.current();
                match &token.code {
                    T![")"] => {
                        r.span_paren_close = token.span;
                        s.plusplus();
                        break;
                    }
                    T![,] => {
                        if expect_pattern {
                            return Err(s.panic(&token, "expected pattern but got ,"));
                        }
                        expect_pattern = true;
                        s.plusplus();
                    }
                    _ => {
                        if !expect_pattern {
                            return Err(s.panic(&token, "not expected pattern"));
                        }
                        expect_pattern = false;
                        r.items.push(s.parse_pattern()?);
                    }
                }
            }

            Ok(r)
        }
    }

    // --------------------------------------------------------------------------------
    // attr
    fn parse_inner_attrs(&self) -> ParseResult<Attrs> {
        let s = self;
        s.parse_attrs(true)
    }

    fn parse_outer_attrs(&self) -> ParseResult<Attrs> {
        let s = self;
        s.parse_attrs(false)
    }

    fn parse_attrs(&self, inner: bool) -> ParseResult<Attrs> {
        let s = self;

        let mut r = Attrs::default();
        while s.has_more() {
            s.skip_semicolons();
            if !s.has_more() {
                break;
            }

            let token = s.current();
            if inner {
                if !matches!(&token.code, T!["#!["]) {
                    break;
                }
            } else {
                // common-code-error: expected outer attributes but got inner attributes
                // implementation: inner attributes can't follow outer attributes
                if matches!(&token.code, T!["#!["]) {
                    return Err(
                        s.panic(&token, "expected outer attributes but got inner attributes")
                    );
                }
                if !matches!(&token.code, T!["#["]) {
                    break;
                }
            }
            s.plusplus();
            let tokens = parse_attr(s)?;
            let mut t_end = s.current().clone();
            t_end.code = TokenCode::Pair(Pair::EndAttr);
            let attr = Attr {
                tokens,
                t_start: token.clone(),
                t_end,
            };
            s.expect(T!["]"])?;
            r.items.push(attr);
        }

        return Ok(r);

        // after #[ or #![
        fn parse_attr(s: &Parse) -> ParseResult<Vec<Token>> {
            let mut r: Vec<Token> = Vec::new();
            let mut square_count = 0;
            while s.has_more() {
                let token = s.current();
                match token.code {
                    T!["["] => {
                        square_count += 1;
                    }
                    T!["]"] => {
                        if square_count > 0 {
                            square_count -= 1;
                        } else {
                            break;
                        }
                    }
                    _ => {}
                }
                s.plusplus();
                r.push(token.clone());
            }
            Ok(r)
        }
    }
}

// --------------------------------------------------------------------------------
#[derive(Debug, Serialize)]
pub struct ParseError {
    #[serde(rename = "lnum")]
    pub line: usize,
    #[serde(rename = "col")]
    pub column: usize,
    #[serde(rename = "_width")]
    pub width: usize,
    pub text: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self;
        write!(f, "{}:{}: {}", s.line, s.column, s.text)
    }
}
impl error::Error for ParseError {}

pub type ParseResult<T> = Result<T, ParseError>;

// --------------------------------------------------------------------------------
#[derive(Debug, Default)]
struct IdentifierPlus {
    name: Token,
    vis: Visibility,
}

#[derive(Debug, Default)]
struct IdentifierPlusGenerics {
    name: Token,
    vis: Visibility,
    generics: Option<Generics>,
}

#[derive(Debug, Default)]
struct ImplLeft {
    type_: Type,
    impl_generics: Option<Generics>,
}
