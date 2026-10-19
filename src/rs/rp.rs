use crate::rs::ra::*;
use crate::rs::rl::{
    Token, TokenCode, Span, Keyword, Pair, Op, Assign, Lex, Literal, LiteralPayload, T,
};
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

    op_map: HashMap<TokenCode, u8>,

    magic: bool, // for auto completion
}

struct SavedState {
    index: usize,
}

impl Parse {
    pub fn new(lex: Lex) -> Parse {
        let mut tokens = lex.tokens;
        if !tokens.is_empty() {
            let last_semicolon = {
                let mut r = Token::default();
                r.code = T![;];
                r.u = ";".to_owned();

                let span = if lex.end_tokens.is_empty() {
                    tokens.last().unwrap().span
                } else {
                    lex.end_tokens.last().unwrap().span
                };
                r.span = Span {
                    line: span.end_line,
                    column: span.end_column + 1,
                    width: 1,
                    end_line: span.end_line,
                    end_column: span.end_column + 1,
                };

                r
            };
            tokens.push(last_semicolon);
        }

        return Parse {
            pkg: Package::default(),

            tokens,
            end_tokens: lex.end_tokens,
            index: Cell::new(0),

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
            m.insert(T![-], p);

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
            m.insert(T![..], p);
            m.insert(T![..=], p);

            m
        }
    }

    fn save_state(&self) -> SavedState {
        let s = self;
        SavedState {
            index: s.index.get(),
        }
    }
    fn restore_state(&self, a: &SavedState) {
        let s = self;
        s.index.set(a.index);
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

    pub fn to_u(self) -> Result<ToU, ParseError> {
        let s = self;
        let mut p = LangFormatter::new(&s.tokens, &s.end_tokens);
        s.pkg.to_u(&mut p);
        Ok(p.to_u())
    }

    pub fn to_rs(self) -> String {
        let s = self;
        let mut p = LangFormatter::new(&s.tokens, &s.end_tokens);
        s.pkg.to_rs(&mut p);
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

    // when encounters ( [ or [`, fast-forward to ) ] or `]
    fn skip_pair(&self, start_index: usize) -> ParseResult<usize> {
        let s = self;

        let token = &s.tokens[start_index];

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
            T!["[`"] => {
                delim_open = Pair::ArrayOpen;
                delim_close = Pair::ArrayClose;
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

        let mut start = start_index + 1;
        let mut delimiter_count = 1;

        if delim_close == Pair::SquareClose {
            while start < s.tokens.len() {
                let token = &s.tokens[start];
                start += 1;
                if let TokenCode::Pair(pair) = &token.code {
                    if matches!(*pair,
                            Pair::SquareOpen |
                            Pair::OuterAttr |
                            Pair::InnerAttr) {
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
        } else {
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
        let rs_pair = RsPair {
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
                rs_pair,
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
            match s.parse_stmt(items.len()) {
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
            rs_pair,
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
            let stmt = s.parse_stmt(r.stmts.len())?;
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
    fn parse_stmt(&self, stmt_len: usize) -> ParseResult<Stmt> {
        let s = self;

        // check if the stmt before ends with ; or "}"
        if stmt_len > 0 {
            s.minusminus();
            let t_last = s.current().clone();
            s.plusplus();
            if !matches!(t_last.code, T![;] | T!["}"]) {
                return Err(s.panic(&t_last, "expected ; or }} for ending Stmt"));
            }
        }

        // parse item's outer_attrs
        let outer_attrs = s.parse_outer_attrs()?;

        // parse item payload
        s.skip_semicolons();
        if !s.has_more() {
            s.minusminus();
            return Err(s.panic(&s.current(), "expected Stmt"));
        }

        let vis = s.parse_vis()?;

        // get_first_special_token
        let Some(first_special_token) = s.get_first_special_token() else {
            // parse stmt expr
            let mut config = ParseConfig::default();
            config.may_end_with_semicolon = true;
            if need_stop_on_block_expr(s) {
                config.stop_on_block_expr = true;
            }
            let mut expr = s.parse_expr_config(&config)?;
            expr.outer_attrs = outer_attrs;

            // stmt value
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                s.minusminus();
                let t_last = s.current();
                s.plusplus();
                let need_backslash = matches!(expr.payload, ExprPayload::WithoutBlock(_))
                    && t_last.span.end_line != token.span.line;
                return Ok(Stmt::Value(StmtValue { expr, need_backslash }));
            }

            // macro call
            let mut is_macro_call = false;
            if let ExprPayload::WithoutBlock(b) = &expr.payload {
                if let ExprWithoutBlock::MacroCall(_) = b.as_ref() {
                    is_macro_call = true;
                }
            }
            if is_macro_call {
                return Ok(Stmt::Item(Item {
                    outer_attrs: Attrs::default(),
                    payload: ItemPayload::MacroCall(expr),
                }));
            }

            // expr
            let token = s.current();
            let semicolon_after = if matches!(&token.code, T![;]) {
                s.plusplus();
                if token.span.end_line == s.current().span.line {
                    true
                } else {
                    matches!(&expr.payload, ExprPayload::WithBlock(_))
                }
            } else {
                false
            };
            return Ok(Stmt::Expr(StmtExpr {
                expr,
                semicolon_after,
            }));
        };

        let stmt: Stmt;
        let special_token = first_special_token;
        match &special_token.code {
            // macro_rules!
            T![!] => {
                let a = s.parse_macro()?;
                stmt = Stmt::Item(Item {
                    outer_attrs,
                    payload: ItemPayload::Macro(a),
                });
            }
            T![let] => {
                let mut a = s.parse_let()?;
                a.outer_attrs = outer_attrs;
                stmt = Stmt::Let(a);
            }
            TokenCode::Keyword(keyword) => match keyword {
                Keyword::Use => {
                    let mut a = s.parse_use()?;
                    a.t_use = special_token.clone();
                    a.vis = vis;
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Use(a),
                    });
                }
                Keyword::Extern => {
                    if matches!(s.nth(1).code, T![crate]) {
                        let mut a = s.parse_crate()?;
                        a.vis = vis;
                        stmt = Stmt::Item(Item {
                            outer_attrs,
                            payload: ItemPayload::Crate(a),
                        });
                    } else {
                        let mut a = s.parse_extern()?;
                        a.t_extern = special_token.clone();
                        a.vis = vis;
                        stmt = Stmt::Item(Item {
                            outer_attrs,
                            payload: ItemPayload::Extern(a),
                        });
                    }
                }
                Keyword::Const => {
                    let mut a = s.parse_const()?;
                    a.vis = vis;
                    a.t_const = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Const(a),
                    });
                }
                Keyword::Enum => {
                    let mut a = s.parse_enum()?;
                    a.vis = vis;
                    a.t_enum = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Enum(a),
                    });
                }
                Keyword::Fn => {
                    let mut a = s.parse_func()?;
                    a.vis = vis;
                    a.t_func = special_token.clone();
                    a.t_func.code = T![func];
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Func(Box::new(a)),
                    });
                }
                Keyword::Impl => {
                    let mut a = s.parse_impl()?;
                    a.t_impl = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Impl(a),
                    });
                }
                Keyword::Trait => {
                    let mut a = s.parse_trait()?;
                    a.vis = vis;
                    a.t_trait = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Trait(a),
                    });
                }
                Keyword::Mod => {
                    let mut a = s.parse_mod()?;
                    a.vis = vis;
                    a.t_mod = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Mod(a),
                    });
                }
                Keyword::Static => {
                    let mut a = s.parse_static()?;
                    a.vis = vis;
                    a.t_static = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Static(a),
                    });
                }
                Keyword::Struct => {
                    let mut a = s.parse_struct()?;
                    a.vis = vis;
                    a.t_struct = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Struct(a),
                    });
                }
                Keyword::Type => {
                    let mut a = s.parse_type_alias()?;
                    a.vis = vis;
                    a.t_type = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::TypeAlias(a),
                    });
                }
                _ => {
                    return Err(s.panic(s.current(), "not a valid first_special_token"));
                }
            },
            TokenCode::Identifier(id) => {
                if id.u == "union" {
                    let mut a = s.parse_union()?;
                    a.vis = vis;
                    a.t_union = special_token.clone();
                    stmt = Stmt::Item(Item {
                        outer_attrs,
                        payload: ItemPayload::Union(a),
                    });
                } else {
                    return Err(s.panic(s.current(), "not a valid first_special_token"));
                }
            }
            _ => {
                return Err(s.panic(s.current(), "not a valid first_special_token"));
            }
        }

        return Ok(stmt);

        fn need_stop_on_block_expr(s: &Parse) -> bool {
            let token = s.current();
            if matches!(
                &token.code,
                T![if]
                | T![match]
                | T![loop]
                | T![while]
                | T![for]
                | T!["{"]
                | TokenCode::Lifetime(_)
            ) {
                return true;
            }
            if s.index.get() + 1 >= s.tokens.len() {
                return false;
            }
            let t1 = s.nth(1);
            match &token.code {
                T![const] | T![unsafe]
                    if matches!(&t1.code, T!["{"]) => true,
                T![async] => {
                    if matches!(&t1.code, T!["{"]) {
                        return true;
                    }
                    if s.index.get() + 2 >= s.tokens.len() {
                        return false;
                    }
                    let t2 = s.nth(2);
                    matches!(&t1.code, T![move]) && matches!(&t2.code, T!["{"])
                }
                _ => false,
            }
        }
    }

    fn get_first_special_token(&self) -> Option<&Token> {
        let s = self;

        let token = s.current();

        let mut is_func = false;

        match &token.code {
            T![use] | T![struct] | T![enum] | T![trait] | T![fn] | T![impl] | T![let] |
            T![mod] | T![static] | T![type]
            => {
                return Some(token);
            }
            T![const] => {
                let t1 = s.nth(1);
                match &t1.code {
                    T!["{"] => {
                        return None;
                    }
                    // const IDENTIFIER
                    TokenCode::Identifier(_) => {
                        return Some(token);
                    }
                    // const async? [safe|unsafe]? (extern Abi?)? fn
                    _ => {
                        is_func = true;
                    }
                }
            }
            T![async] => {
                let t1 = s.nth(1);
                if matches!(t1.code, T![move] | T!["{"]) {
                    return None;
                }
                // async [safe|unsafe]? (extern Abi?)? fn
                is_func = true;
            }
            T![unsafe] => {
                let t1 = s.nth(1);
                match &t1.code {
                    // unsafe mod
                    // unsafe static
                    // unsafe fn
                    // unsafe trait
                    // unsafe impl
                    T![mod] | T![static] | T![fn] | T![trait] | T![impl] => {
                        return Some(t1);
                    }
                    T![extern] => {
                        let t2 = s.nth(2);
                        match &t2.code {
                            // unsafe extern {}
                            T!["{"] => {
                                return Some(t1);
                            }
                            // unsafe extern fn
                            T![fn] => {
                                return Some(t2);
                            }
                            _ => {}
                        }
                        let t3 = s.nth(3);
                        // unsafe extern Abi fn
                        if matches!(t3.code, T![fn]) {
                            return Some(t3);
                        }
                        // unsafe extern Abi {}
                        return Some(t1);
                    }
                    _ => {}
                }
            }
            T![extern] => {
                let t1 = s.nth(1);
                match &t1.code {
                    // extern crate
                    // extern {}
                    T![crate] | T!["{"] => {
                        return Some(token);
                    }
                    // extern fn
                    T![fn] => {
                        return Some(t1);
                    }
                    _ => {}
                }
                let t2 = s.nth(2);
                match &t2.code {
                    // extern Abi {}
                    T!["{"] => {
                        return Some(token);
                    }
                    // extern Abi fn
                    T![fn] => {
                        return Some(t2);
                    }
                    _ => {}
                }
            }
            TokenCode::Identifier(a) => {
                if a.prefix.is_some() {
                    return None;
                }
                match a.id.as_ref() {
                    "safe" => {
                        // safe static
                        // safe fn
                        // safe extern Abi? fn
                        let t1 = s.nth(1);
                        match &t1.code {
                            T![static] => {
                                return Some(t1);
                            }
                            T![fn] => {
                                return Some(t1);
                            }
                            T![extern] => {
                                is_func = true;
                            }
                            _ => {}
                        }
                    }
                    "macro_rules" => {
                        let token = s.nth(1);
                        if matches!(&token.code, T![!]) {
                            return Some(&token);
                        }
                    }
                    "union" => {
                        return Some(&token);
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        // const? async? [safe|unsafe]? (extern Abi?)? fn
        if is_func {
            let mut i = 0;
            while i < s.tokens.len() {
                let token = s.nth(i);
                if matches!(token.code, T![fn]) {
                    return Some(token);
                }
                i += 1;
            }
        }

        return None;
    }

    // --------------------------------------------------------------------------------

    fn parse_vis(&self) -> ParseResult<Visibility> {
        let s = self;

        let mut r = Visibility::default();

        let token = s.current();
        if !matches!(&token.code, T![pub]) {
            return Ok(r);
        }

        r = Visibility {
            payload: VisibilityPayload::Pub,
            t_pub: Some(s.current().clone()),
        };
        s.plusplus();

        if !matches!(s.current().code, T!["("]) {
            return Ok(r);
        };
        s.plusplus();

        // pub(sth)
        let token = s.current();
        s.plusplus();
        match &token.code {
            T![self] => r.payload = VisibilityPayload::Private(true),
            T![super] => r.payload = VisibilityPayload::Super,
            T![crate] => r.payload = VisibilityPayload::Crate,
            T![in] => {
                let items = s.parse_path_expr_segments()?;
                let mut path = SimplePath::default();
                for item in items {
                    if !item.generic_args.items.is_empty() {
                        return Err(s.panic(token, "pub can't contain generic params"));
                    }
                    let result = SimplePathSegment::try_from(item);
                    let Ok(a) = result else {
                        return Err(s.panic(token, "pub can't contain Self"));
                    };
                    path.items.push(a);
                }
                r.payload = VisibilityPayload::SimplePath(path);
            }
            _ => {
                return Err(s.panic(token, "illegal token after 'pub ('"));
            }
        }
        s.expect(T![")"])?;
        return Ok(r);
    }

    fn parse_generics(&self) -> ParseResult<Generics> {
        let s = self;

        let mut r = Generics::default();

        r.t_square_open = s.current().clone();
        r.t_square_open.code = T!["["];
        s.plusplus();

        let token = s.current();
        if matches!(&token.code, T![>]) {
            return Err(s.panic(token, "generic params can't be empty"));
        }

        while s.has_more() {
            let param = parse_generic_param(s)?;
            r.params.push(param);

            let token = s.current();
            if matches!(&token.code, T![>]) {
                s.plusplus();
                r.t_square_close = token.clone();
                r.t_square_close.code = T!["]"];
                break;
            }
        }

        return Ok(r);

        fn parse_generic_param(s: &Parse) -> ParseResult<GenericParam> {
            let mut r = GenericParam::default();

            let token = s.current();
            if matches!(&token.code, T!["#["]) {
                r.outer_attrs = s.parse_outer_attrs()?;
            }

            let token = s.current();
            match &token.code {
                TokenCode::Lifetime(_) => {
                    let mut c = LifetimeParam::default();
                    c.lifetime = Lifetime {
                        name: token.clone(),
                    };
                    s.plusplus();
                    if matches!(s.current().code, T![:]) {
                        s.plusplus();
                        c.bounds = s.parse_lifetime_bounds()?;
                    }
                    r.payload = GenericParamPayload::Lifetime(c);
                }
                TokenCode::Identifier(_) => {
                    let c = parse_type_param(s)?;
                    r.payload = GenericParamPayload::Type(c);
                }
                T![const] => {
                    let c = parse_const_param(s)?;
                    r.payload = GenericParamPayload::Const(c);
                }
                _ => {
                    return Err(s.panic(
                        token,
                        &format!("illegal token {:?} for generic params", token),
                    ));
                }
            }

            let token = s.current();
            if !matches!(&token.code, T![>] | T![,]) {
                return Err(s.panic(token, &format!("expected > or , but got {:?}", token)));
            }
            if matches!(&token.code, T![,]) {
                r.t_comma = Some(token.clone());
                s.plusplus();
                return Ok(r);
            }

            Ok(r)
        }

        fn parse_type_param(s: &Parse) -> ParseResult<TypeParam> {
            let mut r = TypeParam::default();

            r.name = s.current().clone();
            s.plusplus();

            let token = s.current();
            if matches!(&token.code, T![:]) {
                s.plusplus();
                r.bounds = s.parse_type_param_bounds()?;
            }

            let token = s.current();
            if matches!(&token.code, T![=]) {
                s.plusplus();
                r.type_ = Some((token.clone(), s.parse_type()?));
            }

            Ok(r)
        }

        fn parse_const_param(s: &Parse) -> ParseResult<ConstParam> {
            let mut r = ConstParam::default();

            r.t_const = s.current().clone();
            s.plusplus();
            r.name = s.current().clone();
            s.plusplus();

            s.expect(T![:])?;
            r.type_ = s.parse_type()?;

            let token = s.current();
            if token.code == T![=] {
                let t_eq = token.clone();
                s.plusplus();

                let token = s.current();
                let expr = match &token.code {
                    T!["{"] => {
                        let a = s.parse_block_expr()?;
                        ConstParamExpr::BlockExpr(a)
                    }
                    TokenCode::Identifier(_) => {
                        s.plusplus();
                        ConstParamExpr::Identifier(token.clone())
                    }
                    TokenCode::Literal(_) => {
                        s.plusplus();
                        ConstParamExpr::Literal((None, ExprLiteral(token.clone())))
                    }
                    T![-] => {
                        let t_sep = Some(token.clone());
                        s.plusplus();
                        let token = s.current();
                        if !matches!(&token.code, TokenCode::Literal(_)) {
                            return Err(s.panic(token,
                                &format!("illegal token {:?} after - for const param", token)));
                        }
                        s.plusplus();
                        ConstParamExpr::Literal((t_sep, ExprLiteral(token.clone())))
                    }
                    _ => {
                        return Err(s.panic(token,
                            &format!("illegal token {:?} for const param", token)));
                    }
                };
                r.expr = Some((t_eq, expr));
            }
            let token = s.current();
            if !matches!(&token.code, T![>] | T![,]) {
                return Err(s.panic(token, &format!("expected > or , but got {:?}", token)));
            }

            Ok(r)
        }
    }

    // --------------------------------------------------------------------------------
    // bounds

    fn parse_lifetime_bounds(&self) -> ParseResult<LifetimeBounds> {
        let s = self;

        let mut r = LifetimeBounds::default();

        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T![>] | T![,] | T![;]) {
                break;
            }
            if !matches!(&token.code, TokenCode::Lifetime(_)) {
                return Err(s.panic(
                    token,
                    &format!("expected lifetime but got '{:?}'", token),
                ));
            }
            let lifetime = Lifetime {
                name: token.clone(),
            };
            s.plusplus();

            let token = s.current();
            if matches!(&token.code, T![>] | T![,] | T![;] | T!["{"]) {
                r.items.push((lifetime, None));
                break;
            }
            if !matches!(&token.code, T![+]) {
                return Err(s.panic(
                    token,
                    &format!("expected '+' but got '{:?}'", token),
                ));
            }
            s.plusplus();

            r.items.push((lifetime, Some(token.clone())));
        }

        Ok(r)
    }

    fn parse_type_param_bounds(&self) -> ParseResult<TypeParamBounds> {
        let s = self;

        let mut r = TypeParamBounds::default();

        while s.has_more() {
            let token = s.current();

            let mut type_bound = TypeParamBound::Lifetime(Lifetime::default());

            if let TokenCode::Lifetime(_) = &token.code {
                type_bound = TypeParamBound::Lifetime(Lifetime {
                    name: token.clone(),
                });
                s.plusplus();
            } else if matches!(&token.code, T![use]) {
                let t_use = token.clone();
                s.plusplus();
                let token = s.current();
                let mut t_square_open = token.clone();
                t_square_open.code = T!["["];
                s.expect(T![<])?;
                let mut items = Vec::new();
                while s.has_more() {
                    let token = s.current();

                    if matches!(&token.code, T![>]) {
                        s.plusplus();
                        let mut t_square_close = token.clone();
                        t_square_close.code = T!["]"];
                        let use_bound = UseBound {
                            items,
                            t_use,
                            t_square_open,
                            t_square_close,
                        };
                        type_bound = TypeParamBound::UseBound(use_bound);
                        break;
                    }

                    if matches!(&token.code,
                            TokenCode::Lifetime(_) | TokenCode::Identifier(_) | T![Self]) {
                        let one_token = token.clone();
                        s.plusplus();
                        let token = s.current();
                        if matches!(&token.code, T![,]) {
                            items.push((one_token, Some(token.clone())));
                            s.plusplus();
                        } else if matches!(&token.code, T![>]) {
                            items.push((one_token, None));
                        }
                    } else {
                        return Err(s.panic(
                            token,
                            &format!("illegal token {:?} for use bound", token),
                        ));
                    }
                }
            } else {
                let bound = s.parse_trait_bound()?;
                type_bound = TypeParamBound::TraitBound(bound);
            }

            let token = s.current();
            if !matches!(&token.code, T![+]) {
                r.items.push((type_bound, None));
                break;
            }
            s.plusplus();
            r.items.push((type_bound, Some(token.clone())));
        }

        Ok(r)
    }

    fn parse_trait_bound(&self) -> ParseResult<TraitBound> {
        let s = self;
        let mut r = TraitBound::default();

        let token = s.current();
        if matches!(&token.code, T!["("]) {
            r.t_paren_open = Some(token.clone());
            s.plusplus();
        }

        let token = s.current();
        if matches!(&token.code, T![?]) {
            r.t_unsized = Some(token.clone());
            s.plusplus();
        }

        let token = s.current();
        if matches!(&token.code, T![for]) {
            let f = s.parse_for_lifetimes()?;
            r.for_lifetimes = Some(f);
        }

        r.type_path = s.parse_type_path()?;

        let token = s.current();
        if r.t_paren_open.is_some() {
            r.t_paren_close = Some(token.clone());
            s.expect(T![")"])?;
        }

        Ok(r)
    }

    fn parse_for_lifetimes(&self) -> ParseResult<ForLifetimes> {
        let s = self;
        let mut r = ForLifetimes::default();
        r.t_for = s.current().clone();
        s.plusplus();

        let token = s.current();
        if !matches!(&token.code, T![<]) {
            return Err(s.panic(&token, &format!("expected '<', but got {:?}", token)));
        }
        r.for_ = s.parse_generics()?;

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    // items
    fn parse_mod(&self) -> ParseResult<Mod> {
        let s = self;

        let mut r = Mod::default();

        s.expect(T![mod])?;

        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }

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
            let stmt = s.parse_stmt(r.items.len())?;
            let item = match stmt {
                Stmt::Item(a) => {
                    a
                }
                Stmt::Value(a) => {
                    let is_macro_call = match &a.expr.payload {
                        ExprPayload::WithoutBlock(b) => {
                            if matches!(b.as_ref(), ExprWithoutBlock::MacroCall(_)) {
                                true
                            } else {
                                false
                            }
                        }
                        _ => {
                            false
                        }
                    };
                    if !is_macro_call {
                        return Err(s.panic(token, "expected Item"));
                    }
                    Item {
                        outer_attrs: Attrs::default(),
                        payload: ItemPayload::MacroCall(a.expr),
                    }
                }
                _ => {
                    return Err(s.panic(token, "expected Item"));
                }
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
        if !matches!(&r.name.code, TokenCode::Identifier(_) | T![self]) {
            return Err(s.panic(&r.name,
                &format!("expected identifier or 'self' after 'crate', but got {:?}", r.name)));
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

        s.expect(T![use])?;

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
                match &token.code {
                    T![as] => {
                        s.plusplus();
                        r.alias = Some((token.clone(), s.current().clone()));
                        s.plusplus(); // identifier
                        return Ok(r);
                    }
                    T![;] | T![,] | T!["}"] => {
                        return Ok(r);
                    }
                    T!["{"] => {
                        r.sub = Some(parse_use_item_sub(s)?);
                        return Ok(r);
                    }
                    _ => {}
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

                let token = s.current();
                match &token.code {
                    T![,] => {
                        s.plusplus();
                        let show_comma = true;
                        r.items.push((it, show_comma));
                        let token = s.current();
                        if matches!(&token.code, T!["}"]) {
                            r.t_brace_close = token.clone();
                            s.plusplus();
                            return Ok(r);
                        }
                    }
                    T!["}"] => {
                        s.plusplus();
                        r.items.push((it, false));
                        r.t_brace_close = token.clone();
                        return Ok(r);
                    }
                    _ => {
                        return Err(s.panic(token, "illegal token in use"));
                    }
                }
            }

            return Ok(r);
        }
    }

    fn parse_extern(&self) -> ParseResult<ExternalBlock> {
        let s = self;

        let mut r = ExternalBlock::default();

        let token = s.current();
        if matches!(&token.code, T![unsafe]) {
            r.t_unsafe = Some(token.clone());
            s.plusplus();
        }
        s.expect(T![extern])?;

        // possible string literal, i.e. "C"
        let token = s.current();
        if let TokenCode::Literal(Literal {
            payload: LiteralPayload::String(_),
            ..
        }) = &token.code {
            r.t_abi = Some(token.clone());
            s.plusplus();
        }

        // items
        r.t_brace_open = s.current().clone();
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                r.t_brace_close = token.clone();
                s.plusplus();
                break;
            }
            let token = s.current();
            let stmt = s.parse_stmt(r.items.len())?;
            match stmt {
                Stmt::Item(item) => match item.payload {
                    ItemPayload::Func(a) => {
                        r.items.push(ExternalBlockItem {
                            outer_attrs: item.outer_attrs,
                            payload: ExternalBlockItemPayload::Func(*a),
                        });
                    }
                    ItemPayload::Static(a) => {
                        r.items.push(ExternalBlockItem {
                            outer_attrs: item.outer_attrs,
                            payload: ExternalBlockItemPayload::Static(a),
                        });
                    }
                    ItemPayload::MacroCall(a) => {
                        r.items.push(ExternalBlockItem {
                            outer_attrs: item.outer_attrs,
                            payload: ExternalBlockItemPayload::MacroCall(a),
                        });
                    }
                    _ => return Err(s.panic(token, "illegal extern stmt item")),
                },
                Stmt::Value(item) => {
                    let mut is_macro_call = false;
                    if let ExprPayload::WithoutBlock(b) = &item.expr.payload {
                        if let ExprWithoutBlock::MacroCall(_) = b.as_ref() {
                            is_macro_call = true;
                        }
                    }
                    if is_macro_call {
                        r.items.push(ExternalBlockItem {
                            outer_attrs: Attrs::default(),
                            payload: ExternalBlockItemPayload::MacroCall(item.expr),
                        });
                    } else {
                        return Err(s.panic(token, "illegal extern stmt value"));
                    }
                }
                _ => return Err(s.panic(token, "illegal extern stmt")),
            }

            s.skip_semicolons();
            if !s.has_more() {
                return Err(s.panic(&s.tokens[s.tokens.len() - 1], "expected }"));
            }
        }

        Ok(r)
    }

    // const? async? [safe|unsafe]? (extern Abi?)? fn
    fn parse_func(&self) -> ParseResult<Func> {
        let s = self;

        let mut r = Func::default();

        r.t_first = s.current().clone();

        // qualifier
        let token = s.current();
        if matches!(&token.code, T![const]) {
            r.qualifier.t_const = Some(token.clone());
            r.qualifier.exist = true;
            s.plusplus();
        }
        let token = s.current();
        if matches!(&token.code, T![async]) {
            r.qualifier.t_async = Some(token.clone());
            r.qualifier.exist = true;
            s.plusplus();
        }
        let token = s.current();
        if token.u == "safe" || token.u == "unsafe" {
            r.qualifier.t_unsafe = Some(token.clone());
            r.qualifier.exist = true;
            s.plusplus();
        }
        let token = s.current();
        if matches!(&token.code, T![extern]) {
            r.qualifier.exist = true;
            s.plusplus();
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

        s.expect(T![fn])?;

        // name
        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }

        // possible GenericsParams
        if matches!(s.current().code, T![<]) {
            r.generics = Some(s.parse_generics()?);
        }

        // params
        s.parse_func_params(&mut r)?;

        // return type
        let token = s.current();
        if matches!(&token.code, T![->]) {
            s.minusminus();
            let t_last = s.current();
            s.plusplus();
            if t_last.span.end_line != token.span.line {
                r.show_arrow = true;
            }

            s.plusplus();
            let type_ = s.parse_type()?;
            r.return_type = Some((token.clone(), type_));
        }

        // where
        let token = s.current();
        if matches!(&token.code, T![where]) {
            r.where_ = Some(s.parse_where()?);
        }

        let token = s.current();
        if matches!(&token.code, T![;]) {
            return Ok(r);
        }

        // body
        let mut a = s.parse_block_expr()?;
        if r.self_param.is_some() && !a.stmts.is_empty() {
            // check if the first stmt is
            // let s = self;
            r.qualifier.no_s = true;
            r.qualifier.exist = true;
            let first = &a.stmts[0];
            if let Stmt::Let(a) = first &&
                let Some((name, None)) = &a.short &&
                name.u == "s" &&
                let Some(expr) = &a.expr &&
                let ExprPayload::WithoutBlock(a) = &expr.payload &&
                let ExprWithoutBlock::Path(a) = a.as_ref() &&
                let ExprPath::Path(a) = a &&
                !a.items.is_empty()
            {
                let a = &a.items[0];
                if a.t_sep.is_none() &&
                    a.generic_args.items.is_empty() &&
                    matches!(a.name.token.code, T![self])
                {
                    r.qualifier.no_s = false;
                    r.qualifier.exist = false;
                }
            }

            if !r.qualifier.no_s {
                a.stmts.remove(0);
            }
        }
        r.body = Some(a);

        Ok(r)
    }

    fn parse_func_params(&self, f: &mut Func) -> ParseResult<()> {
        let s = self;

        f.t_paren_open = s.current().clone();
        s.expect(T!["("])?;
        f.t_first_after_paren = s.current().clone();

        // self param
        let save_state = s.save_state();
        let outer_attrs = s.parse_outer_attrs()?;
        let token = s.current();
        match &token.code {
            T![&] => {
                let mut i = 1;
                let label = if matches!(s.nth(i).code, TokenCode::Lifetime(_)) {
                    i += 1;
                    Some(s.nth(i - 1).clone())
                } else {
                    None
                };
                if matches!(s.nth(i).code, T![self]) {
                    s.inc(i + 1);
                    f.self_param = Some(SelfParam {
                        outer_attrs,
                        payload: SelfParamPayload::Short(SelfParamShort{
                            t_ref: Some(token.clone()),
                            label: label,
                            t_mut: None,
                        }),
                        t_first_after_attr: token.clone(),
                        has_comma: false,
                    });
                } else if matches!(s.nth(i).code, T![mut]) {
                    let t_mut = Some(s.nth(i).clone());
                    i += 1;
                    if matches!(s.nth(i).code, T![self]) {
                        s.inc(i + 1);
                        f.self_param = Some(SelfParam {
                            outer_attrs,
                            payload: SelfParamPayload::Short(SelfParamShort{
                                t_ref: Some(token.clone()),
                                label: label,
                                t_mut,
                            }),
                            t_first_after_attr: token.clone(),
                            has_comma: false,
                        });
                    }
                }
            }
            T![mut] => {
                if matches!(s.nth(1).code, T![self]) {
                    if matches!(s.nth(2).code, T![:]) {
                        s.inc(3);
                        let typ = s.parse_type()?;
                        f.self_param = Some(SelfParam {
                            outer_attrs,
                            payload: SelfParamPayload::Type(SelfParamType{
                                typ,
                                t_mut: Some(token.clone()),
                            }),
                            t_first_after_attr: token.clone(),
                            has_comma: false,
                        });
                    } else {
                        s.inc(2);
                        f.self_param = Some(SelfParam {
                            outer_attrs,
                            payload: SelfParamPayload::Short(SelfParamShort{
                                t_ref: None,
                                label: None,
                                t_mut: Some(token.clone()),
                            }),
                            t_first_after_attr: token.clone(),
                            has_comma: false,
                        });
                    }
                }
            }
            T![self] => {
                if matches!(s.nth(1).code, T![:]) {
                    s.inc(2);
                    let typ = s.parse_type()?;
                    f.self_param = Some(SelfParam {
                        outer_attrs,
                        payload: SelfParamPayload::Type(SelfParamType{
                            typ,
                            t_mut: None,
                        }),
                        t_first_after_attr: token.clone(),
                        has_comma: false,
                    });
                } else {
                    s.inc(1);
                    f.self_param = Some(SelfParam {
                        outer_attrs,
                        payload: SelfParamPayload::Short(SelfParamShort{
                            t_ref: None,
                            label: None,
                            t_mut: None,
                        }),
                        t_first_after_attr: token.clone(),
                        has_comma: false,
                    });
                }
            }
            _ => {}
        }
        if let Some(sp) = &mut f.self_param {
            if matches!(s.current().code, T![,]) {
                s.plusplus();
                sp.has_comma = true;
            }
        } else {
            s.restore_state(&save_state);
        }

        // other params
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T![")"]) {
                f.t_paren_close = token.clone();
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
                f.variadics = Some(FuncVariadics {
                    outer_attrs: param.outer_attrs,
                    t_var,
                    t_comma,
                });
                f.t_paren_close = s.current().clone();
                s.expect(T![")"])?;
                break;
            }

            // Pattern: Type
            param.t_first_in_pattern = s.current().clone();
            let pat = s.parse_pattern_no_top_alt()?;
            // check if it's the short form
            let mut is_short = false;
            if let PatternNoTopAlt::WithoutRange(pat) = &pat {
                if let PatternWithoutRange::Identifier(a) = pat
                    && a.pattern.is_none()
                    && a.t_ref.is_none()
                    && a.t_mut.is_none()
                {
                    param.name = a.name.clone();
                    is_short = true;
                } else if let PatternWithoutRange::Path(a) = pat
                    && let PatternPath::Path(a) = a
                    && a.items.len() == 1
                {
                    let name = a.items[0].name.token.clone();
                    param.name = name.clone();
                    is_short = true;
                }
            }
            if !is_short {
                param.pat = Some(Pattern {
                    items: vec![(pat, None)],
                });
            }

            s.expect(T![:])?;
            let token = s.current();
            if matches!(&token.code, T![...]) {
                s.plusplus();
                param.t_var = Some(token.clone());
            } else {
                param.type_ = s.parse_type()?;
            }

            let token = s.current();
            s.plusplus();
            match &token.code {
                T![")"] => {
                    f.t_paren_close = token.clone();
                    f.params.push(param);
                    break;
                }
                T![,] => {
                    param.show_comma = true;
                    f.params.push(param);
                }
                _ => {
                    return Err(s.panic(token, &format!("expected , or ) but got {:?}", token)));
                }
            }
        }

        Ok(())
    }

    // type IDENTIFIER GenericParams? ( : Bounds? )?
    //      WhereClause?
    //      ( = Type WhereClause? )? ;
    fn parse_type_alias(&self) -> ParseResult<TypeAlias> {
        let s = self;

        let mut r = TypeAlias::default();

        s.expect(T![type])?;

        // name
        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }

        // possible GenericsParams
        if matches!(s.current().code, T![<]) {
            r.generics = Some(s.parse_generics()?);
        }

        // possible bounds
        let token = s.current();
        if matches!(&token.code, T![:]) {
            s.plusplus();
            r.bounds = s.parse_type_param_bounds()?;
        }

        // possible where clause before =
        let token = s.current();
        if matches!(&token.code, T![where]) {
            r.where_ = Some(s.parse_where()?);
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
                type_where = Some(s.parse_where()?);
            }
            r.rhs = Some((t_eq, type_, type_where));
        }
        s.expect(T![;])?;

        Ok(r)
    }

    fn parse_struct(&self) -> ParseResult<Struct> {
        let s = self;

        let mut r = Struct::default();

        s.expect(T![struct])?;

        // name
        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }

        // possible GenericsParams
        if matches!(s.current().code, T![<]) {
            r.generics = Some(s.parse_generics()?);
        }

        let token = s.current();
        // tuple
        if matches!(&token.code, T!["("]) {
            let t_o = token.clone();
            s.plusplus();
            let fields = s.parse_tuple_fields()?;
            let t_c = s.current().clone();
            s.expect(T![")"])?;
            r.payload = Some((StructPayload::Tuple(fields), t_o, t_c));

            if matches!(&s.current().code, T![where]) {
                r.where_ = Some(s.parse_where()?);
            }
            s.expect(T![;])?;
            return Ok(r);
        }

        // struct
        if matches!(&s.current().code, T![where]) {
            r.where_ = Some(s.parse_where()?);
        }
        if matches!(&s.current().code, T![;]) {
            return Ok(r);
        }

        let t_o = s.current().clone();
        s.expect(T!["{"])?;
        let fields = s.parse_struct_fields()?;
        let t_c = s.current().clone();
        s.expect(T!["}"])?;
        r.payload = Some((StructPayload::Struct(fields), t_o, t_c));

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

            field.vis = s.parse_vis()?;

            let token = s.current();
            let TokenCode::Identifier(_) = &token.code else {
                return Err(s.panic(token, &format!("expected identifier but got {:?}", token)));
            };
            field.name = token.clone();
            s.plusplus();

            s.expect(T![:])?;

            field.type_ = s.parse_type()?;

            let token = s.current();
            if matches!(&token.code, T![,]) {
                s.plusplus();
                let next_token = s.current();
                field.show_comma = next_token.span.line == token.span.line;
            } else if matches!(&token.code, T!["}"]) {
                s.minusminus();
                let before_token = s.current();
                s.plusplus();
                field.show_backslash = before_token.span.line != token.span.line;
            } else {
                return Err(s.panic(token, &format!("illegal token after field {:?}", token)));
            }
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

            let outer_attrs = s.parse_outer_attrs()?;
            let vis = s.parse_vis()?;
            let t_first_in_type = s.current().clone();
            let type_ = s.parse_type()?;

            let token = s.current();
            let t_comma = if matches!(&token.code, T![,]) {
                s.plusplus();
                Some(token.clone())
            } else if !matches!(&token.code, T![")"]) {
                return Err(s.panic(token, &format!("illegal token after field {:?}", token)));
            } else {
                None
            };

            r.push(TupleField {
                outer_attrs,
                vis,
                type_,
                t_comma,
                t_first_in_type,
            });
        }

        Ok(r)
    }

    fn parse_enum(&self) -> ParseResult<Enum> {
        let s = self;

        let mut r = Enum::default();

        s.expect(T![enum])?;

        // name
        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }

        // possible GenericsParams
        if matches!(s.current().code, T![<]) {
            r.generics = Some(s.parse_generics()?);
        }

        if matches!(&s.current().code, T![where]) {
            r.where_ = Some(s.parse_where()?);
        }

        r.t_brace_open = s.current().clone();
        s.expect(T!["{"])?;
        r.items = parse_enum_items(s)?;
        r.t_brace_close = s.current().clone();
        s.expect(T!["}"])?;

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
                let TokenCode::Identifier(_) = &token.code else {
                    return Err(s.panic(token, &format!("expected identifier but got {:?}", token)));
                };
                field.name = token.clone();
                s.plusplus();

                // field.vis = s.parse_vis()?;

                let token = s.current();
                match &token.code {
                    T!["{"] => {
                        let t_o = token.clone();
                        s.plusplus();
                        let fields = s.parse_struct_fields()?;
                        let t_c = s.current().clone();
                        s.expect(T!["}"])?;
                        field.payload = Some((EnumItemPayload::Struct(fields), t_o, t_c));
                    }
                    T!["("] => {
                        let t_o = token.clone();
                        s.plusplus();
                        let fields = s.parse_tuple_fields()?;
                        let t_c = s.current().clone();
                        s.expect(T![")"])?;
                        field.payload = Some((EnumItemPayload::Tuple(fields), t_o, t_c));
                    }
                    _ => (),
                }

                let token = s.current();
                if matches!(token.code, T![=]) {
                    let t_eq = token.clone();
                    s.plusplus();
                    let mut config = ParseConfig::default();
                    config.may_end_with_semicolon = true;
                    let expr = s.parse_expr_config(&config)?;
                    field.discriminant = Some((t_eq, expr));
                }

                let token = s.current();
                if matches!(&token.code, T![,]) {
                    s.plusplus();
                    let next_token = s.current();
                    let show_comma = next_token.span.line == token.span.line;
                    field.show_comma = show_comma;
                } else if !matches!(&token.code, T![")"]) {
                    return Err(s.panic(token, &format!("illegal token after field {:?}", token)));
                }
                r.push(field);
            }

            Ok(r)
        }
    }

    fn parse_union(&self) -> ParseResult<Union> {
        let s = self;

        let mut r = Union::default();

        // union
        s.plusplus();

        // name
        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }

        // possible GenericsParams
        if matches!(s.current().code, T![<]) {
            r.generics = Some(s.parse_generics()?);
        }

        if matches!(&s.current().code, T![where]) {
            r.where_ = Some(s.parse_where()?);
        }
        if matches!(&s.current().code, T![;]) {
            return Ok(r);
        }

        r.t_brace_open = s.current().clone();
        s.expect(T!["{"])?;
        r.fields = s.parse_struct_fields()?;
        r.t_brace_close = s.current().clone();
        s.expect(T!["}"])?;

        Ok(r)
    }

    // const IDENTIFIER : Type ( = Expr )? ;
    fn parse_const(&self) -> ParseResult<Const> {
        let s = self;

        let mut r = Const::default();

        s.expect(T![const])?;

        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }
        s.expect(T![:])?;

        r.type_ = s.parse_type()?;

        let token = s.current();
        if matches!(&token.code, T![;]) {
            return Ok(r);
        }

        s.expect(T![=])?;

        let t_eq = token.clone();
        let mut config = ParseConfig::default();
        config.may_end_with_semicolon = true;
        let expr = s.parse_expr_config(&config)?;
        r.expr = Some((t_eq, expr));
        s.expect(T![;])?;

        Ok(r)
    }

    // [safe|unsafe]? static mut? IDENTIFIER : Type ( = Expr )? ;
    fn parse_static(&self) -> ParseResult<Static> {
        let s = self;

        let mut r = Static::default();

        let token = s.current();
        if matches!(&token.code, T![unsafe]) {
            r.t_safe = Some(token.clone());
            s.plusplus();
        } else if let TokenCode::Identifier(a) = &token.code &&
                a.prefix.is_none() && a.id == "safe" {
            r.t_safe = Some(token.clone());
            s.plusplus();
        }

        s.expect(T![static])?;

        let token = s.current();
        if matches!(&token.code, T![mut]) {
            r.t_mut = Some(token.clone());
            s.plusplus();
        }
        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }
        s.expect(T![:])?;

        r.type_ = s.parse_type()?;

        let token = s.current();
        if matches!(&token.code, T![=]) {
            let t_eq = token.clone();
            s.expect(T![=])?;
            let mut config = ParseConfig::default();
            config.may_end_with_semicolon = true;
            let expr = s.parse_expr_config(&config)?;
            r.expr = Some((t_eq, expr));
        }
        s.expect(T![;])?;

        Ok(r)
    }

    fn parse_trait(&self) -> ParseResult<Trait> {
        let s = self;

        let mut r = Trait::default();

        let token = s.current();
        if matches!(&token.code, T![unsafe]) {
            r.t_unsafe = Some(token.clone());
            s.plusplus();
        }
        s.expect(T![trait])?;

        // name
        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }

        // possible GenericsParams
        if matches!(s.current().code, T![<]) {
            r.generics = Some(s.parse_generics()?);
        }

        // possible TypeParamBounds
        if matches!(&s.current().code, T![:]) {
            s.plusplus();
            r.bounds = s.parse_type_param_bounds()?;
        }

        // possible where clause
        if matches!(&s.current().code, T![where]) {
            r.where_ = Some(s.parse_where()?);
        }

        r.t_brace_open = s.current().clone();
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                r.t_brace_close = token.clone();
                s.plusplus();
                break;
            }
            let token = s.current();
            let stmt = s.parse_stmt(r.items.len())?;
            match stmt {
                Stmt::Item(item) => match item.payload {
                    ItemPayload::Func(f) => {
                        let a = AssociatedItem {
                            outer_attrs: item.outer_attrs,
                            payload: AssociatedItemPayload::Func(*f),
                        };
                        r.items.push(a);
                    }
                    ItemPayload::Const(f) => {
                        let a = AssociatedItem {
                            outer_attrs: item.outer_attrs,
                            payload: AssociatedItemPayload::Const(f),
                        };
                        r.items.push(a);
                    }
                    ItemPayload::TypeAlias(f) => {
                        let a = AssociatedItem {
                            outer_attrs: item.outer_attrs,
                            payload: AssociatedItemPayload::TypeAlias(f),
                        };
                        r.items.push(a);
                    }
                    ItemPayload::MacroCall(f) => {
                        let a = AssociatedItem {
                            outer_attrs: item.outer_attrs,
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

        let token = s.current();
        if matches!(&token.code, T![unsafe]) {
            impl_trait.t_unsafe = Some(token.clone());
            s.plusplus();
        }
        s.expect(T![impl])?;

        if matches!(&s.current().code, T![<]) {
            r.generics = Some(s.parse_generics()?);
        }

        let token = s.current();
        if matches!(&token.code, T![!]) {
            impl_trait.t_not = Some(token.clone());
            s.plusplus();
        }

        let type_ = s.parse_type()?;

        let token = s.current();
        if matches!(&token.code, T![for]) {
            if let TypePayload::TypeNoBounds(a) = type_.payload &&
                    let TypeNoBounds::TypePath(b) = *a {
                impl_trait.type_path = b;
            } else {
                return Err(s.panic(&s.current(), "Type is not TypePath in impl"));
            }
            impl_trait.t_for = token.clone();
            s.plusplus();
            r.type_ = s.parse_type()?;
            r.payload = ImplPayload::Trait(impl_trait);
        } else {
            r.type_ = type_;
            r.payload = ImplPayload::Static;
        }

        // possible where clause
        if matches!(&s.current().code, T![where]) {
            r.where_ = Some(s.parse_where()?);
        }

        r.t_brace_open = s.current().clone();
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T!["}"]) {
                r.t_brace_close = token.clone();
                s.plusplus();
                break;
            }
            let stmt = s.parse_stmt(r.items.len())?;
            match stmt {
                Stmt::Item(item) => {
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
                        payload,
                    });
                }
                Stmt::Value(item) => {
                    let mut is_macro_call = false;
                    if let ExprPayload::WithoutBlock(b) = &item.expr.payload {
                        if let ExprWithoutBlock::MacroCall(_) = b.as_ref() {
                            is_macro_call = true;
                        }
                    }
                    if is_macro_call {
                        r.items.push(AssociatedItem {
                            outer_attrs: Attrs::default(),
                            payload: AssociatedItemPayload::MacroCall(item.expr),
                        });
                    } else {
                        return Err(s.panic(token, "illegal extern stmt value"));
                    }
                }
                _ => {
                    return Err(s.panic(&token, "illegal impl item"));
                }
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
        r.t_macro = s.current().clone();

        s.plusplus();
        s.expect(T![!])?;

        let token = s.current();
        r.name = token.clone();
        s.plusplus();
        if !matches!(&token.code, TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("expected identifier but got {:?}", token),
            ));
        }

        let token = s.current();
        r.t_delim_open = token.clone();
        let delim = match &token.code {
            T!["{"] => 0,
            T!["("] => 1,
            T!["["] => 2,
            _ => {
                return Err(s.panic(
                    token,
                    &format!("expected '(', '[', or '{{' , but got {:?}", token),
                ));
            }
        };
        s.plusplus();

        let mut delim_count = 1;
        while s.has_more() {
            let token = s.current();
            s.plusplus();

            if delim == 0 {
                match &token.code {
                    T!["}"] => {
                        delim_count -= 1;
                        if delim_count == 0 {
                            r.t_delim_close = token.clone();
                            break;
                        }
                        r.tokens.push(token.clone());
                        continue;
                    }
                    T!["{"] => {
                        delim_count += 1;
                        r.tokens.push(token.clone());
                        continue;
                    }
                    _ => {}
                }
            } else if delim == 1 {
                match &token.code {
                    T![")"] => {
                        delim_count -= 1;
                        if delim_count == 0 {
                            r.t_delim_close = token.clone();
                            break;
                        }
                        r.tokens.push(token.clone());
                        continue;
                    }
                    T!["("] => {
                        delim_count += 1;
                        r.tokens.push(token.clone());
                        continue;
                    }
                    _ => {}
                }
            } else if delim == 2 {
                match &token.code {
                    T!["]"] => {
                        delim_count -= 1;
                        if delim_count == 0 {
                            r.t_delim_close = token.clone();
                            break;
                        }
                        r.tokens.push(token.clone());
                        continue;
                    }
                    T!["["] => {
                        delim_count += 1;
                        r.tokens.push(token.clone());
                        continue;
                    }
                    _ => {}
                }
            }

            r.tokens.push(token.clone());
        }

        Ok(r)
    }

    fn parse_macro_call(&self, path: SimplePath, token: &Token) -> ParseResult<MacroCall> {
        let s = self;

        let mut r = MacroCall::default();
        r.path = path;
        r.t_macro_call = token.clone();

        let delim_close;
        let token = s.current();
        match &token.code {
            T!["("] => {
                r.delim = MacroCallDelim::Paren;
                delim_close = Pair::ParenClose;
            }
            T!["["] => {
                r.delim = MacroCallDelim::Square;
                delim_close = Pair::SquareClose;
            }
            T!["{"] => {
                r.delim = MacroCallDelim::Brace;
                delim_close = Pair::BraceClose;
            }
            _ => {
                return Err(s.panic(
                    token,
                    &format!("illegal delimiter {} for macro call", token.u),
                ));
            }
        }

        r.t_delim_open = token.clone();
        let save_state = s.save_state();

        // (expr, expr, ...)
        if let Ok(exprs) = parse_exprs(s, &delim_close) {
            r.body = MacroCallBody::Expr(exprs);
            r.t_delim_close = s.current().clone();
            s.plusplus();
            return Ok(r);
        } else {
            s.restore_state(&save_state);
        }

        // stmts
        /* if let Ok(block) = parse_stmts(s, &delim_close) {
            r.body = MacroCallBody::Stmt(block);
            r.t_delim_close = s.current().clone();
            s.plusplus();
            return Ok(r);
        } else {
            s.restore_state(&save_state);
        } */

        // { tokens }
        let i_cur = s.index.get();
        let start = s.skip_pair(i_cur)?;
        let tokens: Vec<Token> = s.tokens[i_cur + 1..start - 1].to_vec();
        s.index.set(start - 1);
        r.t_delim_close = s.current().clone();
        s.plusplus();
        r.body = MacroCallBody::Token(tokens);
        return Ok(r);

        fn parse_exprs(s: &Parse, delim_close: &Pair) -> ParseResult<Vec<(Expr, bool)>> {
            let mut r = Vec::new();

            // check for consecutive --
            let i_cur = s.index.get();
            let start = s.skip_pair(i_cur)?;
            let tokens: Vec<Token> = s.tokens[i_cur + 1..start - 1].to_vec();
            let mut has_minus = false;
            for it in &tokens {
                if matches!(it.code, T![-]) {
                    if has_minus {
                        if it.tokens_before.is_empty() {
                            return Err(s.panic(it, "--"));
                        }
                    } else {
                        has_minus = true;
                    }
                } else {
                    has_minus = false;
                }
            }

            s.plusplus();

            while s.has_more() {
                let token = s.current();
                if let TokenCode::Pair(pair) = &token.code && *pair == *delim_close {
                    break;
                }

                let expr = s.parse_expr()?;
                let token = s.current();
                if matches!(&token.code, T![,]) {
                    s.plusplus();
                    r.push((expr, true));
                } else if let TokenCode::Pair(pair) = &token.code && *pair == *delim_close {
                    r.push((expr, false));
                } else {
                    return Err(s.panic(&token, "expected , or delim_close for ending exprs"));
                }
            }

            Ok(r)
        }

        /* fn parse_stmts(s: &Parse, delim_close: &Pair) -> ParseResult<MacroCallBodyStmt> {
            let mut r = MacroCallBodyStmt::default();

            s.plusplus();
            r.inner_attrs = s.parse_inner_attrs()?;

            while s.has_more() {
                let token = s.current();
                if let TokenCode::Pair(pair) = &token.code && *pair == *delim_close {
                    return Ok(r);
                }
                let stmt = s.parse_stmt(r.stmts.len())?;
                r.stmts.push(stmt);
                s.skip_semicolons();
                if !s.has_more() {
                    return Err(s.panic(&token, "expected } for ending block expr"));
                }
            }

            return Ok(r);
        } */
    }

    // --------------------------------------------------------------------------------
    // stmts

    // after continue
    fn parse_continue(&self) -> ParseResult<ExprContinue> {
        let s = self;

        let mut r = ExprContinue::default();

        let token = s.current();
        if let TokenCode::Lifetime(_) = &token.code {
            r.label = Some(token.clone());
            s.plusplus();
        }

        let token = s.current();
        if matches!(&token.code, T![;] | T![,] | T!["}"]) {
            return Ok(r);
        }

        Ok(r)
    }

    // after break
    fn parse_break(&self) -> ParseResult<ExprBreak> {
        let s = self;

        let mut r = ExprBreak::default();

        // parse label
        let token = s.current();
        if let TokenCode::Lifetime(_) = &token.code {
            s.plusplus();
            r.label = Some(token.clone());
        }

        let token = s.current();
        if matches!(&token.code, T![;] | T![,] | T!["}"]) {
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
        if matches!(&token.code, T![;] | T![,] | T!["}"]) {
            return Ok(r);
        }

        // parse expr
        r.expr = Some(s.parse_expr()?);

        Ok(r)
    }

    // async move? {}
    fn parse_async(&self) -> ParseResult<ExprAsyncBlock> {
        let s = self;

        let mut r = ExprAsyncBlock::default();
        r.t_async = s.current().clone();
        s.plusplus();

        let token = s.current();
        if matches!(&token.code, T![move]) {
            r.t_move = Some(token.clone());
            s.plusplus();
        }

        r.block = s.parse_block_expr()?;

        Ok(r)
    }

    fn parse_for(&self) -> ParseResult<ExprLoop> {
        let s = self;

        let mut r = ExprLoop::default();

        let token = s.current();
        r.t_for = token.clone();
        r.t_for.code = T![for];
        s.plusplus();
        match &token.code {
            T![loop] => {
                r.payload = ExprLoopPayload::Loop;
            }
            T![while] => {
                let i_start = s.index.get();
                let conditions = s.parse_conditions(false)?;
                let i_end = s.index.get();
                let has_brace = s.tokens[i_start..i_end]
                    .iter().any(|it| matches!(it.code, T!["{"]));
                r.payload = ExprLoopPayload::While(ExprLoopWhile { conditions, has_brace });
            }
            T![for] => {
                let i_start = s.index.get();
                let pattern = s.parse_pattern()?;
                let i_end = s.index.get();
                let pat_has_brace = s.tokens[i_start..i_end]
                    .iter().any(|it| matches!(it.code, T!["{"]));
                let t_in = s.current().clone();
                s.expect(T![in])?;

                let mut config = ParseConfig::default();
                config.stop_on_struct_brace = true;
                let in_expr = s.parse_expr_config(&config)?;

                r.payload = ExprLoopPayload::In(ExprLoopIn {
                    pattern,
                    in_expr,
                    pat_has_brace,
                    t_in,
                });
            }
            _ => {
            }
        }

        // for auto completion
        if s.magic && matches!(&s.current().code, T![;]) {
            return Ok(r);
        }

        r.body = s.parse_block_expr()?;

        Ok(r)
    }

    fn parse_conditions(&self, is_match: bool) -> ParseResult<Conditions> {
        let s = self;

        // the only difference when is_match == true is it allows ExprStruct

        let mut r = Vec::new();

        let save_state = s.save_state();
        if is_match {
            let a = s.parse_expr();
            match a {
                Ok(expr) => {
                    r.push((ConditionItem::Expr(expr), None));
                    return Ok(Conditions {
                        items: r,
                    });
                }
                Err(_) => {
                    s.restore_state(&save_state);
                }
            }
        } else {
            let mut config = ParseConfig::default();
            config.stop_on_struct_brace = true;
            let a = s.parse_expr_config(&config);
            match a {
                Ok(expr) => {
                    r.push((ConditionItem::Expr(expr), None));
                    return Ok(Conditions {
                        items: r,
                    });
                }
                Err(_) => {
                    s.restore_state(&save_state);
                }
            }
        }

        // let-chains
        loop {
            let outer_attrs = s.parse_outer_attrs()?;

            let token = s.current();
            let item = if matches!(&token.code, T![let]) {
                let t_let = token.clone();
                s.plusplus();
                let i_start = s.index.get();
                let pattern = s.parse_pattern()?;
                let i_end = s.index.get();
                let pat_has_brace = s.tokens[i_start..i_end]
                    .iter().any(|it| matches!(it.code, T!["{"]));
                let mut t_eq = s.current().clone();
                t_eq.code = T![->];
                s.expect(T![=])?;

                // specially parse expr
                let mut config = ParseConfig::default();
                let t_first_in_expr = s.current().clone();
                if is_match {
                    config.in_match_conditions = true;
                } else {
                    config.in_conditions = true;
                }
                let expr = s.parse_expr_config(&config)?;

                let a = LetChainCondition {
                    outer_attrs,
                    pattern,
                    scrutinee: expr,
                    t_eq,
                    pat_has_brace,
                    t_first_in_expr,
                    t_let,
                };
                ConditionItem::LetChain(a)
            } else {
                // specially parse expr
                let mut config = ParseConfig::default();
                if is_match {
                    config.in_match_conditions = true;
                } else {
                    config.in_conditions = true;
                }
                let expr = s.parse_expr_config(&config)?;

                ConditionItem::Expr(expr)
            };

            let token = s.current();
            match &token.code {
                T![&&] => {
                    let t = token.clone();
                    r.push((item, Some(t)));
                    s.plusplus();
                    continue;
                }
                T!["{"] | T![=>] => {
                    r.push((item, None));
                    break;
                }
                _ => {
                    return Err(
                        s.panic(token, &format!("expected , or {{ or => but got {:?}", token))
                    );
                }
            }
        }

        return Ok(Conditions {
            items: r,
        });
    }

    fn parse_if(&self) -> ParseResult<ExprIf> {
        let s = self;

        let mut r = ExprIf::default();

        r.t_if = s.current().clone();
        s.plusplus();
        r.conditions = s.parse_conditions(false)?;

        // for auto completion
        if s.magic && matches!(&s.current().code, T![;]) {
            return Ok(r);
        }
        r.body = s.parse_block_expr()?;

        let token = s.current();
        if let T![else] = &token.code {
            let t_else = token.clone();
            s.plusplus();
            let token = s.current();
            match &token.code {
                T![if] => {
                    let a = s.parse_if()?;
                    r.else_ = Some((t_else, ExprIfElse::If(Box::new(a))));
                }
                T!["{"] => {
                    let a = s.parse_block_expr()?;
                    r.else_ = Some((t_else, ExprIfElse::BlockExpr(a)));
                }
                _ => {
                    return Err(s.panic(token, &format!("expected if or {{ but got {:?}", token)));
                }
            }
        }

        Ok(r)
    }

    fn parse_match(&self) -> ParseResult<ExprMatch> {
        let s = self;

        let mut r = ExprMatch::default();

        let mut config = ParseConfig::default();
        config.stop_on_struct_brace = true;
        r.expr = s.parse_expr_config(&config)?;

        // for auto completion
        if s.magic && matches!(&s.current().code, T![;]) {
            return Ok(r);
        }

        r.t_brace_open = s.current().clone();
        s.expect(T!["{"])?;
        r.inner_attrs = s.parse_inner_attrs()?;

        while s.has_more() {
            let token = s.current();
            match &token.code {
                T!["}"] => {
                    r.t_brace_close = token.clone();
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
                let t_if = token.clone();
                s.plusplus();
                r.arm.guard = Some(ExprMatchArmGuard {
                    conditions: s.parse_conditions(true)?,
                    t_if,
                });
            }

            let token = s.current();
            // for auto completion
            if s.magic && matches!(&token.code, T![;]) {
                s.plusplus();
                return Ok(r);
            }
            r.t_sep = token.clone();
            s.expect(T![=>])?;

            // expr
            let mut config = ParseConfig::default();
            config.stop_on_block_expr = true;
            r.expr = s.parse_expr_config(&config)?;

            let token = s.current();
            if matches!(&token.code, T![,]) {
                r.t_comma = Some(token.clone());
                s.plusplus();
            }

            Ok(r)
        }
    }

    // let PatternNoTopAlt ( : Type )?
    // (
    //      = Expr
    //      = Expr else BlockExpr
    // )? ;
    fn parse_let(&self) -> ParseResult<StmtLet> {
        let s = self;

        let mut r = StmtLet::default();

        r.t_let = s.current().clone();
        s.expect(T![let])?;

        r.t_first_in_pattern = Some(s.current().clone());
        let pat = s.parse_pattern_no_top_alt()?;

        // check if it's the short form
        if let PatternNoTopAlt::WithoutRange(pat) = &pat {
            if let PatternWithoutRange::Identifier(a) = pat
                && a.pattern.is_none()
                && a.t_ref.is_none()
            {
                r.short = Some((a.name.clone(), a.t_mut.clone()));
            } else if let PatternWithoutRange::Path(a) = pat
                && let PatternPath::Path(a) = a
                && a.items.len() == 1
            {
                let name = a.items[0].name.token.clone();
                r.short = Some((name, None));
            }
        }
        r.pattern = Pattern {
            items: vec![(pat, None)],
        };

        // possible Type
        let token = s.current();
        if matches!(token.code, T![:]) {
            s.plusplus();
            r.type_ = Some(s.parse_type()?);
        }
        let token = s.current();
        if matches!(token.code, T![;]) {
            if r.short.is_some() {
                r.short = None;
            }
            return Ok(r);
        }

        r.t_eq = s.current().clone();
        s.expect(T![=])?;

        r.t_first_in_expr = Some(s.current().clone());
        if r.short.is_some() {
            let mut config = ParseConfig::default();
            config.may_end_with_semicolon = true;
            r.expr = Some(s.parse_expr_config(&config)?);
        } else {
            r.expr = Some(s.parse_expr()?);
        }

        let token = s.current();
        if matches!(&token.code, T![else]) {
            let t_else = token.clone();
            s.plusplus();
            let block = s.parse_block_expr()?;
            r.else_ = Some((t_else, block));
        }
        s.expect(T![;])?;
        Ok(r)
    }

    // --------------------------------------------------------------------------------
    // exprs

    fn parse_expr(&self) -> ParseResult<Expr> {
        let s = self;
        let config = ParseConfig::default();
        s.parse_expr_config(&config)
    }

    fn parse_expr_config(&self, config: &ParseConfig) -> ParseResult<Expr> {
        let s = self;

        let mut r = Expr::default();
        r.outer_attrs = s.parse_outer_attrs()?;

        let mut list: Vec<ExprOp> = Vec::new();

        while s.has_more() {
            let expr = s.parse_binary_expr(config)?;
            if config.stop_on_block_expr {
                if matches!(&expr.payload, ExprPayload::WithBlock(_)) {
                    list.push(ExprOp::Expr(expr));
                    break;
                }
            }

            s.minusminus();
            let t_last = s.current();
            s.plusplus();
            let token = s.current();
            let show_backslash = t_last.span.end_line != token.span.line;
            list.push(ExprOp::Expr(expr));

            if config.in_conditions || config.in_match_conditions {
                let token = s.current();
                match &token.code {
                    T![&&] | T![||] | T![..] | T![..=] => {
                        break;
                    }
                    _ => {}
                }
            }

            let token = s.current();
            match &token.code {
                // Expr op Expr
                T![+]
                | T![-]
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
                | T![..]
                | T![..=] => {
                    // NOTE because ">>" is two '>' tokens in lexer
                    let t = match &token.code {
                        T![>] => {
                            let t1 = s.nth(1);
                            if matches!(&t1.code, T![>]) {
                                s.plusplus();
                                let mut t = token.clone();
                                t.code = T![>>];
                                t
                            } else if matches!(&t1.code, T![=]) {
                                s.plusplus();
                                let mut t = token.clone();
                                t.code = T![>=];
                                t
                            } else {
                                token.clone()
                            }
                        }
                        T![<] => {
                            let t1 = s.nth(1);
                            if matches!(&t1.code, T![<]) {
                                s.plusplus();
                                let mut t = token.clone();
                                t.code = T![<<];
                                t
                            } else if matches!(&t1.code, T![=]) {
                                s.plusplus();
                                let mut t = token.clone();
                                t.code = T![<=];
                                t
                            } else {
                                token.clone()
                            }
                        }
                        _ => token.clone(),
                    };
                    list.push(ExprOp::Op((t, show_backslash)));
                    s.plusplus();
                }

                _ => {
                    break;
                }
            }
        }

        r.payload = to_op_expr(s, list, config)?.payload;
        return Ok(r);

        #[derive(Debug)]
        enum ExprOp {
            Expr(Expr),
            Op((Token, bool)),
        }
        impl Default for ExprOp {
            fn default() -> Self {
                ExprOp::Op((Token::default(), false))
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
        fn to_op_expr(s: &Parse, list: Vec<ExprOp>, config: &ParseConfig) -> ParseResult<Expr> {
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

            let mut op_stack: Vec<(Token, bool)> = Vec::new();
            let mut r_list: Vec<ExprOp> = Vec::new();

            // from infix to postfix
            for item in list {
                match item {
                    ExprOp::Expr(expr) => {
                        r_list.push(ExprOp::Expr(expr));
                    }
                    ExprOp::Op(item) => loop {
                        if op_stack.is_empty() {
                            op_stack.push(item);
                            break;
                        }
                        let top = op_stack.last().unwrap();
                        let top_precedence = *s.op_map.get(&top.0.code).unwrap();
                        let current_precedence = *s.op_map.get(&item.0.code).unwrap();
                        if current_precedence > top_precedence {
                            op_stack.push(item);
                            break;
                        } else if current_precedence < top_precedence {
                            r_list.push(ExprOp::Op(op_stack.pop().unwrap()));
                        } else {
                            r_list.push(ExprOp::Op(op_stack.pop().unwrap()));
                            op_stack.push(item);
                            break;
                        }
                    },
                }
            }
            for op in op_stack.into_iter().rev().collect::<Vec<(Token, bool)>>() {
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

                    let left = left_option.unwrap();
                    let right = right_option.unwrap();

                    let (op, show) = op;

                    match op.code {
                        T![+]
                        | T![-]
                        | T![*]
                        | T![/]
                        | T![%]
                        | T![&]
                        | T![|]
                        | T![^]
                        | T![<<]
                        | T![>>] => {
                            let show_backslash =
                                if config.may_end_with_semicolon
                                    && matches!(op.code, T![-] | T![*] | T![&] | T![|])
                                {
                                    show
                                } else {
                                    false
                                };
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Operator(ExprOperator::ArithmeticOrLogical(
                                        ExprArithmeticOrLogical {
                                            op,
                                            show_backslash,
                                            left,
                                            right,
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
                                            op,
                                            left,
                                            right,
                                        },
                                    )),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }
                        T![||] | T![&&] => {
                            let show_backslash =
                                if config.may_end_with_semicolon {
                                    show
                                } else {
                                    false
                                };
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Operator(ExprOperator::LazyBoolean(
                                        ExprLazyBoolean {
                                            op,
                                            show_backslash,
                                            left,
                                            right,
                                        },
                                    )),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }

                        T![..] => {
                            let show_backslash =
                                if config.may_end_with_semicolon {
                                    show
                                } else {
                                    false
                                };
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Range(ExprRange {
                                        start: Some(left),
                                        end: Some(right),
                                        t_sep: op,
                                        show_backslash,
                                    }),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }
                        T![..=] => {
                            let show_backslash =
                                if config.may_end_with_semicolon {
                                    show
                                } else {
                                    false
                                };
                            let expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Range(ExprRange {
                                        start: Some(left),
                                        end: Some(right),
                                        t_sep: op,
                                        show_backslash,
                                    }),
                                )),
                            };
                            r_list[index] = ExprOp::Expr(expr);
                        }
                        _ => {
                            return Err(s.panic(s.current(), "not a valid op"));
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
                return Err(s.panic(s.current(), "not an expr"));
            }
        }
    }

    fn parse_binary_expr(&self, config: &ParseConfig) -> ParseResult<Expr> {
        let s = self;

        let mut r = Expr::default();

        let mut expr = s.parse_sub_expr(config)?;

        if matches!(&expr.payload, ExprPayload::WithBlock(_)) {
            let token = s.current();
            if matches!(&token.code, T!["("]) {
                return Ok(expr);
            }
            if !matches!(&token.code, T![.] | T![?]) {
                if config.stop_on_block_expr {
                    return Ok(expr);
                }
            }
        }

        while s.has_more() {
            let token = s.current();

            if config.in_conditions || config.in_match_conditions {
                match &token.code {
                    T![=] | TokenCode::Assign(_) | T![..] => {
                        return Ok(expr);
                    }
                    _ => {}
                }
            }

            // check if the operator is in the same line as the last token of the left expr
            s.minusminus();
            let show_backslash = s.current().span.end_line != token.span.line;
            s.plusplus();

            s.plusplus();
            match &token.code {
                // Expr = Expr
                T![=] => {
                    let right = s.parse_expr_config(config)?;
                    r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                        ExprOperator::Assignment(ExprAssignment {
                            op: token.clone(),
                            left: expr,
                            right,
                        }),
                    )));
                    break;
                }
                T![>] => {
                    let mut is_assign = false;
                    let t1 = s.current();
                    if matches!(&t1.code, T![>]) {
                        let t2 = s.nth(1);
                        if matches!(&t2.code, T![=]) {
                            is_assign = true;
                        }
                    }
                    if is_assign {
                        s.inc(2);
                        let mut t = token.clone();
                        t.code = T![>>=];
                        let right = s.parse_expr_config(config)?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                            ExprOperator::CompoundAssignment(ExprCompoundAssignment {
                                op: t,
                                left: expr,
                                right,
                            }),
                        )));
                        break;
                    } else {
                        s.minusminus();
                        r = expr;
                        break;
                    }
                }
                T![<] => {
                    let mut is_assign = false;
                    let t1 = s.current();
                    if matches!(&t1.code, T![<]) {
                        let t2 = s.nth(1);
                        if matches!(&t2.code, T![=]) {
                            is_assign = true;
                        }
                    }
                    if is_assign {
                        s.inc(2);
                        let mut t = token.clone();
                        t.code = T![<<=];
                        let right = s.parse_expr_config(config)?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                            ExprOperator::CompoundAssignment(ExprCompoundAssignment {
                                op: t,
                                left: expr,
                                right,
                            }),
                        )));
                        break;
                    } else {
                        s.minusminus();
                        r = expr;
                        break;
                    }
                }
                // compound assignment
                T![+=]
                | T![-=]
                | T![*=]
                | T![/=]
                | T![%=]
                | T![&=]
                | T![|=]
                | T![^=]
                | T![<<=]
                | T![>>=] => {
                    let right = s.parse_expr_config(config)?;
                    r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                        ExprOperator::CompoundAssignment(ExprCompoundAssignment {
                            op: token.clone(),
                            left: expr,
                            right,
                        }),
                    )));
                    break;
                }

                // Expr(CallParams?)
                T!["("] => {
                    let t_paren_open = token.clone();
                    let mut params = Vec::new();
                    while s.has_more() {
                        let token = s.current();
                        if matches!(&token.code, T![")"]) {
                            s.plusplus();
                            expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Call(ExprCall {
                                        left: expr,
                                        show_backslash,
                                        params,
                                        t_paren_open,
                                        t_paren_close: token.clone(),
                                    }),
                                )),
                            };
                            break;
                        }

                        let expr = s.parse_expr()?;

                        let token = s.current();
                        if matches!(&token.code, T![,]) {
                            s.plusplus();
                            params.push((expr, true));
                        } else {
                            params.push((expr, false));
                        }
                    }
                }

                // Expr ..
                T![..] => {
                    if s.at_range_end(&s.current().code) {
                        expr = Expr {
                            outer_attrs: Attrs::default(),
                            payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Range(
                                ExprRange {
                                    start: Some(expr),
                                    end: None,
                                    t_sep: token.clone(),
                                    show_backslash: false,
                                },
                            ))),
                        };
                    } else {
                        s.minusminus();
                        r = expr;
                        break;
                    }
                }

                // Expr[Expr]
                T!["["] => {
                    let mut t_delim_open = token.clone();
                    t_delim_open.code = T!["[`"];
                    let right = s.parse_expr()?;
                    let mut t_delim_close = s.current().clone();
                    t_delim_close.code = T!["`]"];
                    s.expect(T!["]"])?;
                    expr = Expr {
                        outer_attrs: Attrs::default(),
                        payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Index(
                            ExprIndex {
                                left: expr,
                                show_backslash,
                                right,
                                t_delim_open,
                                t_delim_close,
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
                                show_backslash,
                                t_sep: token.clone(),
                            }),
                        ))),
                    };
                }
                // Expr as Type
                T![as] => {
                    let type_ = s.parse_type()?;
                    let TypePayload::TypeNoBounds(type_no_bounds) = type_.payload else {
                        return Err(s.panic(token, "not a TypeNoBounds"))
                    };
                    expr = Expr {
                        outer_attrs: Attrs::default(),
                        payload: ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                            ExprOperator::TypeCast(Box::new(ExprTypeCast {
                                expr,
                                type_no_bounds: *type_no_bounds,
                                t_as: token.clone(),
                            })),
                        ))),
                    };
                }

                // Expr.await, Expr.TupleIndex, Expr.PathExprSegment(CallParams?)
                T![.] => {
                    let t_dot = token.clone();
                    let token = s.current();
                    match &token.code {
                        T![await] => {
                            expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Await(ExprAwait {
                                        expr,
                                        t_dot,
                                        t_await: token.clone(),
                                    }),
                                )),
                            };
                            s.plusplus();
                        }
                        TokenCode::Literal(Literal {
                            payload: LiteralPayload::Int(_),
                            ..
                        }) => {
                            expr = Expr {
                                outer_attrs: Attrs::default(),
                                payload: ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::TupleIndexing(ExprTupleIndexing {
                                        left: expr,
                                        index: token.clone(),
                                        t_dot,
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
                                a.t_dot = t_dot;
                                expr = Expr {
                                    outer_attrs: Attrs::default(),
                                    payload: ExprPayload::WithoutBlock(Box::new(
                                        ExprWithoutBlock::MethodCall(a),
                                    )),
                                };
                            } else {
                                let name_token = segment.name.token;
                                if !matches!(name_token.code, TokenCode::Identifier(_)) {
                                    return Err(s.panic(
                                        token,
                                        &format!("expected identifier but got {:?}", token),
                                    ));
                                }
                                expr = Expr {
                                    outer_attrs: Attrs::default(),
                                    payload: ExprPayload::WithoutBlock(Box::new(
                                        ExprWithoutBlock::Field(ExprField {
                                            left: expr,
                                            name: name_token,
                                            t_dot,
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
            r.t_paren_open = s.current().clone();
            s.plusplus();

            while s.has_more() {
                let token = s.current();

                if matches!(&token.code, T![")"]) {
                    r.t_paren_close = token.clone();
                    s.plusplus();
                    break;
                }

                let expr = s.parse_expr()?;

                let token = s.current();
                if matches!(&token.code, T![,]) {
                    s.plusplus();
                    r.params.push((expr, true));
                } else {
                    r.params.push((expr, false));
                }
            }

            Ok(r)
        }
    }

    fn parse_sub_expr(&self, config: &ParseConfig) -> ParseResult<Expr> {
        let s = self;

        let mut r = Expr::default();
        r.outer_attrs = s.parse_outer_attrs()?;

        let token = s.current();
        s.plusplus();
        match &token.code {
            // literal
            TokenCode::Literal(_) => {
                r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Literal(
                    ExprLiteral(token.clone()),
                )));
            }

            // lifetime
            TokenCode::Lifetime(_) => {
                let label = Some((token.clone(), s.current().clone()));
                s.expect(T![:])?;

                let token = s.current();
                if matches!(token.code, T![for] | T![loop] | T![while]) {
                    let mut for_ = s.parse_for()?;
                    for_.label = label;
                    r.payload =
                        ExprPayload::WithBlock(Box::new(ExprWithBlock::Loop(Box::new(for_))));
                } else {
                    let body = s.parse_block_expr()?;
                    let b = ExprLoop {
                        label,
                        body,
                        payload: ExprLoopPayload::Block,
                        t_for: Token::default(),
                    };
                    let c = Box::new(b);
                    r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::Loop(c)));
                }
            }

            // grouped or tuple
            T!["("] => {
                let t_paren_open = token.clone();

                let mut items: Vec<(Expr, bool)> = Vec::new();
                while s.has_more() {
                    let token = s.current();
                    if matches!(&token.code, T![")"]) {
                        let t_paren_close = token.clone();
                        s.plusplus();
                        if items.len() == 1 && !items[0].1 {
                            let expr = mem::take(&mut items[0]).0;
                            r.payload = ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Grouped(ExprGrouped {
                                    expr,
                                    t_paren_open,
                                    t_paren_close,
                                }),
                            ));
                        } else {
                            r.payload = ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Tuple(ExprTuple {
                                    items,
                                    t_paren_open,
                                    t_paren_close,
                                }),
                            ));
                        }
                        return Ok(r);
                    }

                    let expr = s.parse_expr()?;

                    let token = s.current();
                    if matches!(&token.code, T![,]) {
                        s.plusplus();
                        items.push((expr, true));
                    } else {
                        items.push((expr, false));
                    }
                }
            }
            // array
            T!["["] => {
                let mut t_delim_open = token.clone();
                t_delim_open.code = T!["[`"];
                // []
                let token = s.current();
                if matches!(&token.code, T!["]"]) {
                    let mut t_delim_close = token.clone();
                    t_delim_close.code = T!["`]"];
                    s.plusplus();
                    r.payload =
                        ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Array(ExprArray {
                            payload: ExprArrayPayload::Many(ExprArrayMany(Vec::new())),
                            t_delim_open,
                            t_delim_close,
                        })));
                    return Ok(r);
                }

                let first_expr = s.parse_expr()?;
                let token = s.current();
                if matches!(&token.code, T![;]) && !token.is_added_semicolon() {
                    // [ Expr; Expr ]
                    let t_semicolon = token.clone();
                    s.plusplus();
                    let len = s.parse_expr()?;
                    let mut t_delim_close = s.current().clone();
                    t_delim_close.code = T!["`]"];
                    s.expect(T!["]"])?;
                    r.payload =
                        ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Array(ExprArray {
                            payload: ExprArrayPayload::One(ExprArrayOne {
                                value: first_expr,
                                len,
                                t_semicolon,
                            }),
                            t_delim_open,
                            t_delim_close,
                        })));
                    return Ok(r);
                }

                let show_comma = if matches!(&token.code, T![,]) {
                    s.plusplus();
                    true
                } else {
                    false
                };

                // [ Expr([,;] Expr)* ]
                let mut items: Vec<(Expr, bool)> = vec![(first_expr, show_comma)];
                while s.has_more() {
                    let token = s.current();

                    if matches!(&token.code, T!["]"]) {
                        let mut t_delim_close = token.clone();
                        t_delim_close.code = T!["`]"];
                        s.plusplus();
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Array(ExprArray {
                                payload: ExprArrayPayload::Many(ExprArrayMany(items)),
                                t_delim_open,
                                t_delim_close,
                            }),
                        ));
                        return Ok(r);
                    }

                    let expr = s.parse_expr()?;

                    let token = s.current();
                    if matches!(&token.code, T![,]) {
                        s.plusplus();
                        items.push((expr, true));
                    } else {
                        items.push((expr, false));
                    }
                }
            }

            // qualified path in expression
            T![<] => {
                s.minusminus();
                let type_ = s.parse_qualifyed_path_type()?;
                s.expect(T![::])?;
                s.minusminus();
                let items = s.parse_path_expr_segments()?;
                r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Path(
                    ExprPath::QualifiedPath(QualifiedPathInExpr { type_, items }),
                )));
            }

            // path in expression or macro call
            T![super] | T![self] | T![Self] | T![crate] | TokenCode::Identifier(_) | T![::] => {
                s.minusminus();
                let items = s.parse_path_expr_segments()?;
                let token = s.current();
                if !matches!(&token.code, T![!]) {
                    let path = PathInExpr { items };
                    match &token.code {
                        // struct expr
                        T!["{"] => {
                            let t_brace_open = token.clone();
                            if config.stop_on_struct_brace || config.in_conditions {
                                r.payload = ExprPayload::WithoutBlock(Box::new(
                                    ExprWithoutBlock::Path(ExprPath::Path(path)),
                                ));
                                return Ok(r);
                            }

                            s.plusplus();
                            let mut a = ExprStruct::default();
                            a.path = path;
                            a.t_brace_open = t_brace_open;

                            while s.has_more() {
                                let token = s.current();
                                if matches!(&token.code, T!["}"]) {
                                    a.t_brace_close = token.clone();
                                    s.plusplus();
                                    break;
                                }

                                if matches!(&token.code, T![..]) {
                                    let t_sep = token.clone();
                                    s.plusplus();
                                    // NOTE temporarily disable this feature
                                    // let mut e = None;
                                    // if !matches!(s.current().code, T!["}"]) {
                                    //     let expr = s.parse_expr()?;
                                    //     // check if expr is Default::default()
                                    //     if !expr_is_default(&expr) {
                                    //         e = Some(expr);
                                    //     }
                                    // }
                                    let e = Some(s.parse_expr()?);
                                    a.base = Some((t_sep, e));
                                    if matches!(s.current().code, T![;]) {
                                        s.plusplus();
                                    }
                                    a.t_brace_close = s.current().clone();
                                    s.expect(T!["}"])?;
                                    break;
                                }

                                let mut field = parse_struct_expr_field(s)?;

                                let token = s.current();
                                if matches!(&token.code, T![,]) {
                                    s.plusplus();
                                    field.show_comma = true;
                                    a.fields.push(field);
                                } else if matches!(&token.code, T!["}"]) {
                                    a.fields.push(field);
                                } else {
                                    return Err(s.panic(token,
                                        &format!("illegal token after field {:?}", token)));
                                }
                            }
                            r.payload = ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Struct(a),
                            ));
                        }
                        _ => {
                            r.payload = ExprPayload::WithoutBlock(Box::new(
                                ExprWithoutBlock::Path(ExprPath::Path(path)),
                            ));
                        }
                    }
                    return Ok(r);

                    // check if expr is Default::default()
                    fn _expr_is_default(expr: &Expr) -> bool {
                        if let ExprPayload::WithoutBlock(a) = &expr.payload &&
                            let ExprWithoutBlock::Call(call) = a.as_ref() &&
                            call.params.is_empty() &&

                            let ExprPayload::WithoutBlock(a) = &call.left.payload &&
                            let ExprWithoutBlock::Path(p) = a.as_ref() &&
                            let ExprPath::Path(p) = p &&
                            p.items.len() == 2 &&
                            p.items[0].t_sep.is_none() &&
                            p.items[0].name.token.u == "Default" &&
                            p.items[1].t_sep.is_some() &&
                            p.items[1].name.token.u == "default"
                        {
                            true
                        } else {
                            false
                        }
                    }
                }

                // macro call
                let mut path = SimplePath::default();
                for item in items {
                    if !item.generic_args.items.is_empty() {
                        return Err(s.panic(token, "macro call can't contain generic params"));
                    }
                    let result = SimplePathSegment::try_from(item);
                    if let Ok(a) = result {
                        path.items.push(a);
                    } else {
                        return Err(s.panic(token, "macro call can't contain Self"));
                    }
                }
                s.plusplus();
                let call = s.parse_macro_call(path, token)?;
                r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::MacroCall(call)));
            }

            T!["{"] => {
                s.minusminus();
                let a = s.parse_block_expr()?;
                r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::BlockExpr(a)));
            }
            TokenCode::Keyword(keyword) => {
                match keyword {
                    // async move? ||
                    // async move? {}
                    Keyword::Async => {
                        s.minusminus();
                        let mut is_block = false;
                        let t1 = s.nth(1);
                        if matches!(t1.code, T!["{"]) {
                            is_block = true;
                        } else if matches!(t1.code, T![move]) {
                            let t2 = s.nth(2);
                            if matches!(t2.code, T!["{"]) {
                                is_block = true;
                            }
                        } else if !matches!(t1.code, T![|] | T![||]) {
                            return Err(
                                s.panic(token, &format!("illegal token {:?} after async", token))
                            );
                        }
                        if is_block {
                            let a = s.parse_async()?;
                            r.payload = ExprPayload::WithBlock(
                                Box::new(ExprWithBlock::AsyncBlock(a))
                            );
                        } else {
                            let a = s.parse_closure_expr()?;
                            r.payload =
                                ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Closure(a)));
                        }
                    }
                    // move ||
                    // move {}
                    Keyword::Move => {
                        s.minusminus();
                        let t1 = s.nth(1);
                        if matches!(t1.code, T!["{"]) {
                            let a = s.parse_async()?;
                            r.payload = ExprPayload::WithBlock(
                                Box::new(ExprWithBlock::AsyncBlock(a))
                            );
                        } else {
                            let a = s.parse_closure_expr()?;
                            r.payload =
                                ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Closure(a)));
                        }
                    }
                    Keyword::Unsafe => {
                        let t_unsafe = token.clone();
                        let a = s.parse_block_expr()?;
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::UnsafeBlock(
                            ExprUnsafeBlock {
                                block: a,
                                t_unsafe,
                            },
                        )));
                    }
                    Keyword::Const => {
                        let t_const = token.clone();
                        let a = s.parse_block_expr()?;
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::ConstBlock(
                            ExprConstBlock {
                                block: a,
                                t_const,
                            },
                        )));
                    }
                    Keyword::For | Keyword::Loop | Keyword::While => {
                        s.minusminus();
                        let a = s.parse_for()?;
                        r.payload =
                            ExprPayload::WithBlock(Box::new(ExprWithBlock::Loop(Box::new(a))));
                    }
                    Keyword::If => {
                        s.minusminus();
                        let a = s.parse_if()?;
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::If(a)));
                    }
                    Keyword::Match => {
                        let mut a = s.parse_match()?;
                        a.t_match = token.clone();
                        r.payload = ExprPayload::WithBlock(Box::new(ExprWithBlock::Match(a)));
                    }

                    // without block
                    Keyword::Break => {
                        let mut a = s.parse_break()?;
                        a.t_break = token.clone();
                        r.payload = ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Break(a)));
                    }
                    Keyword::Continue => {
                        let mut a = s.parse_continue()?;
                        a.t_continue = token.clone();
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Continue(a)));
                    }
                    Keyword::Return => {
                        let mut a = s.parse_ret()?;
                        a.t_return = token.clone();
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
                    // .. and .. Expr
                    Op::DotDot => {
                        let mut range = ExprRange::default();
                        range.t_sep = token.clone();
                        if !s.at_range_end(&s.current().code) {
                            range.end = Some(s.parse_expr_config(config)?);
                        }
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Range(range)));
                    }
                    // ..= Expr
                    Op::DotDotEq => {
                        let mut range = ExprRange::default();
                        range.t_sep = token.clone();
                        range.end = Some(s.parse_expr_config(config)?);
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Range(range)));
                    }

                    // unary Expr
                    Op::BitAnd => {
                        let borrow = parse_borrow(s, token, config)?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Operator(ExprOperator::Borrow(borrow)),
                        ));
                    }
                    Op::And => {
                        let borrow = parse_borrow(s, token, config)?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Operator(ExprOperator::Borrow(borrow)),
                        ));
                    }
                    Op::Star => {
                        let expr = s.parse_expr_config(config)?;
                        r.payload =
                            ExprPayload::WithoutBlock(Box::new(ExprWithoutBlock::Operator(
                                ExprOperator::Dereference(ExprDereference {
                                    expr,
                                    t_sep: token.clone(),
                                }),
                            )));
                    }
                    Op::Sub => {
                        let expr = s.parse_expr_config(config)?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Operator(ExprOperator::Negation(ExprNegation {
                                expr,
                                t_sep: token.clone(),
                            })),
                        ));
                    }
                    Op::Not => {
                        let expr = s.parse_expr_config(config)?;
                        r.payload = ExprPayload::WithoutBlock(Box::new(
                            ExprWithoutBlock::Operator(ExprOperator::Negation(ExprNegation {
                                expr,
                                t_sep: token.clone(),
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

        fn parse_borrow(
            s: &Parse,
            ref_token: &Token,
            config: &ParseConfig,
        ) -> ParseResult<ExprBorrow> {
            let mut r = ExprBorrow::default();

            r.t_ref = ref_token.clone();

            let token = s.current();
            if matches!(&token.code, T![mut]) {
                r.t_mut = Some(token.clone());
                s.plusplus();
            } else if let TokenCode::Identifier(a) = &token.code
                && a.prefix.is_none()
                && a.id == "raw"
            {
                let t_raw = token;
                let token = s.nth(1);
                if matches!(&token.code, T![mut] | T![const]) {
                    r.t_raw = Some(t_raw.clone());
                    r.t_mut = Some(token.clone());
                    s.inc(2);
                }
            }

            r.expr = s.parse_expr_config(config)?;
            return Ok(r);
        }

        fn parse_struct_expr_field(s: &Parse) -> ParseResult<ExprStructField> {
            let mut r = ExprStructField::default();

            r.outer_attrs = s.parse_outer_attrs()?;
            r.name = s.current().clone();
            s.plusplus();

            let token = s.current();
            if matches!(&token.code, T![:]) {
                s.plusplus();
                r.expr = Some((token.clone(), s.parse_expr()?));
                if !matches!(r.name.code,
                        TokenCode::Identifier(_)
                        | TokenCode::Literal(Literal {
                            payload: LiteralPayload::Int(_),
                            ..
                        })) {
                    return Err(s.panic(
                        token,
                        &format!("expected tuple index but got {:?}", token),
                    ));
                }
            } else {
                if !matches!(r.name.code, TokenCode::Identifier(_)) {
                    return Err(s.panic(
                        token,
                        &format!("expected identifier but got {:?}", token),
                    ));
                }
            }

            Ok(r)
        }
    }

    fn at_range_end(&self, code: &TokenCode) -> bool {
        matches!(code,
            T![")"] | T!["]"] | T!["{"] | T![;] | T![,] |
            T![==] | T![!=] | T![>] | T![>=] | T![<=] |
            T![=] | T![+=] | T![-=] | T![*=] | T![/=] | T![%=] |
            T![&=] | T![|=] | T![^=] | T![<<=] | T![>>=]
        )
    }

    // async? move?
    // ( || | | Params | )
    // (Expr | -> TypeNoBounds BlockExpr)
    //
    // Param -> OuterAttribute* PatternNoTopAlt ( : Type )?
    fn parse_closure_expr(&self) -> ParseResult<ExprClosure> {
        let s = self;

        let mut r = ExprClosure::default();

        let token = s.current();
        r.t_first = token.clone();
        if matches!(&token.code, T![async]) {
            r.t_async = Some(token.clone());
            s.plusplus();
        }

        let token = s.current();
        if matches!(&token.code, T![move]) {
            r.t_move = Some(token.clone());
            s.plusplus();
        }

        // params
        let token = s.current();
        r.t_delim_open = token.clone();
        s.plusplus();
        match token.code {
            T![|] => {
                while s.has_more() {
                    let token = s.current();
                    if matches!(&token.code, T![|]) {
                        r.t_delim_close = token.clone();
                        s.plusplus();
                        break;
                    }

                    let mut param = FuncParamOptionalType::default();
                    param.outer_attrs = s.parse_outer_attrs()?;

                    let pat = s.parse_pattern_no_top_alt()?;
                    // check if it's the short form
                    let mut is_short = false;
                    if let PatternNoTopAlt::WithoutRange(pat) = &pat {
                        if let PatternWithoutRange::Identifier(a) = pat {
                            if a.t_mut.is_none() {
                                param.name = a.name.clone();
                                is_short = true;
                            }
                        } else if let PatternWithoutRange::Path(a) = pat
                                && let PatternPath::Path(a) = a
                                && a.items.len() == 1 {
                            let name = a.items[0].name.token.clone();
                            param.name = name.clone();
                            is_short = true;
                        }
                    }
                    if !is_short {
                        param.pat = Some(pat);
                    }

                    let token = s.current();
                    if matches!(&token.code, T![:]) {
                        s.plusplus();
                        let type_ = s.parse_type()?;
                        param.type_ = Some(type_);
                    } else {
                        param.type_ = None;
                    }

                    let token = s.current();
                    let t_comma = if matches!(&token.code, T![,]) {
                        s.plusplus();
                        Some(token.clone())
                    } else {
                        None
                    };
                    r.params.push((param, t_comma));
                }
            }
            T![||] => {}
            _ => {
                return Err(s.panic(token, &format!("expected | or || but got {:?}", token)));
            }
        }

        // return type
        let token = s.current();
        if matches!(&token.code, T![->]) {
            let t_sep = token.clone();
            s.plusplus();
            let type_ = s.parse_type()?;
            let TypePayload::TypeNoBounds(a) = type_.payload else {
                return Err(s.panic(token, "expected TypeNoBounds"));
            };
            r.return_type = Some((t_sep, *a));
            // the expr after return type must be a block expr
            let token = s.current();
            if !matches!(token.code, T!["{"]) {
                return Err(s.panic(token, "expected '{'"));
            }
        }
        // body expr
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
            rs_pair: RsPair{start, end},
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
                let t_paren_open = token.clone();
                let mut type_list: Vec<(Type, Option<Token>)> = Vec::new();
                let mut is_tuple = false;
                while s.has_more() {
                    let token = s.current();
                    if matches!(&token.code, T![")"]) {
                        s.plusplus();
                        if type_list.len() == 1 && !is_tuple {
                            let a = mem::take(&mut type_list[0]);
                            let payload = TypePayload::TypeNoBounds(Box::new(
                                TypeNoBounds::Parenthesized(TypeParenthesized {
                                    type_: a.0,
                                    t_paren_open,
                                    t_paren_close: token.clone(),
                                }),
                            ));
                            return Ok(payload);
                        } else {
                            let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::Tuple(
                                TypeTuple {
                                    items: type_list,
                                    t_paren_open,
                                    t_paren_close: token.clone(),
                                },
                            )));
                            return Ok(payload);
                        }
                    }

                    let type_ = s.parse_type()?;
                    let token = s.current();
                    if matches!(&token.code, T![,]) {
                        is_tuple = true;
                        s.plusplus();
                        type_list.push((type_, Some(token.clone())));
                    } else {
                        type_list.push((type_, None));
                    }
                }
            }
            // qualified path
            T![<] => {
                s.minusminus();
                let qualified_path_type = s.parse_qualifyed_path_type()?;
                s.expect(T![::])?;
                s.minusminus();
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
            T!["["] => {
                let mut t_delim_open = token.clone();
                t_delim_open.code = T!["[`"];
                let type_ = s.parse_type()?;
                let token = s.current();
                match &token.code {
                    T![;] => {
                        let t_semicolon = token.clone();
                        s.plusplus();
                        let expr = s.parse_expr()?;
                        let mut t_delim_close = s.current().clone();
                        t_delim_close.code = T!["`]"];
                        s.expect(T!["]"])?;
                        let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::Array(
                            TypeArray {
                                type_,
                                expr,
                                t_delim_open,
                                t_delim_close,
                                t_semicolon,
                            },
                        )));
                        return Ok(payload);
                    }
                    T!["]"] => {
                        let mut t_delim_close = s.current().clone();
                        t_delim_close.code = T!["`]"];
                        s.plusplus();
                        let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::Slice(
                            TypeSlice {
                                type_,
                                t_delim_open,
                                t_delim_close,
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
                let t_ref = token.clone();

                let mut lifetime = None;
                let token = s.current();
                if let TokenCode::Lifetime(_) = &token.code {
                    lifetime = Some(Lifetime {
                        name: token.clone(),
                    });
                    s.plusplus();
                }
                let mut t_mut = None;
                let token = s.current();
                if matches!(&token.code, T![mut]) {
                    t_mut = Some(token.clone());
                    s.plusplus();
                }
                let type_ = s.parse_type()?;
                let TypePayload::TypeNoBounds(t) = type_.payload else {
                    return Err(s.panic(token, "expected TypeNoBounds"));
                };

                let no_bounds = Box::new(TypeNoBounds::Reference(TypeReference {
                    lifetime,
                    type_no_bounds: t,
                    t_ref,
                    t_mut,
                }));
                let payload = TypePayload::TypeNoBounds(no_bounds);
                return Ok(payload);
            }
            // raw pointer
            T![*] => {
                let t_pointer = token.clone();
                let token = s.current();
                let t_mut_const = token.clone();
                if !matches!(&token.code, T![const] | T![mut]) {
                    return Err(
                        s.panic(token, &format!("expected const or mut but got {:?}", token))
                    );
                }
                s.plusplus();
                let type_ = s.parse_type()?;
                if let TypePayload::TypeNoBounds(t) = type_.payload {
                    let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::RawPointer(
                        TypeRawPointer {
                            type_no_bounds: t,
                            t_pointer,
                            t_mut_const,
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
                    token.clone(),
                )));
                return Ok(payload);
            }
            // impl trait or impl trait one bound
            T![impl] => {
                let next_token = s.current();
                if let TokenCode::Lifetime(_) = &next_token.code {
                    let bounds = s.parse_type_param_bounds()?;
                    let payload = TypePayload::ImplTrait(TypeImplTrait {
                        bounds,
                        t_impl: token.clone(),
                    });
                    return Ok(payload);
                }
                let t_impl = token.clone();
                let bound = s.parse_trait_bound()?;
                let token = s.current();
                if !matches!(token.code, T![+]) {
                    let payload = TypePayload::TypeNoBounds(Box::new(
                        TypeNoBounds::ImplTraitOneBound(TypeImplTraitOneBound {
                            bound,
                            t_impl,
                        }),
                    ));
                    return Ok(payload);
                }
                s.plusplus();
                let mut bounds = s.parse_type_param_bounds()?;
                let type_bound = TypeParamBound::TraitBound(bound);
                bounds
                    .items
                    .insert(0, (type_bound, Some(token.clone())));
                let payload = TypePayload::ImplTrait(TypeImplTrait {
                    bounds,
                    t_impl,
                });
                return Ok(payload);
            }
            // trait object or trait object one bound
            T![dyn] => {
                let next_token = s.current();
                if let TokenCode::Lifetime(_) = &next_token.code {
                    let bounds = s.parse_type_param_bounds()?;
                    let payload = TypePayload::TraitObject(TypeTraitObject {
                        bounds,
                        t_dyn: token.clone(),
                    });
                    return Ok(payload);
                }
                let t_dyn = token.clone();
                let bound = s.parse_trait_bound()?;
                let token = s.current();
                if !matches!(token.code, T![+]) {
                    let payload = TypePayload::TypeNoBounds(Box::new(
                        TypeNoBounds::TraitObjectOneBound(TypeTraitObjectOneBound {
                            bound,
                            t_dyn,
                        }),
                    ));
                    return Ok(payload);
                }
                s.plusplus();
                let mut bounds = s.parse_type_param_bounds()?;
                let type_bound = TypeParamBound::TraitBound(bound);
                bounds
                    .items
                    .insert(0, (type_bound, Some(token.clone())));
                let payload = TypePayload::TraitObject(TypeTraitObject {
                    bounds,
                    t_dyn,
                });
                return Ok(payload);
            }

            // bare function
            T![for] => {
                s.minusminus();
                let t_first = s.current().clone();
                let lifetimes = s.parse_for_lifetimes()?;
                let mut f = s.parse_func_type()?;
                f.t_first = t_first;
                f.for_lifetimes = Some(lifetimes);
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::BareFunction(
                    Box::new(f),
                )));
                return Ok(payload);
            }
            // unsafe? ( extern Abi? )? fn
            T![fn] | T![unsafe] | T![extern] => {
                s.minusminus();
                let t_first = s.current().clone();
                let mut f = s.parse_func_type()?;
                f.t_first = t_first;
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::BareFunction(
                    Box::new(f),
                )));
                return Ok(payload);
            }

            // type path
            T![Self] | T![::] => {
                s.minusminus();
                let a = s.parse_type_path()?;
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::TypePath(a)));
                return Ok(payload);
            }

            // type path or macro call
            T![super] | T![self] | T![crate] | TokenCode::Identifier(_) => {
                s.minusminus();
                let mut tp = s.parse_type_path()?;
                let token = s.current();
                if !matches!(token.code, T![!]) {
                    // Option<Type> to ?Type
                    let mut is_optional = false;
                    if tp.items.len() == 1 {
                        let a = &tp.items[0];
                        if a.t_sep.is_none() &&
                                a.name.token.u == "Option" &&
                                let Some(payload) = &a.payload &&
                                let TypePathSegmentPayload::GenericArgs(b) = payload &&
                                b.items.len() == 1 &&
                                let (c, None) = &b.items[0] &&
                                matches!(c, GenericArg::Type(_))
                        {
                            is_optional = true;
                        }
                    }
                    if is_optional {
                        let a = mem::take(&mut tp.items[0]);
                        let t_sep = a.name.token.clone();
                        let payload = a.payload.unwrap();
                        let TypePathSegmentPayload::GenericArgs(mut b) = payload else {
                            return Err(s.panic(token, "unreachable in Option<Type>"));
                        };
                        let (c, _) = mem::take(&mut b.items[0]);
                        let GenericArg::Type(type_) = c else {
                            return Err(s.panic(token, "unreachable in Option<Type>"));
                        };

                        let tp = TypeOptional {
                            type_,
                            t_sep,
                        };
                        let payload = TypePayload::TypeNoBounds(
                            Box::new(TypeNoBounds::Optional(tp))
                        );
                        return Ok(payload);
                    }
                    let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::TypePath(tp)));
                    return Ok(payload);
                }

                let mut path = SimplePath::default();
                for item in tp.items {
                    let result = SimplePathSegment::try_from(item);
                    if let Ok(a) = result {
                        path.items.push(a);
                    } else {
                        return Err(s.panic(token, "macro call can't contain Self"));
                    }
                }
                s.plusplus();
                let call = s.parse_macro_call(path, token)?;
                let payload = TypePayload::TypeNoBounds(Box::new(TypeNoBounds::MacroCall(call)));
                return Ok(payload);
            }
            _ => {
                return Err(s.panic(token, &format!("illegal token {:?} for type", token)));
            }
        }

        return Err(s.panic(token, "not a type"));
    }

    // unsafe? ( extern Abi? )? fn
    fn parse_func_type(&self) -> ParseResult<TypeBareFunction> {
        let s = self;

        let mut r = TypeBareFunction::default();

        // qualifier
        let token = s.current();
        if matches!(&token.code, T![unsafe]) {
            r.qualifier.t_unsafe = Some(token.clone());
            s.plusplus();
        }
        let token = s.current();
        if matches!(&token.code, T![extern]) {
            s.plusplus();
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

        let token = s.current();
        if matches!(&token.code, T![fn]) {
            r.t_func = Some(token.clone());
            s.plusplus();
        }

        // params
        r.t_paren_open = s.current().clone();
        s.expect(T!["("])?;
        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T![")"]) {
                r.t_paren_close = token.clone();
                s.plusplus();
                break;
            }

            let mut param = MaybeNamedParam::default();
            param.outer_attrs = s.parse_outer_attrs()?;

            // variadics
            if matches!(&token.code, T![...]) {
                r.variadics = Some(MaybeNamedParamVariadics {
                    outer_attrs: param.outer_attrs,
                    t_var: token.clone(),
                });
                s.plusplus();
                r.t_paren_close = s.current().clone();
                s.expect(T![")"])?;
                break;
            }

            // optional param name
            if let TokenCode::Identifier(_) = &token.code &&
                    matches!(&s.nth(1).code, T![:]) {
                param.name = Some(token.clone());
                s.inc(2);
            }

            // param type
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
                    param.show_comma = true;
                    r.params.push(param);
                }
                _ => {
                    return Err(s.panic(token, &format!("expected , or ) but got {:?}", token)));
                }
            }
        }

        let token = s.current();
        if matches!(token.code, T![->]) {
            let t_sep = token.clone();
            s.plusplus();
            // return type
            let type_ = s.parse_type()?;
            if let TypePayload::TypeNoBounds(_) = type_.payload {
                r.return_type = Some((t_sep, type_));
            } else {
                return Err(s.panic(token, "expected TypeNoBounds but got {:?}"));
            }
        }

        Ok(r)
    }

    fn parse_qualifyed_path_type(&self) -> ParseResult<QualifiedPathType> {
        let s = self;

        let mut r = QualifiedPathType::default();
        r.t_square_open = s.current().clone();
        r.t_square_open.code = T!["["];
        s.plusplus();
        r.type_ = s.parse_type()?;
        let token = s.current();
        if matches!(&token.code, T![as]) {
            s.plusplus();
            r.type_path = Some((token.clone(), s.parse_type_path()?));
        }
        r.t_square_close = s.current().clone();
        r.t_square_close.code = T!["]"];
        s.expect(T![>])?;

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    fn parse_where(&self) -> ParseResult<Where> {
        let s = self;

        let mut r = Where::default();

        r.t_where = s.current().clone();
        s.expect(T![where])?;

        while s.has_more() {
            let token = s.current();
            let w_item = if let TokenCode::Lifetime(_) = &token.code {
                let lifetime = token.clone();
                s.plusplus();
                s.expect(T![:])?;
                let bounds = s.parse_lifetime_bounds()?;
                let item = WhereItemLifetime {
                    lifetime: Lifetime {
                        name: lifetime,
                    },
                    lifetime_bounds: bounds.items,
                };
                WhereItem::Lifetime(item)
            } else {
                let mut item = WhereItemTypeBound::default();
                if matches!(token.code, T![for]) {
                    let f = s.parse_for_lifetimes()?;
                    item.for_lifetimes = Some(f);
                }
                item.type_ = s.parse_type()?;
                s.expect(T![:])?;
                item.bounds = s.parse_type_param_bounds()?;
                WhereItem::TypeBound(item)
            };

            let token = s.current();
            match &token.code {
                T![;] | T!["{"] | T![=] => {
                    r.items.push((w_item, false));
                    r.t_after_last = s.current().clone();
                    break;
                }
                T![,] => {
                    s.plusplus();
                    let next_token = s.current();
                    r.items.push((w_item, true));
                    if matches!(&next_token.code, T![;] | T!["{"] | T![=]) {
                        r.t_after_last = next_token.clone();
                        break;
                    }
                }
                _ => {
                    return Err(s.panic(token, &format!("illegal token {:?} for WhereClause", token)));
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

        while s.has_more() {
            let a = s.parse_type_path_segment()?;
            r.items.push(a);

            let token = s.current();
            if !matches!(&token.code, T![::]) {
                break;
            }
        }

        Ok(r)
    }

    fn parse_type_path_segment(&self) -> ParseResult<TypePathSegment> {
        let s = self;

        let token = s.current();
        let t_sep = if matches!(&token.code, T![::]) {
            s.plusplus();
            Some(token.clone())
        } else {
            None
        };

        let token = s.current();
        if !matches!(&token.code,
                T![super] | T![self] | T![Self] | T![crate] | TokenCode::Identifier(_)) {
            return Err(s.panic(token, &format!("required path token but got {:?}", token)));
        }
        let name = PathIdentSegment { token: token.clone() };
        s.plusplus();

        let token = s.current();
        if matches!(&token.code, T!["("]) {
            // it's safe to call parse_func_type, because the next token is (, not [
            let f = s.parse_func_type()?;

            let mut a = TypePathFunc {
                inputs: Vec::new(),
                return_type: None,
                t_paren_open: token.clone(),
                t_paren_close: f.t_paren_close,
            };
            for param in f.params {
                a.inputs.push((param.type_, param.show_comma));
            }
            a.return_type = f.return_type;
            return Ok(TypePathSegment {
                t_sep,
                name,
                payload: Some(TypePathSegmentPayload::TypePathFunc(a)),
            });
        }

        // possible :: before <
        let need_colons = if matches!(&s.current().code, T![::]) &&
                matches!(&s.nth(1).code, T![<]) {
            s.plusplus();
            true
        } else {
            false
        };

        let token = s.current();
        if matches!(&token.code, T![<]) {
            let mut args = s.parse_generic_args()?;
            args.need_colons = need_colons;
            return Ok(TypePathSegment {
                t_sep,
                name,
                payload: Some(TypePathSegmentPayload::GenericArgs(args)),
            });
        }

        return Ok(TypePathSegment {
            t_sep,
            name,
            payload: None,
        });
    }

    fn parse_generic_args(&self) -> ParseResult<GenericArgs> {
        let s = self;

        let mut r = GenericArgs::default();
        r.t_square_open = s.current().clone();
        r.t_square_open.code = T!["["];
        s.plusplus();

        while s.has_more() {
            let token = s.current();
            if matches!(&token.code, T![>]) {
                if r.items.is_empty() {
                    return Err(s.panic(token, "empty generic args"));
                }
                r.t_square_close = token.clone();
                r.t_square_close.code = T!["]"];
                s.plusplus();
                break;
            }

            let arg = parse_generic_arg(s)?;

            let token = s.current();
            let t_comma = if matches!(&token.code, T![,]) {
                s.plusplus();
                Some(token.clone())
            } else {
                None
            };

            r.items.push((arg, t_comma));
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
                TokenCode::Literal(_) => {
                    s.plusplus();
                    GenericArg::Const(GenericArgsConst::LiteralExpr(ExprLiteral(token.clone())))
                }
                T![-] => {
                    s.plusplus();
                    let next_token = s.current();
                    if let TokenCode::Literal(_) = &next_token.code {
                        s.plusplus();
                        GenericArg::Const(GenericArgsConst::MinusLiteralExpr(
                            token.clone(),
                            ExprLiteral(next_token.clone()),
                        ))
                    } else {
                        return Err(s.panic(
                            &next_token,
                            &format!("expected literal but got {:?}", next_token),
                        ));
                    }
                }
                TokenCode::Identifier(_) => {
                    let mut type_ = s.parse_type()?;
                    if let TypePayload::TypeNoBounds(a) = &mut type_.payload &&
                            let TypeNoBounds::TypePath(b) = a.as_mut() &&
                            b.items.len() == 1 {

                        let token = s.current();
                        if matches!(&token.code, T![=]) {
                            s.plusplus();
                            let path_seg = mem::take(&mut b.items[0]);
                            let t_eq = token.clone();
                            let type_ = s.parse_type()?;
                            GenericArg::Binding(GenericArgsBinding {
                                path_seg,
                                type_,
                                t_eq,
                            })
                        } else if matches!(&token.code, T![:]) {
                            s.plusplus();
                            let path_seg = mem::take(&mut b.items[0]);
                            let bounds = s.parse_type_param_bounds()?;
                            GenericArg::Bounds(GenericArgsBounds {
                                path_seg,
                                bounds,
                            })
                        } else {
                            GenericArg::Type(type_)
                        }
                    } else {
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

        while s.has_more() {
            let token = s.current();
            let t_sep = if matches!(&token.code, T![::]) {
                s.plusplus();
                Some(token.clone())
            } else {
                None
            };

            let mut item = s.parse_path_expr_segment()?;
            item.t_sep = t_sep;
            r.push(item);

            let token = s.current();
            if !matches!(&token.code, T![::]) {
                break;
            }
        }

        Ok(r)
    }

    fn parse_path_expr_segment(&self) -> ParseResult<PathExprSegment> {
        let s = self;

        let mut r = PathExprSegment::default();

        let token = s.current();
        s.plusplus();
        if !matches!(&token.code,
                T![super] | T![self] | T![Self] | T![crate] | TokenCode::Identifier(_)) {
            return Err(s.panic(
                token,
                &format!("illegal token {:?} in path_expr_segment", token),
            ));
        }
        r.name = PathIdentSegment { token: token.clone() };

        // possible :: before <
        if matches!(&s.current().code, T![::]) &&
                matches!(&s.nth(1).code, T![<]) {
            s.plusplus();
            r.generic_args = s.parse_generic_args()?;
        }

        Ok(r)
    }

    // --------------------------------------------------------------------------------
    // pattern
    fn parse_pattern(&self) -> ParseResult<Pattern> {
        let s = self;
        let config = ParseConfig::default();
        s.parse_pattern_config(&config)
    }

    fn parse_pattern_config(&self, config: &ParseConfig) -> ParseResult<Pattern> {
        let s = self;

        let mut r = Pattern::default();

        while s.has_more() {
            let pat = s.parse_pattern_no_top_alt_config(config)?;
            let token = s.current();
            if !matches!(&token.code, T![|]) {
                r.items.push((pat, None));
                break;
            }
            r.items.push((pat, Some(token.clone())));
            s.plusplus();
        }

        let token = s.current();
        if r.items.is_empty() {
            return Err(s.panic(token, &format!("expected pattern but got {:?}", token)));
        }

        Ok(r)
    }

    fn parse_pattern_no_top_alt(&self) -> ParseResult<PatternNoTopAlt> {
        let s = self;
        let config = ParseConfig::default();
        s.parse_pattern_no_top_alt_config(&config)
    }

    fn parse_pattern_no_top_alt_config(&self, config: &ParseConfig) -> ParseResult<PatternNoTopAlt> {
        let s = self;

        let token = s.current();
        s.plusplus();
        match &token.code {
            // literal
            T![-] | TokenCode::Literal(_) => {
                let pattern_literal = if let TokenCode::Literal(_) = &token.code {
                    PatternLiteral {
                        name: token.clone(),
                        t_minus: None,
                    }
                } else {
                    let next_token = s.current();
                    s.plusplus();
                    let TokenCode::Literal(_) = &next_token.code else {
                        return Err(s.panic(next_token, "already checked to be Literal"));
                    };
                    PatternLiteral {
                        name: next_token.clone(),
                        t_minus: Some(token.clone()),
                    }
                };

                let token = s.current();
                match &token.code {
                    T![..] => {
                        s.plusplus();
                        let next_token = s.current();
                        if matches!(next_token.code,
                            T![-]
                            | TokenCode::Literal(_)
                            | TokenCode::Identifier(_)
                            | T![super] | T![self] | T![Self] | T![crate]
                            | T!["["]
                        ) {
                            let bound = parse_range_pattern_bound(s, config)?;
                            return Ok(PatternNoTopAlt::WithRange(PatternRange {
                                start: Some(PatternRangeBound::Literal(pattern_literal)),
                                end: Some(bound),
                                t_sep: token.clone(),
                            }));
                        }
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start: Some(PatternRangeBound::Literal(pattern_literal)),
                            end: None,
                            t_sep: token.clone(),
                        }));
                    }
                    T![..=] => {
                        s.plusplus();
                        let bound = parse_range_pattern_bound(s, config)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start: Some(PatternRangeBound::Literal(pattern_literal)),
                            end: Some(bound),
                            t_sep: token.clone(),
                        }));
                    }
                    T![...] => {
                        s.plusplus();
                        let bound = parse_range_pattern_bound(s, config)?;
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::Obsolete(PatternRange {
                                start: Some(PatternRangeBound::Literal(pattern_literal)),
                                end: Some(bound),
                                t_sep: token.clone(),
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
            // .. RangePattern
            // ..= RangePattern
            // ..
            T![..] | T![..=] => {
                let save_state = s.save_state();
                let bound = match parse_range_pattern_bound(s, config) {
                    Ok(bound) => bound,
                    Err(_) => {
                        // rest
                        s.restore_state(&save_state);
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::Rest(token.clone())
                        ));
                    }
                };
                return Ok(PatternNoTopAlt::WithRange(PatternRange {
                    start: None,
                    end: Some(bound),
                    t_sep: token.clone(),
                }));
            }

            // identifier
            T![ref] | T![mut] => {
                s.minusminus();
                let mut r = PatternIdentifier::default();
                if matches!(&token.code, T![ref]) {
                    r.t_ref = Some(token.clone());
                    s.plusplus();
                }
                let token = s.current();
                if matches!(&token.code, T![mut]) {
                    r.t_mut = Some(token.clone());
                    s.plusplus();
                }

                let token = s.current();
                s.plusplus();
                let TokenCode::Identifier(_) = &token.code else {
                    return Err(s.panic(token, &format!("expected identifier but got {:?}", token)));
                };
                r.name = token.clone();

                // possible @
                let token = s.current();
                if matches!(&token.code, T![@]) {
                    let t_at = token.clone();
                    s.plusplus();
                    let pat = s.parse_pattern_no_top_alt_config(config)?;
                    r.pattern = Some((t_at, Box::new(pat)));
                }
                return Ok(PatternNoTopAlt::WithoutRange(
                    PatternWithoutRange::Identifier(r),
                ));
            }

            // grouped or tuple
            T!["("] => {
                let t_paren_open = token.clone();
                let mut list: Vec<(Pattern, bool)> = Vec::new();
                while s.has_more() {
                    let token = s.current();
                    if matches!(&token.code, T![")"]) {
                        s.plusplus();
                        // grouped
                        if list.len() == 1 && !list[0].1 {
                            let a = mem::take(&mut list[0]);
                            return Ok(PatternNoTopAlt::WithoutRange(
                                PatternWithoutRange::Grouped(PatternGrouped {
                                    pattern: Box::new(a.0),
                                    t_paren_open,
                                    t_paren_close: token.clone(),
                                }),
                            ));
                        }

                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::Tuple(PatternTuple {
                                items: list,
                                t_paren_open,
                                t_paren_close: token.clone(),
                            }),
                        ));
                    }

                    let pat = s.parse_pattern()?;

                    let token = s.current();
                    let show_comma = if matches!(&token.code, T![,]) {
                        s.plusplus();
                        true
                    } else {
                        false
                    };
                    list.push((pat, show_comma));
                }
            }
            // slice
            T!["["] => {
                let mut t_square_open = token.clone();
                t_square_open.code = T!["[`"];
                let mut items: Vec<(Pattern, bool)> = Vec::new();
                while s.has_more() {
                    let token = s.current();
                    if matches!(&token.code, T!["]"]) {
                        s.plusplus();
                        let mut t_square_close = token.clone();
                        t_square_close.code = T!["`]"];
                        return Ok(PatternNoTopAlt::WithoutRange(PatternWithoutRange::Slice(
                            PatternSlice {
                                items,
                                t_square_open,
                                t_square_close,
                            },
                        )));
                    }
                    let pat = s.parse_pattern()?;
                    let token = s.current();
                    let show_comma = if matches!(&token.code, T![,]) {
                        s.plusplus();
                        true
                    } else {
                        false
                    };
                    items.push((pat, show_comma));
                }
            }
            // reference
            T![&] | T![&&] => {
                let t_ref = token.clone();

                // possible mut
                let mut t_mut = None;
                let token = s.current();
                if matches!(&token.code, T![mut]) {
                    t_mut = Some(token.clone());
                    s.plusplus();
                }

                let token = s.current();
                let pat = s.parse_pattern_no_top_alt_config(config)?;
                let PatternNoTopAlt::WithoutRange(p) = pat else {
                    return Err(s.panic(token, "expected PatternWithoutRange"));
                };
                return Ok(PatternNoTopAlt::WithoutRange(
                    PatternWithoutRange::Reference(PatternReference {
                        pattern_without_range: Box::new(p),
                        t_ref,
                        t_mut,
                    }),
                ));
            }

            // qualified path in expression
            T![<] => {
                s.minusminus();
                let type_ = s.parse_qualifyed_path_type()?;
                s.expect(T![::])?;
                s.minusminus();
                let items = s.parse_path_expr_segments()?;
                return Ok(PatternNoTopAlt::WithoutRange(PatternWithoutRange::Path(
                    PatternPath::QualifiedPath(QualifiedPathInExpr { type_, items }),
                )));
            }

            // path in expression or macro call
            T![super] | T![self] | T![Self] | T![crate] | TokenCode::Identifier(_) | T![::] => {
                let next_token = s.current();
                if matches!(&next_token.code, T![@]) {
                    // possible @
                    let TokenCode::Identifier(_) = &token.code else {
                        return Err(
                            s.panic(token, &format!("expected identifier but got {:?}", token))
                        );
                    };
                    let mut a = PatternIdentifier::default();
                    a.name = token.clone();
                    let t_at = next_token.clone();
                    s.plusplus();
                    let pat = s.parse_pattern_no_top_alt_config(config)?;
                    a.pattern = Some((t_at, Box::new(pat)));
                    return Ok(PatternNoTopAlt::WithoutRange(
                        PatternWithoutRange::Identifier(a),
                    ));
                }

                s.minusminus();
                let items = s.parse_path_expr_segments()?;
                let token = s.current();
                match &token.code {
                    // macro call
                    T![!] => {
                        let mut path = SimplePath::default();
                        for item in items {
                            if !item.generic_args.items.is_empty() {
                                return Err(
                                    s.panic(token, "macro call can't contain generic params")
                                );
                            }
                            let result = SimplePathSegment::try_from(item);
                            if let Ok(a) = result {
                                path.items.push(a);
                            } else {
                                return Err(s.panic(token, "macro call can't contain Self"));
                            }
                        }
                        s.plusplus();
                        let call = s.parse_macro_call(path, token)?;
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::MacroCall(call),
                        ));
                    }
                    // RangePattern ..
                    T![..] => {
                        s.plusplus();
                        let start = Some(PatternRangeBound::Path(PathInExpr { items }));
                        let next_token = s.current();
                        if !can_start_a_range_bound(&next_token.code) {
                            return Ok(PatternNoTopAlt::WithRange(PatternRange {
                                start,
                                end: None,
                                t_sep: token.clone(),
                            }));
                        }
                        let bound = parse_range_pattern_bound(s, config)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start,
                            end: Some(bound),
                            t_sep: token.clone(),
                        }));
                    }
                    // RangePattern ..= RangePattern
                    T![..=] => {
                        s.plusplus();
                        let start = Some(PatternRangeBound::Path(PathInExpr { items }));
                        let bound = parse_range_pattern_bound(s, config)?;
                        return Ok(PatternNoTopAlt::WithRange(PatternRange {
                            start,
                            end: Some(bound),
                            t_sep: token.clone(),
                        }));
                    }
                    // RangePattern ... RangePattern
                    T![...] => {
                        s.plusplus();
                        let start = Some(PatternRangeBound::Path(PathInExpr { items }));
                        let bound = parse_range_pattern_bound(s, config)?;
                        return Ok(PatternNoTopAlt::WithoutRange(
                            PatternWithoutRange::Obsolete(PatternRange {
                                start,
                                end: Some(bound),
                                t_sep: token.clone(),
                            }),
                        ));
                    }
                    _ => {
                        let mut path = PathInExpr { items };
                        match &token.code {
                            T!["{"] => {
                                s.plusplus();
                                let mut a = parse_struct_pattern(s)?;
                                a.path = path;
                                a.t_brace_open = token.clone();
                                return Ok(PatternNoTopAlt::WithoutRange(
                                    PatternWithoutRange::Struct(a),
                                ));
                            }
                            T!["("] => {
                                s.plusplus();
                                let t_first_in_pattern = s.current();
                                let mut a = parse_tuple_struct_pattern(s)?;

                                // Some(Pattern) to ?Pattern
                                // but
                                //      Some(
                                //          Pattern
                                //      )
                                // and
                                //      Some("a" | "b")
                                // and
                                //      Some("a") | b
                                // don't change
                                let mut is_optional = false;
                                if path.items.len() == 1 {
                                    let p = &path.items[0];
                                    if p.t_sep.is_none()
                                        && !matches!(&s.current().code, T![|])
                                        && p.name.token.u == "Some"
                                        && p.generic_args.items.is_empty()
                                        && a.items.len() == 1
                                        && a.items[0].0.items.len() == 1
                                        && p.name.token.span.end_line == t_first_in_pattern.span.line
                                    {
                                        is_optional = true;
                                    }
                                }
                                if is_optional {
                                    let mut pat = mem::take(&mut a.items[0]);
                                    let pat = mem::take(&mut pat.0.items[0]);
                                    let pattern = Box::new(pat.0);
                                    let t_sep = mem::take(&mut path.items[0]).name.token;
                                    let r = PatternSome {
                                        pattern,
                                        t_sep,
                                    };
                                    return Ok(PatternNoTopAlt::WithoutRange(
                                        PatternWithoutRange::PatSome(r),
                                    ));
                                }

                                a.path = path;
                                a.t_paren_open = token.clone();
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

        return Err(s.panic(token, "not a pattern"));

        fn can_start_a_range_bound(code: &TokenCode) -> bool {
            matches!(code,
                T![-]
                | TokenCode::Literal(_)
                | T![<]
                | T![::]
                | TokenCode::Identifier(_)
                | T![super]
                | T![self]
                | T![Self]
                | T![crate]
            )
        }

        fn parse_range_pattern_bound(
            s: &Parse,
            config: &ParseConfig,
        ) -> ParseResult<PatternRangeBound> {
            use PatternNoTopAlt as A;
            use PatternWithoutRange as B;
            use PatternPath as C;
            use PatternRangeBound as R;

            let token = s.current();
            let pat = s.parse_pattern_no_top_alt_config(config)?;
            Ok(match pat {
                A::WithoutRange(B::Literal(a)) => R::Literal(a),
                A::WithoutRange(B::Path(C::Path(p))) => R::Path(p),
                A::WithoutRange(B::Path(C::QualifiedPath(p))) => R::QualifiedPath(p),
                _ => {
                    return Err(s.panic(token, "not a pattern range bound"));
                }
            })
        }

        // after {
        fn parse_struct_pattern(s: &Parse) -> ParseResult<PatternStruct> {
            let mut r = PatternStruct::default();

            while s.has_more() {
                let token = s.current();
                if matches!(&token.code, T!["}"]) {
                    r.t_brace_close = token.clone();
                    s.plusplus();
                    break;
                }
                let a = parse_struct_pattern_item(s)?;
                r.items.push(a);
            }

            return Ok(r);

            fn parse_struct_pattern_item(s: &Parse) -> ParseResult<PatternStructItem> {
                let outer_attrs = s.parse_outer_attrs()?;

                let token = s.current();
                s.plusplus();

                if matches!(&token.code, T![..]) {
                    return Ok(PatternStructItem::Etc(token.clone()));
                }

                let mut r = match &token.code {
                    TokenCode::Literal(Literal {
                        payload: LiteralPayload::Int(_),
                        ..
                    }) => {
                        let t_colon = s.current().clone();
                        s.expect(T![:])?;
                        let pattern = s.parse_pattern()?;
                        PatternStructField {
                            outer_attrs,
                            payload: PatternStructFieldPayload::A(PatternStructFieldA {
                                pattern,
                                t_colon,
                                t_tuple_index: token.clone(),
                            }),
                            show_comma: false,
                            show_backslash: false,
                        }
                    }
                    TokenCode::Identifier(_) => {
                        if matches!(&s.current().code, T![:]) {
                            let t_colon = s.current().clone();
                            s.plusplus();
                            let pattern = s.parse_pattern()?;
                            PatternStructField {
                                outer_attrs,
                                payload: PatternStructFieldPayload::B(PatternStructFieldB {
                                    name: token.clone(),
                                    t_colon,
                                    pattern,
                                }),
                                show_comma: false,
                                show_backslash: false,
                            }
                        } else {
                            let mut c = PatternStructFieldC::default();
                            c.name = token.clone();
                            PatternStructField {
                                outer_attrs,
                                payload: PatternStructFieldPayload::C(c),
                                show_comma: false,
                                show_backslash: false,
                            }
                        }
                    }
                    T![ref] => {
                        let mut c = PatternStructFieldC::default();
                        c.t_ref = Some(token.clone());
                        let token = s.current();
                        if matches!(&token.code, T![mut]) {
                            c.t_mut = Some(token.clone());
                            s.plusplus();
                        }
                        let token = s.current();
                        if let TokenCode::Identifier(_) = &token.code {
                            c.name = token.clone();
                            s.plusplus();
                        } else {
                            return Err(s.panic(
                                &token,
                                &format!("expected identifier but got {:?}", token),
                            ));
                        }
                        PatternStructField {
                            outer_attrs,
                            payload: PatternStructFieldPayload::C(c),
                            show_comma: false,
                            show_backslash: false,
                        }
                    }
                    T![mut] => {
                        let mut c = PatternStructFieldC::default();
                        c.t_mut = Some(token.clone());
                        let token = s.current();
                        if let TokenCode::Identifier(_) = &token.code {
                            c.name = token.clone();
                            s.plusplus();
                        } else {
                            return Err(s.panic(
                                &token,
                                &format!("expected identifier but got {:?}", token),
                            ));
                        }
                        PatternStructField {
                            outer_attrs,
                            payload: PatternStructFieldPayload::C(c),
                            show_comma: false,
                            show_backslash: false,
                        }
                    }
                    _ => {
                        return Err(
                            s.panic(&token, &format!("illegal token {:?} for pattern", token))
                        );
                    }
                };

                let token = s.current();
                if matches!(&token.code, T![,]) {
                    s.plusplus();
                    r.show_comma = true;
                } else if matches!(&token.code, T!["}"]) {
                    s.minusminus();
                    let before_token = s.current();
                    s.plusplus();
                    r.show_backslash = before_token.span.line != token.span.line;
                } else {
                    return Err(s.panic(token,
                        &format!("illegal token after field {:?}", token)));
                }

                Ok(PatternStructItem::Field(r))
            }
        }

        // after (
        fn parse_tuple_struct_pattern(s: &Parse) -> ParseResult<PatternTupleStruct> {
            let mut r = PatternTupleStruct::default();

            while s.has_more() {
                let token = s.current();
                if matches!(&token.code, T![")"]) {
                    r.t_paren_close = token.clone();
                    s.plusplus();
                    break;
                }
                let pat = s.parse_pattern()?;
                let token = s.current();
                let show_comma = if matches!(&token.code, T![,]) {
                    s.plusplus();
                    true
                } else {
                    false
                };
                r.items.push((pat, show_comma));
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

// to only apply these rules on the outer-most level
#[derive(Debug, Default)]
struct ParseConfig {
    // prevent the following case, when {}(a) is intepreted as an expr.
    // not happening in u-to-rs, because {} always has an auto added semicolon after it
    //
    // match tuple {
    //     1 => {}
    //     (a) => {}
    // }
    stop_on_block_expr: bool,
    stop_on_struct_brace: bool,
    in_conditions: bool,
    in_match_conditions: bool,
    may_end_with_semicolon: bool,
}
