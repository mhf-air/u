use super::*;
use crate::compile as cc;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::io;
use std::mem;

/** usage: u u-fmt <file-path>

rules:
    - format multiline struct declaration, function declaration's parameters, struct init expression
    - group these items seperated by empty lines

    - skip comments, string literals, attributes
    - replace line-leading spaces with tabs
    - remove line-trailling spaces
*/
pub(crate) struct CmdFmt {
    src: String,
    tab_stop: u32,
    field_gap: usize,
}
impl CmdFmt {
    pub(crate) fn new(mut args: Vec<String>) -> CmdFmt {
        let tab_stop = 4;
        let field_gap = 1;
        if args.is_empty() {
            return CmdFmt {
                src: "".to_string(),
                tab_stop,
                field_gap,
            };
        }

        let src = mem::take(&mut args[0]);
        CmdFmt {
            src,
            tab_stop,
            field_gap,
        }
    }

    pub(crate) fn run(&self) -> io::Result<()> {
        let s = self;

        if s.src.is_empty() {
            return Err(io::Error::new(io::ErrorKind::Other, "empty file path"));
        };
        let text = fs::read_to_string(&s.src)?;

        match run_inner(text, &s) {
            Ok(text) => {
                // println!("{}", text);
                fs::write(&s.src, text)?;
            }
            Err(_) => {}
        };

        Ok(())
    }
}

fn run_inner(data: String, config: &CmdFmt) -> Result<String, String> {
    let data_clone = data.clone();

    let mut l = cc::Lex::new();
    if l.lex(data).is_err() {
        return Err(data_clone);
    }
    let mut fmt = Fmt::new(&config);
    fmt.parse_skipped_lines(&l);

    let mut p = cc::Parse::new(l);
    p.set_magic();
    if p.parse().is_err() {
        return Err(data_clone);
    }
    fmt.fmt_pkg(p.get_pkg());

    let mut r = String::new();
    let mut pos = Pos { line: 1, column: 1 };

    let tab_stop = config.tab_stop;
    let list: Vec<char> = data_clone.chars().collect();
    let mut i = 0;
    let len = list.len();
    'outer: while i < len {
        if list[i] == '\n' {
            r.push(list[i]);
            inc_i(&mut i, &mut pos, 1);
            pos.line += 1;
            pos.column = 1;
            if fmt.skipped_lines.contains(&pos.line) {
                continue;
            }
            if i >= len {
                break;
            }

            // skip tabs
            while list[i] == '\t' {
                inc_i(&mut i, &mut pos, 1);
                r.push('\t');
            }

            // spaces to tabs
            if list[i] == ' ' {
                let mut space_count = 1;
                inc_i(&mut i, &mut pos, 1);
                while list[i] == ' ' {
                    space_count += 1;
                    inc_i(&mut i, &mut pos, 1);
                }
                let tabs = space_count / tab_stop;
                for _ in 0..tabs {
                    r.push('\t');
                }
                let spaces = space_count % tab_stop;
                for _ in 0..spaces {
                    r.push(' ');
                }
            }
            continue;
        }

        // remove line-trailling spaces
        if list[i] == ' ' {
            let mut space_count = 1;
            let mut a = i + 1;
            while a < len {
                match list[a] {
                    ' ' => {
                        space_count += 1;
                    }
                    '\n' => {
                        inc_i(&mut i, &mut pos, space_count);
                        continue 'outer;
                    }
                    _ => break,
                }
                a += 1;
            }
            if a >= len {
                break;
            }
        }

        if let Some(a) = fmt.items.get(&pos) {
            for _ in 0..a.expected_space_count {
                r.push(' ');
            }
            if list[i] == ' ' {
                while list[i] == ' ' {
                    inc_i(&mut i, &mut pos, 1);
                }
            } else {
                r.push(list[i]);
                inc_i(&mut i, &mut pos, 1);
            }
        } else {
            if !fmt.skipped_chars.contains(&pos) {
                r.push(list[i]);
            }
            inc_i(&mut i, &mut pos, 1);
        }
    }

    return Ok(r);

    fn inc_i(i: &mut usize, pos: &mut Pos, n: usize) {
        *i += n;
        pos.column += n;
    }
}

// ----------------------------------------------------------------------
#[derive(Debug)]
struct Fmt {
    items: BTreeMap<Pos, Edit>,
    skipped_lines: BTreeSet<usize>,
    field_gap: usize,
    skipped_chars: BTreeSet<Pos>,
}
#[derive(Debug)]
struct Edit {
    // pos: Pos,
    expected_space_count: usize,
}
#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
struct Pos {
    line: usize,
    column: usize,
}

impl Fmt {
    fn new(config: &CmdFmt) -> Self {
        Self {
            items: BTreeMap::new(),
            skipped_lines: BTreeSet::new(),
            field_gap: config.field_gap,
            skipped_chars: BTreeSet::new(),
        }
    }

    // skip multiline string literals and comments
    fn parse_skipped_lines(&mut self, lex: &cc::Lex) {
        let s = self;
        for token in &lex.tokens {
            match &token.code {
                cc::TokenCode::Literal(cc::Literal {
                    payload: cc::LiteralPayload::String(a),
                    ..
                }) => {
                    let count = a.text.chars().filter(|ch| *ch == '\n').count();
                    if count == 0 {
                        continue;
                    }
                    for i in 1..=count {
                        s.skipped_lines.insert(token.span.line + i);
                    }
                }
                _ => {}
            }
        }

        for comment in &lex.comments {
            if comment.is_line {
                continue;
            }
            let count = comment.text.chars().filter(|ch| *ch == '\n').count();
            if count == 0 {
                continue;
            }
            for i in 1..=count {
                s.skipped_lines.insert(comment.span.line + i);
            }
        }
    }

    fn fmt_pkg(&mut self, pkg: cc::Package) {
        let s = self;
        for item in pkg.items {
            s.fmt_item(item);
        }
    }

    fn fmt_item(&mut self, item: cc::Item) {
        let s = self;
        match item.payload {
            cc::ItemPayload::Mod(a) => {
                for b in a.items {
                    s.fmt_item(b);
                }
            }
            cc::ItemPayload::Test(a) => {
                for b in a.items {
                    s.fmt_item(b);
                }
            }
            cc::ItemPayload::Func(a) => {
                s.fmt_params(a.params, &a.span_paren_open);
                if let Some(body) = a.body {
                    s.fmt_block(body);
                }
            }
            cc::ItemPayload::Struct(a) => {
                if let Some(payload) = a.payload {
                    match payload {
                        cc::StructPayload::Struct(fields) => {
                            s.fmt_fields(fields, &a.span_brace_open);
                        }
                        cc::StructPayload::Tuple(_) => {}
                    }
                }
            }
            cc::ItemPayload::Union(a) => {
                s.fmt_fields(a.fields, &a.span_brace_open);
            }
            cc::ItemPayload::Enum(a) => {
                for item in a.items {
                    match item.payload {
                        cc::EnumItemPayload::Struct(fields) => {
                            s.fmt_fields(fields, &a.span_brace_open)
                        }
                        cc::EnumItemPayload::Tuple(_) => {}
                        cc::EnumItemPayload::None => {}
                    }
                    if let Some(a) = item.discriminant {
                        s.fmt_expr(a);
                    }
                }
            }
            cc::ItemPayload::Interface(a) => {
                for item in a.items {
                    match item.payload {
                        cc::AssociatedItemPayload::Func(a) => {
                            s.fmt_params(a.params, &a.span_paren_open);
                            if let Some(body) = a.body {
                                s.fmt_block(body);
                            }
                        }
                        cc::AssociatedItemPayload::Const(a) => {
                            if let Some(a) = a.expr {
                                s.fmt_expr(a);
                            }
                        }
                        cc::AssociatedItemPayload::TypeAlias(_) => {}
                        cc::AssociatedItemPayload::MacroCall(a) => s.fmt_macro_call(a),
                    }
                }
            }
            cc::ItemPayload::Impl(a) => {
                for item in a.items {
                    match item.payload {
                        cc::AssociatedItemPayload::Func(a) => {
                            s.fmt_params(a.params, &a.span_paren_open);
                            if let Some(body) = a.body {
                                s.fmt_block(body);
                            }
                        }
                        cc::AssociatedItemPayload::Const(a) => {
                            if let Some(a) = a.expr {
                                s.fmt_expr(a);
                            }
                        }
                        cc::AssociatedItemPayload::TypeAlias(_) => {}
                        cc::AssociatedItemPayload::MacroCall(a) => s.fmt_macro_call(a),
                    }
                }
            }
            cc::ItemPayload::Const(a) => {
                if let Some(expr) = a.expr {
                    s.fmt_expr(expr);
                }
            }
            cc::ItemPayload::Static(a) => {
                if let Some(expr) = a.expr {
                    s.fmt_expr(expr);
                }
            }
            cc::ItemPayload::MacroCall(a) => {
                s.fmt_macro_call(a);
            }
            cc::ItemPayload::Import(a) => {
                s.fmt_import(a.items);
            }
            cc::ItemPayload::Crate(_)
            | cc::ItemPayload::Extern(_)
            | cc::ItemPayload::TypeAlias(_)
            | cc::ItemPayload::Macro(_) => {}
        }
    }
    fn fmt_macro_call(&mut self, call: cc::MacroCall) {
        let s = self;
        match call.body {
            cc::MacroCallBody::Expr(exprs) => {
                for expr in exprs {
                    s.fmt_expr(expr);
                }
            }
            cc::MacroCallBody::Stmt(stmts) => {
                for stmt in stmts {
                    s.fmt_stmt(stmt);
                }
            }
            cc::MacroCallBody::Token(_) => {}
            cc::MacroCallBody::UCustomMod(_) => {}
        }
    }
    fn fmt_import(&mut self, items: Vec<cc::ImportItem>) {
        let s = self;
        if items.is_empty() {
            return;
        }
        for item in items {
            if let Some(span) = item.leading_sep {
                s.skipped_chars.insert(Pos {
                    line: span.line,
                    column: span.column,
                });
                s.skipped_chars.insert(Pos {
                    line: span.line,
                    column: span.column + 1,
                });
            }
            s.fmt_import(item.sub);
        }
    }
    fn fmt_stmt(&mut self, stmt: cc::Stmt) {
        let s = self;
        match stmt {
            cc::Stmt::Item(a) => s.fmt_item(a),
            cc::Stmt::Let(a) => {
                if let Some(expr) = a.expr {
                    s.fmt_expr(expr);
                }
            }
            cc::Stmt::Expr(a) => s.fmt_expr(a),
            cc::Stmt::Value(a) => s.fmt_expr(a.expr),
        }
    }
    fn fmt_expr(&mut self, expr: cc::Expr) {
        let s = self;

        match expr.payload {
            cc::ExprPayload::WithBlock(a) => match *a {
                cc::ExprWithBlock::BlockExpr(a) => s.fmt_block(a),
                cc::ExprWithBlock::AsyncBlock(a) => s.fmt_block(a.block),
                cc::ExprWithBlock::UnsafeBlock(a) => s.fmt_block(a.block),
                cc::ExprWithBlock::ConstBlock(a) => s.fmt_block(a.block),
                cc::ExprWithBlock::Loop(a) => s.fmt_block(a.body),
                cc::ExprWithBlock::If(a) => s.fmt_if(a),
                cc::ExprWithBlock::Match(a) => {
                    s.fmt_expr(a.expr);
                    for arm in a.arms {
                        s.fmt_expr(arm.expr);
                    }
                }
            },
            cc::ExprPayload::WithoutBlock(a) => match *a {
                cc::ExprWithoutBlock::Literal(_) => {}
                cc::ExprWithoutBlock::Path(_) => {}

                cc::ExprWithoutBlock::Operator(a) => match a {
                    cc::ExprOperator::Borrow(a) => s.fmt_expr(a.expr),
                    cc::ExprOperator::Dereference(a) => s.fmt_expr(a.expr),
                    cc::ExprOperator::ErrorPropagation(a) => s.fmt_expr(a.expr),
                    cc::ExprOperator::Negation(a) => s.fmt_expr(a.expr),
                    cc::ExprOperator::ArithmeticOrLogical(a) => {
                        s.fmt_expr(a.left);
                        s.fmt_expr(a.right);
                    }
                    cc::ExprOperator::Comparison(a) => {
                        s.fmt_expr(a.left);
                        s.fmt_expr(a.right);
                    }
                    cc::ExprOperator::LazyBoolean(a) => {
                        s.fmt_expr(a.left);
                        s.fmt_expr(a.right);
                    }
                    cc::ExprOperator::TypeCast(a) => s.fmt_expr(a.expr),
                    cc::ExprOperator::Assignment(a) => {
                        s.fmt_expr(a.left);
                        s.fmt_expr(a.right);
                    }
                    cc::ExprOperator::CompoundAssignment(a) => {
                        s.fmt_expr(a.left);
                        s.fmt_expr(a.right);
                    }
                },
                cc::ExprWithoutBlock::Grouped(a) => s.fmt_expr(a.expr),
                cc::ExprWithoutBlock::Array(a) => match a.payload {
                    cc::ExprArrayPayload::One(a) => {
                        s.fmt_expr(a.value);
                        s.fmt_expr(a.len);
                    }
                    cc::ExprArrayPayload::Many(a) => {
                        for item in a.0 {
                            s.fmt_expr(item);
                        }
                    }
                },
                cc::ExprWithoutBlock::Await(a) => s.fmt_expr(a.expr),
                cc::ExprWithoutBlock::Index(a) => {
                    s.fmt_expr(a.left);
                    s.fmt_expr(a.right);
                }
                cc::ExprWithoutBlock::Tuple(a) => {
                    for item in a.items {
                        s.fmt_expr(item);
                    }
                }
                cc::ExprWithoutBlock::TupleIndexing(a) => s.fmt_expr(a.left),
                cc::ExprWithoutBlock::Struct(a) => match a {
                    cc::ExprStruct::Struct(a) => {
                        s.fmt_struct_expr(a);
                    }
                    cc::ExprStruct::Tuple(a) => {
                        for expr in a.exprs {
                            s.fmt_expr(expr);
                        }
                    }
                },
                cc::ExprWithoutBlock::Call(a) => {
                    s.fmt_expr(a.left);
                    for item in a.params {
                        s.fmt_expr(item);
                    }
                }
                cc::ExprWithoutBlock::MethodCall(a) => {
                    s.fmt_expr(a.left);
                    for item in a.params {
                        s.fmt_expr(item);
                    }
                }
                cc::ExprWithoutBlock::Field(a) => s.fmt_expr(a.left),
                cc::ExprWithoutBlock::Closure(a) => s.fmt_expr(a.expr),
                cc::ExprWithoutBlock::Continue(_) => {}
                cc::ExprWithoutBlock::Break(a) => {
                    if let Some(a) = a.expr {
                        s.fmt_expr(a);
                    }
                }
                cc::ExprWithoutBlock::Range(a) => {
                    if let Some(a) = a.start {
                        s.fmt_expr(a);
                    }
                    if let Some(a) = a.end {
                        s.fmt_expr(a);
                    }
                }
                cc::ExprWithoutBlock::Return(a) => {
                    if let Some(a) = a.expr {
                        s.fmt_expr(a);
                    }
                }
                cc::ExprWithoutBlock::MacroCall(a) => s.fmt_macro_call(a),
            },
        }
    }
    fn fmt_block(&mut self, block: cc::BlockExpr) {
        let s = self;
        for stmt in block.stmts {
            s.fmt_stmt(stmt);
        }
    }
    fn fmt_conditions(&mut self, conditions: cc::Conditions) {
        let s = self;
        match conditions {
            cc::Conditions::Expr(expr) => {
                s.fmt_expr(expr);
            }
            cc::Conditions::LetChain(list) => {
                for item in list {
                    match item {
                        cc::LetChainCondition::Expr(expr) => {
                            s.fmt_expr(expr);
                        }
                        cc::LetChainCondition::Condition(a) => {
                            s.fmt_expr(a.scrutinee);
                        }
                    }
                }
            }
        }
    }
    fn fmt_if(&mut self, expr_if: cc::ExprIf) {
        let s = self;
        s.fmt_conditions(expr_if.conditions);
        s.fmt_block(expr_if.body);
        if let Some(e) = expr_if.else_ {
            match e {
                cc::ExprIfElse::BlockExpr(a) => s.fmt_block(a),
                cc::ExprIfElse::If(a) => s.fmt_if(*a),
            }
        }
    }

    // ----------------------------------------------------------------------
    fn fmt_params(&mut self, params: Vec<cc::FuncParam>, span_open: &cc::Span) {
        let s = self;

        if params.is_empty() {
            return;
        }
        if span_open.line == params[0].name.span.line {
            return;
        }

        let mut m = BTreeMap::new();
        let mut last_line = 0;
        let mut max_len = 0;
        let mut group_index = 0;
        let mut group_m = BTreeMap::new();

        for item in &params {
            let param = GetMaxLenParam {
                m: &mut m,
                last_line: &mut last_line,
                max_len: &mut max_len,
                group_index: &mut group_index,
                group_m: &mut group_m,
                span: &item.name.span,
                span_right: 0,
            };
            Self::get_max_len(param);
        }

        for item in &params {
            Self::add_edit(s, item.name.span, &m, &group_m);
        }
    }
    fn fmt_fields(&mut self, fields: Vec<cc::StructField>, span_open: &Option<cc::Span>) {
        let s = self;

        if fields.is_empty() {
            return;
        }
        if let Some(span_open) = span_open {
            if span_open.line == fields[0].name.span.line {
                return;
            }
        }

        let mut m = BTreeMap::new();
        let mut last_line = 0;
        let mut max_len = 0;
        let mut group_index = 0;
        let mut group_m = BTreeMap::new();

        for item in &fields {
            let span_right = if item.public.is_default {
                0
            } else {
                match &item.public.payload {
                    cc::VisibilityPayload::Private => 7, // +(self)
                    cc::VisibilityPayload::Pub => 1,     // +
                    cc::VisibilityPayload::Super => 8,   // +(super)
                    cc::VisibilityPayload::Crate => 8,   // +(crate)
                    cc::VisibilityPayload::SimplePath(path) => {
                        // +(in crate..a)
                        let mut sum = 6;
                        for (i, a) in path.items.iter().enumerate() {
                            if i != 0 {
                                sum += 2;
                            }
                            sum += match a {
                                cc::SimplePathSegment::Identifier(a) => a.id.len(),
                                cc::SimplePathSegment::Super(_) => 5,
                                cc::SimplePathSegment::SelfValue(_) => 4,
                                cc::SimplePathSegment::Crate(_) => 5,
                            }
                        }
                        sum
                    }
                }
            };
            let param = GetMaxLenParam {
                m: &mut m,
                last_line: &mut last_line,
                max_len: &mut max_len,
                group_index: &mut group_index,
                group_m: &mut group_m,
                span: &item.name.span,
                span_right,
            };
            Self::get_max_len(param);
        }

        for item in &fields {
            Self::add_edit(s, item.name.span, &m, &group_m);
        }
    }
    fn fmt_struct_expr(&mut self, expr: cc::ExprStructStruct) {
        let s = self;

        if expr.fields.is_empty() {
            return;
        }
        let first_span = match &expr.fields[0] {
            cc::ExprStructField::A(a) => a.name.span,
            cc::ExprStructField::B(a) => a.name.span,
            cc::ExprStructField::C(a) => a.span_index,
            cc::ExprStructField::Base(_, span) => *span,
        };
        if expr.span_brace_open.line == first_span.line {
            return;
        }

        let mut m = BTreeMap::new();
        let mut last_line = 0;
        let mut max_len = 0;
        let mut group_index = 0;
        let mut group_m = BTreeMap::new();

        for item in &expr.fields {
            match item {
                cc::ExprStructField::B(a) => {
                    let param = GetMaxLenParam {
                        m: &mut m,
                        last_line: &mut last_line,
                        max_len: &mut max_len,
                        group_index: &mut group_index,
                        group_m: &mut group_m,
                        span: &a.name.span,
                        span_right: 1,
                    };
                    Self::get_max_len_struct_expr(param, &a.expr);
                }
                cc::ExprStructField::C(a) => {
                    let param = GetMaxLenParam {
                        m: &mut m,
                        last_line: &mut last_line,
                        max_len: &mut max_len,
                        group_index: &mut group_index,
                        group_m: &mut group_m,
                        span: &a.span_index,
                        span_right: 1,
                    };
                    Self::get_max_len_struct_expr(param, &a.expr);
                }
                _ => {}
            }
        }

        for item in expr.fields {
            match item {
                cc::ExprStructField::A(_) => {}
                cc::ExprStructField::B(a) => {
                    Self::add_edit(s, a.name.span, &m, &group_m);
                    s.fmt_expr(a.expr);
                }
                cc::ExprStructField::C(a) => {
                    Self::add_edit(s, a.span_index, &m, &group_m);
                    s.fmt_expr(a.expr);
                }
                cc::ExprStructField::Base(a, _) => {
                    if let Some(a) = a {
                        s.fmt_expr(a);
                    }
                }
            }
        }
    }

    fn get_max_len(param: GetMaxLenParam) {
        Self::get_max_len_inner(param, None);
    }
    fn get_max_len_struct_expr(param: GetMaxLenParam, expr: &cc::Expr) {
        let need_new_group = if param.span.line > *param.last_line + 1 {
            true
        } else if multiline_expr(expr, param.span.line) {
            true
        } else {
            false
        };
        Self::get_max_len_inner(param, Some(need_new_group));
    }

    fn get_max_len_inner(param: GetMaxLenParam, need_new_group: Option<bool>) {
        let GetMaxLenParam {
            m,
            last_line,
            max_len,
            group_index,
            group_m,
            span,
            span_right,
        } = param;

        let need_new_group = if span.line > *last_line + 1 {
            true
        } else if let Some(a) = need_new_group {
            a
        } else {
            false
        };
        if need_new_group {
            *max_len = 0;
            *group_index += 1;
        }
        *last_line = span.line;

        let new_len = span.width + span_right;
        if new_len > *max_len {
            *max_len = new_len;
        }

        group_m.insert(*group_index, *max_len);

        m.insert(
            Pos {
                line: span.line,
                column: span.column,
            },
            (*group_index, span_right),
        );
    }

    fn add_edit(
        s: &mut Fmt,
        span: cc::Span,
        m: &BTreeMap<Pos, (usize, usize)>,
        group_m: &BTreeMap<usize, usize>,
    ) {
        let (group_index, span_right) = m
            .get(&Pos {
                line: span.line,
                column: span.column,
            })
            .unwrap();
        let max_len = group_m.get(&group_index).unwrap();

        let column = span.column + span.width + span_right;
        let pos = Pos {
            line: span.line,
            column,
        };
        s.items.insert(
            pos,
            Edit {
                // pos,
                expected_space_count: max_len - (span.width + span_right) + s.field_gap,
            },
        );
    }

    // ----------------------------------------------------------------------
}

struct GetMaxLenParam<'a> {
    m: &'a mut BTreeMap<Pos, (usize, usize)>, // (group_index, span_right)
    last_line: &'a mut usize,
    max_len: &'a mut usize,
    group_index: &'a mut usize,
    group_m: &'a mut BTreeMap<usize, usize>, // (group_index, max_len)
    span: &'a cc::Span,
    span_right: usize,
}

fn multiline_expr(expr: &cc::Expr, line: usize) -> bool {
    return match &expr.payload {
        cc::ExprPayload::WithBlock(a) => match a.as_ref() {
            cc::ExprWithBlock::BlockExpr(a) => a.span_brace_close.line > line,
            cc::ExprWithBlock::AsyncBlock(a) => a.block.span_brace_close.line > line,
            cc::ExprWithBlock::UnsafeBlock(a) => a.block.span_brace_close.line > line,
            cc::ExprWithBlock::ConstBlock(a) => a.block.span_brace_close.line > line,
            cc::ExprWithBlock::Loop(a) => a.body.span_brace_close.line > line,
            cc::ExprWithBlock::If(a) => check_if(&a, line),
            cc::ExprWithBlock::Match(a) => a.span_brace_close.line > line,
        },
        cc::ExprPayload::WithoutBlock(a) => match a.as_ref() {
            cc::ExprWithoutBlock::Literal(a) => {
                let mut r = false;
                if let cc::LiteralPayload::String(a) = &a.0.payload {
                    for ch in a.text.chars() {
                        if ch == '\n' {
                            r = true;
                            break;
                        }
                    }
                }

                r
            }
            cc::ExprWithoutBlock::Path(a) => {
                let seg = match a {
                    cc::ExprPath::Path(a) => a.items.last().unwrap(),
                    cc::ExprPath::QualifiedPath(a) => a.items.last().unwrap(),
                };
                if !seg.generic_args.items.is_empty() {
                    seg.generic_args.span_square_close.line > line
                } else {
                    let span = match &seg.name {
                        cc::PathIdentSegment::Identifier(a) => &a.span,
                        cc::PathIdentSegment::Super(a) => a,
                        cc::PathIdentSegment::SelfValue(a) => a,
                        cc::PathIdentSegment::SelfType(a) => a,
                        cc::PathIdentSegment::Crate(a) => a,
                    };
                    span.line > line
                }
            }

            cc::ExprWithoutBlock::Operator(a) => match a {
                cc::ExprOperator::Borrow(a) => multiline_expr(&a.expr, line),
                cc::ExprOperator::Dereference(a) => multiline_expr(&a.expr, line),
                cc::ExprOperator::ErrorPropagation(a) => a.span.line > line,
                cc::ExprOperator::Negation(a) => multiline_expr(&a.expr, line),
                cc::ExprOperator::ArithmeticOrLogical(a) => multiline_expr(&a.right, line),
                cc::ExprOperator::Comparison(a) => multiline_expr(&a.right, line),
                cc::ExprOperator::LazyBoolean(a) => multiline_expr(&a.right, line),
                cc::ExprOperator::TypeCast(a) => a.span_as.line > line,
                cc::ExprOperator::Assignment(a) => multiline_expr(&a.right, line),
                cc::ExprOperator::CompoundAssignment(a) => multiline_expr(&a.right, line),
            },
            cc::ExprWithoutBlock::Grouped(a) => a.span_paren_close.line > line,
            cc::ExprWithoutBlock::Array(a) => a.span_double_brace_close.line > line,
            cc::ExprWithoutBlock::Await(a) => a.span.line > line,
            cc::ExprWithoutBlock::Index(a) => multiline_expr(&a.right, line),
            cc::ExprWithoutBlock::Tuple(a) => a.span_paren_close.line > line,
            cc::ExprWithoutBlock::TupleIndexing(a) => a.span_index.line > line,
            cc::ExprWithoutBlock::Struct(a) => match a {
                cc::ExprStruct::Struct(a) => a.span_brace_close.line > line,
                cc::ExprStruct::Tuple(a) => a.span_paren_close.line > line,
            },
            cc::ExprWithoutBlock::Call(a) => a.span_paren_close.line > line,
            cc::ExprWithoutBlock::MethodCall(a) => a.span_paren_close.line > line,
            cc::ExprWithoutBlock::Field(a) => a.name.span.line > line,
            cc::ExprWithoutBlock::Closure(a) => multiline_expr(&a.expr, line),
            cc::ExprWithoutBlock::Continue(_) => false,
            cc::ExprWithoutBlock::Break(a) => {
                if let Some(a) = &a.expr {
                    multiline_expr(&a, line)
                } else {
                    false
                }
            }
            cc::ExprWithoutBlock::Range(a) => {
                if let Some(a) = &a.end {
                    multiline_expr(&a, line)
                } else {
                    a.span.line > line
                }
            }
            cc::ExprWithoutBlock::Return(a) => {
                if let Some(a) = &a.expr {
                    multiline_expr(&a, line)
                } else {
                    false
                }
            }
            cc::ExprWithoutBlock::MacroCall(a) => match a.body {
                cc::MacroCallBody::Expr(_) | cc::MacroCallBody::UCustomMod(_) => {
                    a.span_paren_close.unwrap().line > line
                }
                cc::MacroCallBody::Token(_) | cc::MacroCallBody::Stmt(_) => {
                    a.span_brace_close.unwrap().line > line
                }
            },
        },
    };

    fn check_if(a: &cc::ExprIf, line: usize) -> bool {
        if let Some(else_) = &a.else_ {
            match else_ {
                cc::ExprIfElse::BlockExpr(a) => a.span_brace_close.line > line,
                cc::ExprIfElse::If(a) => check_if(&a, line),
            }
        } else {
            a.body.span_brace_close.line > line
        }
    }
}
