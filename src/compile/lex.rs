use super::ast::*;
use super::parse::*;
use std::fmt;
use std::str::FromStr;

pub struct Lex {
    pub tokens: Vec<Token>,
    // pure comments and whitespaces at the end of the file
    pub end_tokens: Vec<Token>,

    chars: Vec<char>,
    tokens_before: Vec<Token>,
    line: usize,
    column: usize,
}

impl Lex {
    pub fn new() -> Lex {
        Lex {
            tokens: Vec::new(),
            end_tokens: Vec::new(),
            chars: Vec::new(),
            tokens_before: Vec::new(),
            line: 1,
            column: 1,
        }
    }

    fn panic(&self, a: &str) -> ParseResult<()> {
        let s = self;

        // panic!("{}:{}: {}", s.line, s.column, a);

        let e = ParseError {
            line: s.line,
            column: s.column,
            width: 1,
            text: a.to_string(),
        };
        Err(e)
    }

    fn add_token(&mut self, code: TokenCode, u: String) {
        let s = self;
        s.add_token_inner(code, u, false, false);
    }
    // for string
    fn add_token_nl(&mut self, code: TokenCode, u: String) {
        let s = self;
        s.add_token_inner(code, u, true, false);
    }
    // for single-line comment, backslash
    fn add_token_to_before(&mut self, code: TokenCode, u: String) {
        let s = self;
        s.add_token_inner(code, u, false, true);
    }
    // for whitespace, multi-line comment
    fn add_token_to_before_nl(&mut self, code: TokenCode, u: String) {
        let s = self;
        s.add_token_inner(code, u, true, true);
    }

    fn add_token_inner(
        &mut self,
        code: TokenCode,
        u: String,
        check_nl: bool,
        to_before: bool,
    ) {
        let s = self;

        // NOTE the width is in bytes, not in characters
        let width = u.len();

        let (end_line, end_column) = if check_nl {
            if let Some(index) = u.as_str().rfind('\n'){
                let count = u.as_bytes().iter().filter(|&&c| c == b'\n').count();
                (s.line + count, width - index)
            } else {
                (s.line, s.column + width - 1)
            }
        } else {
            (s.line, s.column + width - 1)
        };
        let span = Span {
            line: s.line,
            column: s.column,
            width,
            end_line,
            end_column,
        };

        let mut code = code;
        match &mut code {
            TokenCode::Identifier(a) => {
                a.span = span;
            }
            TokenCode::Literal(a) => {
                a.span = span;
            }
            _ => {}
        }

        if to_before {
            s.tokens_before.push(Token { code, span, tokens_before: Vec::new(), u });
        } else {
            let tokens_before = std::mem::take(&mut s.tokens_before);
            s.tokens.push(Token { code, span, tokens_before, u });
        }

        s.line = end_line;
        s.column = end_column + 1;
    }

    // find the last real token(blank, comment)
    fn last_real_token(&self) -> Result<&Token, ParseError> {
        let s = self;
        if !s.tokens.is_empty() {
            return Ok(s.tokens.last().unwrap());
        }

        let e = ParseError {
            line: s.line,
            column: s.column,
            width: 1,
            text: "cannot find the last non-comment token".to_string(),
        };
        Err(e)
    }

    // add semicolon at line end where neccessary
    // after '\n' or before EOF
    fn may_add_semicolon(&mut self) {
        let s = self;
        let Ok(last_token) = s.last_real_token() else {
            return;
        };
        let need = match &last_token.code {
            TokenCode::Identifier(_)
            | TokenCode::Lifetime(_)
            | TokenCode::Literal(_) => true,
            TokenCode::Keyword(k) => match k {
                Keyword::Await
                | Keyword::Break
                | Keyword::Continue
                | Keyword::Crate
                | Keyword::Mod
                | Keyword::Ret
                | Keyword::Super
                | Keyword::Struct
                | Keyword::SelfType
                | Keyword::SelfValue
                | Keyword::Type
                | Keyword::Unsafe
                | Keyword::Unsized => true,
                _ => false,
            },
            TokenCode::Op(op) => match op {
                Op::PlusPlus | Op::MinusMinus | Op::Question | Op::TickTick |
                    Op::Dot | Op::DotDot => true,
                _ => false,
            },
            TokenCode::Pair(pair) => match pair {
                Pair::ParenClose
                | Pair::SquareClose
                | Pair::BraceClose
                | Pair::DoubleBraceClose => true,
                _ => false,
            },
            _ => false,
        };
        if need {
            // auto added semicolon has colomn which is std::usize::MAX
            let new_token = Token {
                code: TokenCode::Op(Op::Semicolon),
                span: Span {
                    line: s.line,
                    column: std::usize::MAX,
                    width: 1,
                    end_line: s.line,
                    end_column: std::usize::MAX,
                },
                tokens_before: Vec::new(),
                u: String::new(),
            };
            s.tokens.push(new_token);
        }
    }

    fn add_comment(&mut self, u: String) {
        let s = self;

        let chars: Vec<char> = u.chars().collect();
        let total_len = chars.len();

        let mut is_line = false;
        let mut is_doc = false;
        let mut inner_doc = false;

        // total_len >= 2
        if chars[1] == '/' {
            is_line = true;
            if total_len > 2 {
                match chars[2] {
                    '/' => {
                        is_doc = true;
                    }
                    '!' => {
                        is_doc = true;
                        inner_doc = true;
                    }
                    _ => {}
                }
            }
        } else {
            // total_len >= 4
            if chars[2] == '*' {
                is_doc = chars[3] != '/';
            } else if chars[2] == '!' {
                is_doc = true;
                inner_doc = true;
            }
        }

        let text = if is_line {
            if is_doc {
                // ///, //!
                chars[3..total_len].iter().collect()
            } else {
                // //
                chars[2..total_len].iter().collect()
            }
        } else {
            if is_doc {
                // /** */, /*! */
                chars[3..total_len - 2].iter().collect()
            } else {
                // /* */
                chars[2..total_len - 2].iter().collect()
            }
        };
        let comment = Comment {
            text,
            is_line,
            is_doc,
            inner_doc,
        };
        if is_line {
            s.add_token_to_before(TokenCode::Comment(comment), u);
        } else {
            s.add_token_to_before_nl(TokenCode::Comment(comment), u);
        }
    }

    fn sub_str(&self, start: usize, end: usize) -> String {
        let s = self;
        s.chars[start..end].iter().collect()
    }

    fn get_literal_suffix(&self, new_i: usize) -> (Option<String>, usize) {
        let s = self;

        let total_len = s.chars.len();
        if new_i >= total_len {
            return (None, 0);
        }
        let c_first = s.chars[new_i];
        if !(c_first.is_alphabetic() || c_first != '-') {
            return (None, 0);
        }
        let mut end = new_i;
        for k in new_i..total_len {
            end = k;
            let c = s.chars[k];
            if !(c.is_alphanumeric() || c == '-') {
                break;
            }
        }
        (Some(s.sub_str(new_i, end)), end)
    }

    // --------------------------------------------------------------------------------
    pub fn lex(&mut self, data: String) -> Result<(), ParseError> {
        let s = self;
        let r = s.lex_inner(data);
        // if the last char is not '\n', add semicolon where neccessary
        s.may_add_semicolon();
        r
    }
    pub fn lex_inner(&mut self, data: String) -> Result<(), ParseError> {
        let s = self;

        if data.is_empty() {
            return Ok(());
        }

        s.chars = data.chars().collect();
        let total_len = s.chars.len();

        let mut id_prefix: Option<(String, usize)> = None;
        let mut i = 0;

        // possible shebang #! at the begining of the file
        if total_len > 2 && s.chars[0] == '#' && s.chars[1] == '!' && s.chars[2] != '[' {
            while i < total_len {
                if s.chars[i] == '\n' {
                    let u = s.sub_str(0, i);
                    s.add_token(TokenCode::Shebang, u);
                    break;
                }
                i += 1;
            }
            if i == total_len {
                let u = s.sub_str(0, i);
                s.add_token(TokenCode::Shebang, u);
                return Ok(());
            }
        }

        // main loop
        let mut is_backslash = false;
        'outer: while i < total_len {
            let ch = s.chars[i];

            if ch == '\\' {
                is_backslash = true;
                s.add_token_to_before(TokenCode::Op(Op::Backslash), "\\".to_owned());
                i += 1;
                continue;
            }

            // whitespace
            if ch.is_whitespace() {
                let start = i;
                while i < total_len {
                    let ch = s.chars[i];
                    if !ch.is_whitespace() {
                        break;
                    }
                    // newline
                    if ch == '\n' {
                        if !is_backslash {
                            s.may_add_semicolon();
                        }
                    }
                    i += 1;
                }
                let u = s.sub_str(start, i);
                s.add_token_to_before_nl(TokenCode::Blank, u);
                continue;
            } else {
                is_backslash = false;
            }

            // ->
            if ch == '-' && i + 1 < total_len && s.chars[i + 1] == '>' {
                let u = s.sub_str(i, i + 2);
                s.add_token(TokenCode::Op(Op::RArrow), u);
                i += 2;
                continue;
            }

            // identifier
            if ch.is_alphabetic() || ch == '-' {
                let start = i;
                for delta in start + 1..total_len {
                    let c = s.chars[delta];
                    if c.is_alphanumeric() || c == '-' {
                        continue;
                    }

                    let a = s.sub_str(start, delta);
                    // prefix for string literal or char literal
                    if c == '"' || c == '\'' {
                        id_prefix = Some((a, start));
                        i = delta;
                        continue 'outer;
                    }
                    if c == '#' {
                        let ch = s.chars[delta + 1];

                        // prefix for string literal
                        // id#"
                        // id##"
                        if ch == '"' || ch == '#' {
                            id_prefix = Some((a, start));
                            i = delta;
                            continue 'outer;
                        }

                        // prefix for identifier
                        // id#id, id#123
                        if !(ch.is_alphanumeric() || ch == '-') {
                            return s.panic("expected identifier or digit after #");
                        }

                        id_prefix = Some((a, start));
                        i = delta + 1;
                        continue 'outer;
                    }

                    // add identifier
                    if let Some(prefix) = id_prefix {
                        // with prefix
                        let u = s.sub_str(prefix.1, delta);
                        let id = Identifier {
                            prefix: Some(prefix.0),
                            id: a,
                            span: Span::default(),
                            u: u.clone(),
                        };
                        s.add_token(TokenCode::Identifier(id), u);
                        id_prefix = None;
                    } else {
                        // no prefix
                        if a == "true" || a == "false" {
                            s.add_token(TokenCode::Literal(Literal::new(LiteralPayload::Bool(
                                a.clone(),
                            ))), a);
                        } else {
                            let keyword = Keyword::from_str(&a);
                            if let Ok(key) = keyword {
                                s.add_token(TokenCode::Keyword(key), a);
                            } else {
                                let id = Identifier::new(a.clone(), a.clone());
                                s.add_token(TokenCode::Identifier(id), a);
                            }
                        }
                    }
                    i = delta;
                    continue 'outer;
                }
                // reached EOF here
                let u = s.sub_str(start, total_len);
                let id = Identifier::new(u.clone(), u.clone());
                s.add_token(TokenCode::Identifier(id), u);
                return Ok(());
            }

            // number
            if ch.is_digit(10) {
                let start = i;

                // with leading 0
                if ch == '0' {
                    if i + 1 == total_len {
                        let u = s.sub_str(start, total_len);
                        s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                            LiteralPayload::Int(u.clone()),
                            &mut id_prefix,
                            None,
                        )), u);
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // hex
                        'x' | 'X' => {
                            i += 1;
                            for delta in start + 2..total_len {
                                let c = s.chars[delta];
                                if ('0'..='9').contains(&c)
                                    || ('a'..='f').contains(&c)
                                    || ('A'..='F').contains(&c)
                                    || c == '_'
                                {
                                    continue;
                                }
                                i = delta;
                                break;
                            }
                        }
                        // octal
                        'o' | 'O' => {
                            i += 1;
                            for delta in start + 2..total_len {
                                let c = s.chars[delta];
                                if ('0'..='7').contains(&c) || c == '_' {
                                    continue;
                                }
                                i = delta;
                                break;
                            }
                        }
                        // binary
                        'b' | 'B' => {
                            i += 1;
                            for delta in start + 2..total_len {
                                let c = s.chars[delta];
                                if c == '0' || c == '1' || c == '_' {
                                    continue;
                                }
                                i = delta;
                                break;
                            }
                        }
                        _ => {}
                    }
                    // add number
                    if i > start {
                        if i - start == 1 {
                            return s.panic(&format!("expected digit after 0{}, but got EOF",
                                next_ch));
                        }
                        if i - start == 2 {
                            return s.panic(&format!("expected digit after 0{}, but got {:?}",
                                next_ch, s.chars[start + 2]));
                        }
                        // possible suffix
                        let (suffix, end) = s.get_literal_suffix(i);
                        let a = s.sub_str(start, i);
                        let u = s.sub_str(start, end);
                        s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                            LiteralPayload::Int(a),
                            &mut id_prefix,
                            suffix,
                        )), u);
                        i = end;
                        continue;
                    }
                }

                /* decimal
                DEC_LITERAL:
                   DEC_DIGIT (DEC_DIGIT|_)*

                FLOAT_LITERAL:
                    DEC_LITERAL (. DEC_LITERAL)? FLOAT_EXPONENT? FLOAT_SUFFIX?
                FLOAT_EXPONENT :
                   (e|E) (+|~)? (DEC_DIGIT|_)* DEC_DIGIT (DEC_DIGIT|_)*
                */

                // skip DEC_LITERAL
                let new_start = i;
                for delta in new_start..total_len {
                    let c = s.chars[delta];
                    if ('0'..='9').contains(&c) || c == '_' {
                        continue;
                    }
                    i = delta;
                    // for cases such as a.0.0
                    if let Some(a) = s.tokens.last() &&
                        matches!(a.code, TokenCode::Op(Op::Dot))
                    {
                        let u = s.sub_str(start, i);
                        s.add_token(TokenCode::Literal(Literal {
                            payload: LiteralPayload::Int(u.clone()),
                            prefix: None,
                            suffix: None,
                            span: Span::default(),
                        }), u);
                        continue 'outer;
                    }
                    break;
                }
                // possible .
                let mut is_float = false;
                if s.chars[i] == '.' {
                    i += 1;
                    // skip DEC_LITERAL
                    let new_i = i;
                    for delta in new_i..total_len {
                        let c = s.chars[delta];
                        if ('0'..='9').contains(&c) || c == '_' {
                            continue;
                        }
                        i = delta;
                        break;
                    }
                    // no digit after INT. translates to INT and .
                    if i == new_i {
                        let u = s.sub_str(start, i - 1);
                        s.add_token(TokenCode::Literal(Literal {
                            payload: LiteralPayload::Int(u.clone()),
                            prefix: None,
                            suffix: None,
                            span: Span::default(),
                        }), u);
                        s.add_token(TokenCode::Op(Op::Dot), ".".to_owned());
                        continue 'outer;
                    }
                    is_float = true;
                }
                // possible e | E
                if s.chars[i] == 'e' || s.chars[i] == 'E' {
                    is_float = true;
                    i += 1;
                    if i == total_len {
                        return s.panic(&format!("expected stuff after float literal {:?}",
                            s.chars[i - 1]));
                    }
                    // possible + | ~
                    if s.chars[i] == '+' || s.chars[i] == '~' {
                        i += 1;
                    }
                    if i == total_len {
                        return s.panic(&format!("expected stuff after float literal {:?}",
                            s.chars[i - 1]));
                    }
                    // possible _
                    if s.chars[i] == '_' {
                        i += 1;
                    }
                    if i == total_len {
                        return s.panic(&format!("expected stuff after float literal {:?}",
                            s.chars[i - 1]));
                    }
                    // digit
                    if !(('0'..='9').contains(&s.chars[i]) || s.chars[i] == '_') {
                        return s.panic("expected digit after float literal");
                    }
                    i += 1;
                    // skip DEC_LITERAL
                    let new_i = i;
                    for delta in new_i..total_len {
                        let c = s.chars[delta];
                        if ('0'..='9').contains(&c) || c == '_' {
                            continue;
                        }
                        i = delta;
                        break;
                    }
                }
                // possible suffix
                let (suffix, end) = s.get_literal_suffix(i);
                let a = s.sub_str(start, i);
                let u = s.sub_str(start, end);
                if is_float {
                    s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                        LiteralPayload::Float(a),
                        &mut id_prefix,
                        suffix,
                    )), u);
                } else {
                    s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                        LiteralPayload::Int(a),
                        &mut id_prefix,
                        suffix,
                    )), u);
                }
                i = end;
                continue;
            }

            // other
            match ch {
                '/' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Div), "/".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // //
                        '/' => {
                            let start = i;
                            for delta in (start + 2)..total_len {
                                if s.chars[delta] == '\n' {
                                    let a = s.sub_str(start, delta);
                                    s.add_comment(a);
                                    i = delta;
                                    continue 'outer;
                                }
                            }
                            if i == total_len {
                                let u = s.sub_str(start, total_len);
                                s.add_comment(u);
                                return Ok(());
                            }
                        }
                        // /* */
                        '*' => {
                            let start = i;
                            let mut comment_count = 1;
                            let mut skip = false;
                            for delta in (start + 2)..total_len {
                                if skip {
                                    skip = false;
                                    continue;
                                }
                                let c = s.chars[delta];
                                if delta + 1 == total_len {
                                    return s.panic("expected */ after /*");
                                }
                                let next_c = s.chars[delta + 1];
                                if c == '/' && next_c == '*' {
                                    comment_count += 1;
                                    skip = true;
                                    continue;
                                }
                                if c == '*' && next_c == '/' {
                                    comment_count -= 1;
                                    if comment_count == 0 {
                                        let u = s.sub_str(start, delta + 2);
                                        s.add_comment(u);
                                        i = delta + 2;
                                        continue 'outer;
                                    }
                                }
                            }
                            return s.panic("expected */ after /*");
                        }
                        // /=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::Div), "/=".to_owned());
                            i += 1;
                        }
                        // /
                        _ => {
                            s.add_token(TokenCode::Op(Op::Div), "/".to_owned());
                        }
                    }
                }

                // string
                '"' => {
                    let start = i;
                    let mut skip = false;
                    for delta in (start + 1)..total_len {
                        if skip {
                            skip = false;
                            continue;
                        }
                        let c = s.chars[delta];
                        if c == '\\' {
                            skip = true;
                            continue;
                        }
                        if c == '"' {
                            let (suffix, end) = s.get_literal_suffix(delta + 1);
                            let text = s.sub_str(start + 1, delta);
                            let u = if let Some(prefix) = &id_prefix {
                                s.sub_str(prefix.1, end)
                            } else {
                                s.sub_str(start, end)
                            };
                            s.add_token_nl(TokenCode::Literal(Literal::new_with_prefix(
                                LiteralPayload::String(
                                    LiteralString {
                                        raw_count: 0,
                                        text,
                                    }
                                ),
                                &mut id_prefix,
                                suffix,
                            )), u);
                            i = end;
                            continue 'outer;
                        }
                    }
                    return s.panic("expected closing \"");
                }

                // lifetime or char
                '\'' => {
                    if i + 1 == total_len {
                        return s.panic("expected char after '");
                    }
                    let next_ch = s.chars[i + 1];
                    if next_ch == '\'' {
                        return s.panic("expected char in ''");
                    }

                    if next_ch.is_alphabetic() || next_ch == '-' {
                        let mut is_lifetime = true;
                        let start = i;
                        let mut end = start + 2;
                        for k in (start + 2)..total_len {
                            end = k;
                            let c = s.chars[k];
                            if c == '\'' {
                                is_lifetime = false;
                                break;
                            }
                            if !(c.is_alphanumeric() || c == '-' || c == '#') {
                                break;
                            }
                        }

                        // lifetime
                        if is_lifetime {
                            let a = s.sub_str(start + 1, end);
                            let u = s.sub_str(start, end);
                            let id = Identifier::new(a, u.clone());
                            s.add_token(TokenCode::Lifetime(id), u);
                            i = end;
                            continue 'outer;
                        }
                    }

                    // char
                    let start = i;
                    let mut skip = false;
                    for delta in (start + 1)..total_len {
                        if skip {
                            skip = false;
                            continue;
                        }
                        let c = s.chars[delta];
                        if c == '\\' {
                            skip = true;
                            continue;
                        }
                        if c == '\'' {
                            let (suffix, end) = s.get_literal_suffix(delta + 1);
                            let a = s.sub_str(start + 1, delta);
                            let u = if let Some(prefix) = &id_prefix {
                                s.sub_str(prefix.1, end)
                            } else {
                                s.sub_str(start, end)
                            };
                            s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                                LiteralPayload::Char(LiteralChar { text: a }),
                                &mut id_prefix,
                                suffix,
                            )), u);
                            i = end;
                            continue 'outer;
                        }
                    }
                    return s.panic("expected closing '");
                }

                '+' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Add), "+".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // ++
                        '+' => {
                            s.add_token(TokenCode::Op(Op::PlusPlus), "++".to_owned());
                            i += 1;
                        }
                        // +=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::Add), "+=".to_owned());
                            i += 1;
                        }
                        // +
                        _ => {
                            s.add_token(TokenCode::Op(Op::Add), "+".to_owned());
                        }
                    }
                },

                '~' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Sub), "~".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // ~~
                        '~' => {
                            s.add_token(TokenCode::Op(Op::MinusMinus), "~~".to_owned());
                            i += 1;
                        }
                        // ~=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::Sub), "~=".to_owned());
                            i += 1;
                        }
                        // ~
                        _ => {
                            s.add_token(TokenCode::Op(Op::Sub), "~".to_owned());
                        }
                    }
                },

                '*' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Star), "*".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // *=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::Star), "*=".to_owned());
                            i += 1;
                        }
                        // *
                        _ => {
                            s.add_token(TokenCode::Op(Op::Star), "*".to_owned());
                        }
                    }
                },

                '%' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Rem), "%".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // %=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::Rem), "%=".to_owned());
                            i += 1;
                        }
                        // %
                        _ => {
                            s.add_token(TokenCode::Op(Op::Rem), "%".to_owned());
                        }
                    }
                },

                '!' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Not), "!".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // !=
                        '=' => {
                            s.add_token(TokenCode::Op(Op::Neq), "!=".to_owned());
                            i += 1;
                        }
                        // !
                        _ => {
                            s.add_token(TokenCode::Op(Op::Not), "!".to_owned());
                        }
                    }
                },

                '^' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::BitXor), "^".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // ^=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::BitXor), "^=".to_owned());
                            i += 1;
                        }
                        // ^
                        _ => {
                            s.add_token(TokenCode::Op(Op::BitXor), "^".to_owned());
                        }
                    }
                },

                '|' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::BitOr), "|".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // ||
                        '|' => {
                            s.add_token(TokenCode::Op(Op::Or), "||".to_owned());
                            i += 1;
                        }
                        // |=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::BitOr), "|=".to_owned());
                            i += 1;
                        }
                        // |
                        _ => {
                            s.add_token(TokenCode::Op(Op::BitOr), "|".to_owned());
                        }
                    }
                },

                '=' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Assign(Assign::Assign), "=".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // ==
                        '=' => {
                            s.add_token(TokenCode::Op(Op::Eq), "==".to_owned());
                            i += 1;
                        }
                        // =>
                        '>' => {
                            s.add_token(TokenCode::Op(Op::FatArrow), "=>".to_owned());
                            i += 1;
                        }
                        // =
                        _ => {
                            s.add_token(TokenCode::Assign(Assign::Assign), "=".to_owned());
                        }
                    }
                },

                ':' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Colon), ":".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // :=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::Declare), ":=".to_owned());
                            i += 1;
                        }
                        // ::
                        ':' => {
                            s.add_token(TokenCode::Op(Op::ColonColon), "::".to_owned());
                            i += 1;
                        }
                        // :
                        _ => {
                            s.add_token(TokenCode::Op(Op::Colon), ":".to_owned());
                        }
                    }
                },

                ',' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Comma), ",".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // ,,
                        ',' => {
                            s.add_token(TokenCode::Op(Op::CommaComma), ",,".to_owned());
                            i += 1;
                        }
                        // ,
                        _ => {
                            s.add_token(TokenCode::Op(Op::Comma), ",".to_owned());
                        }
                    }
                },

                '`' => {
                    if i + 1 == total_len {
                        return s.panic("expected ` after `");
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        '`' => {
                            if i + 2 >= total_len {
                                // ``
                                s.add_token(TokenCode::Op(Op::TickTick), "``".to_owned());
                                i += 1;
                            } else {
                                match s.chars[i + 2] {
                                    '=' => {
                                        // ``=
                                        s.add_token(TokenCode::Op(Op::TickTickEq), "``=".to_owned());
                                        i += 2;
                                    }
                                    _ => {
                                        // ``
                                        s.add_token(TokenCode::Op(Op::TickTick), "``".to_owned());
                                        i += 1;
                                    }
                                }
                            }
                        }
                        _ => {
                            return s.panic("expected ` after `");
                        }
                    }
                },

                '.' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Dot), ".".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        '.' => {
                            if i + 2 >= total_len {
                                // ..
                                s.add_token(TokenCode::Op(Op::DotDot), "..".to_owned());
                                i += 1;
                            } else {
                                match s.chars[i + 2] {
                                    '.' => {
                                        // ...
                                        s.add_token(TokenCode::Op(Op::DotDotDot), "...".to_owned());
                                        i += 2;
                                    }
                                    '=' => {
                                        // ..=
                                        s.add_token(TokenCode::Op(Op::DotDotEq), "..=".to_owned());
                                        i += 2;
                                    }
                                    _ => {
                                        // ..
                                        s.add_token(TokenCode::Op(Op::DotDot), "..".to_owned());
                                        i += 1;
                                    }
                                }
                            }
                        }
                        /* .
                        if the last non-comment token is an added semicolon, drop the semicolon.
                        for cases like:
                            list
                                .filter()
                                .map()
                        */
                        _ => {
                            let last_token = s.last_real_token()?;
                            if last_token.is_added_semicolon() {
                                s.tokens.pop();
                            }
                            s.add_token(TokenCode::Op(Op::Dot), ".".to_owned());
                        }
                    }
                },

                '<' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Less), "<".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // <=
                        '=' => {
                            s.add_token(TokenCode::Op(Op::LessEq), "<=".to_owned());
                            i += 1;
                        }
                        '<' => {
                            if i + 2 < total_len && s.chars[i + 2] == '=' {
                                // <<=
                                s.add_token(TokenCode::Assign(Assign::LeftShift), "<<=".to_owned());
                                i += 2;
                            } else {
                                // <<
                                s.add_token(TokenCode::Op(Op::LeftShift), "<<".to_owned());
                                i += 1;
                            }
                        }
                        // <
                        _ => {
                            s.add_token(TokenCode::Op(Op::Less), "<".to_owned());
                        }
                    }
                },

                '>' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Greater), ">".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // >=
                        '=' => {
                            s.add_token(TokenCode::Op(Op::GreaterEq), ">=".to_owned());
                            i += 1;
                        }
                        '>' => {
                            if i + 2 < total_len && s.chars[i + 2] == '=' {
                                // >>=
                                s.add_token(TokenCode::Assign(Assign::RightShift), ">>=".to_owned());
                                i += 2;
                            } else {
                                // >>
                                s.add_token(TokenCode::Op(Op::RightShift), ">>".to_owned());
                                i += 1;
                            }
                        }
                        // >
                        _ => {
                            s.add_token(TokenCode::Op(Op::Greater), ">".to_owned());
                        }
                    }
                },

                '&' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::BitAnd), "&".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // &&
                        '&' => {
                            s.add_token(TokenCode::Op(Op::And), "&&".to_owned());
                            i += 1;
                        }
                        // &=
                        '=' => {
                            s.add_token(TokenCode::Assign(Assign::BitAnd), "&=".to_owned());
                            i += 1;
                        }
                        // &
                        _ => {
                            s.add_token(TokenCode::Op(Op::BitAnd), "&".to_owned());
                        }
                    }
                },

                '{' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Pair(Pair::BraceOpen), "{".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // {{
                        '{' => {
                            s.add_token(TokenCode::Pair(Pair::DoubleBraceOpen), "{{".to_owned());
                            i += 1;
                        }
                        // {
                        _ => {
                            s.add_token(TokenCode::Pair(Pair::BraceOpen), "{".to_owned());
                        }
                    }
                },
                '}' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Pair(Pair::BraceClose), "}".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // }}
                        '}' => {
                            s.add_token(TokenCode::Pair(Pair::DoubleBraceClose), "}}".to_owned());
                            i += 1;
                        }
                        // }
                        _ => {
                            s.add_token(TokenCode::Pair(Pair::BraceClose), "}".to_owned());
                        }
                    }
                },

                '#' => {
                    if i + 1 == total_len {
                        s.add_token(TokenCode::Op(Op::Pound), "#".to_owned());
                        return Ok(());
                    }
                    let next_ch = s.chars[i + 1];
                    match next_ch {
                        // #[
                        '[' => {
                            s.add_token(TokenCode::Pair(Pair::OuterAttr), "#[".to_owned());
                            i += 1;
                        }
                        // #![
                        '!' => {
                            if i + 2 == total_len {
                                s.add_token(TokenCode::Op(Op::Pound), "#".to_owned());
                                s.add_token(TokenCode::Op(Op::Not), "!".to_owned());
                                return Ok(());
                            }
                            if s.chars[i + 2] != '[' {
                                return s.panic("expected [ after #!");
                            }
                            s.add_token(TokenCode::Pair(Pair::InnerAttr), "#![".to_owned());
                            i += 2;
                        }
                        // #" "#
                        // ##" "##
                        // ###" "###
                        '"' | '#' => {
                            let mut raw_count = 1;
                            let new_i = i + 1;
                            for delta in new_i..total_len {
                                let c = s.chars[delta];
                                if c == '#' {
                                    raw_count += 1;
                                    continue;
                                }
                                if c != '"' {
                                    return s.panic("expected \" after #");
                                }
                                break;
                            }

                            let start = i;
                            let new_i = i + raw_count + 1;
                            for delta in new_i..total_len {
                                let c = s.chars[delta];
                                // find "###
                                if c != '"' {
                                    continue;
                                }
                                if delta + 1 == total_len {
                                    return s.panic("expected closing \"#");
                                }
                                let mut count = 0;
                                for j in delta + 1..total_len {
                                    let c = s.chars[j];
                                    if c == '#' {
                                        count += 1;
                                        if count == raw_count {
                                            break;
                                        }
                                        continue;
                                    }
                                    break;
                                }
                                if count != raw_count {
                                    continue;
                                }

                                let (suffix, end) = s.get_literal_suffix(delta + raw_count + 1);
                                let a = s.sub_str(new_i, delta);
                                let u = if let Some(prefix) = &id_prefix {
                                    s.sub_str(prefix.1, end)
                                } else {
                                    s.sub_str(start, end)
                                };
                                s.add_token_nl(TokenCode::Literal(Literal::new_with_prefix(
                                    LiteralPayload::String(
                                        LiteralString {
                                            raw_count,
                                            text: a,
                                        }
                                    ),
                                    &mut id_prefix,
                                    suffix,
                                )), u);
                                i = end;
                                continue 'outer;
                            }
                            return s.panic("expected closing \"#");
                        }
                        _ => s.add_token(TokenCode::Op(Op::Pound), "#".to_owned()),
                    }
                },

                '(' => s.add_token(TokenCode::Pair(Pair::ParenOpen), "(".to_owned()),
                ')' => s.add_token(TokenCode::Pair(Pair::ParenClose), ")".to_owned()),
                '[' => s.add_token(TokenCode::Pair(Pair::SquareOpen), "[".to_owned()),
                ']' => s.add_token(TokenCode::Pair(Pair::SquareClose), "]".to_owned()),
                ';' => s.add_token(TokenCode::Op(Op::Semicolon), ";".to_owned()),
                '?' => s.add_token(TokenCode::Op(Op::Question), "?".to_owned()),
                '@' => s.add_token(TokenCode::Op(Op::At), "@".to_owned()),
                '$' => s.add_token(TokenCode::Op(Op::Dollar), "$".to_owned()),

                _ => return s.panic(&format!("illegal char: {}", ch)),
            }

            i += 1;
        }

        if !s.tokens_before.is_empty() {
            s.end_tokens = std::mem::take(&mut s.tokens_before);
        }

        Ok(())
    }
}

impl fmt::Debug for Lex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::with_capacity(self.tokens.len());
        for item in self.tokens.iter() {
            buf.push_str(&format!(" {:?}", item));
        }
        write!(f, "{}", buf)
    }
}

// --------------------------------------------------------------------------------
#[derive(Clone, Default)]
pub struct Token {
    pub code: TokenCode,
    pub span: Span,
    pub tokens_before: Vec<Token>,
    pub u: String,
}

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    // width in bytes
    pub width: usize,
    // some tokens have a different ending, such as multi-line string and comment
    pub end_line: usize,
    pub end_column: usize,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self;
        write!(f, "{:?}", s.code)
        // write!(f, "{:?}{}", s.code, s.column)
    }
}

impl ToLang for Token {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;
        s.to_rs_before(p);
        s.to_rs_one(p);
    }
}

impl Token {
    #[allow(dead_code)]
    pub fn println(&self) {
        println!("{:?}", self);
    }

    pub fn is_added_semicolon(&self) -> bool {
        let s = self;
        if let TokenCode::Op(Op::Semicolon) = s.code {
            if s.span.column == std::usize::MAX {
                return true;
            }
        }
        false
    }

    pub fn to_rs_before(&self, p: &mut LangFormatter) {
        let s = self;

        for it in &s.tokens_before {
            it.to_rs_one(p);
        }
    }

    pub fn to_rs_one(&self, p: &mut LangFormatter) {
        let s = self;

        let mut delta_line = 0;

        let rs = match &s.code {
            TokenCode::Shebang => &s.u,
            TokenCode::Blank => {
                delta_line = s.span.end_line - s.span.line;
                &s.u
            }
            TokenCode::Comment(_) => {
                delta_line = s.span.end_line - s.span.line;
                &s.u
            }
            TokenCode::Literal(literal) => {
                delta_line = s.span.end_line - s.span.line;
                &literal.to_rust_string()
            }
            TokenCode::Identifier(identifier) => &identifier.to_rust_string_with_prefix(),
            TokenCode::Lifetime(identifier) => &{
                let mut a = identifier.to_rust_string_with_prefix();
                a.insert_str(0, "'");
                a
            },
            TokenCode::Keyword(keyword) => {
                match keyword {
                    Keyword::Async => "async",
                    Keyword::Await => "await",
                    Keyword::Break => "break",
                    Keyword::Const => "const",
                    Keyword::Continue => "continue",
                    Keyword::Crate => "crate",
                    Keyword::Dyn => "dyn",
                    Keyword::Else => "else",
                    Keyword::Enum => "enum",
                    Keyword::Extern => "extern",
                    Keyword::For => "for",
                    Keyword::Func => "fn",
                    Keyword::If => "if",
                    Keyword::Impl => "impl",
                    Keyword::In => "in",
                    Keyword::Let => "let",
                    Keyword::Macro => "macro_rules",
                    Keyword::Mod => "mod",
                    Keyword::Mut => "mut",
                    Keyword::Ref => "ref",
                    Keyword::Ret => "return",
                    Keyword::SelfType => "Self",
                    Keyword::Static => "static",
                    Keyword::Struct => "struct",
                    Keyword::Super => "super",
                    Keyword::Match => "match",
                    Keyword::Type => "type",
                    Keyword::Unsafe => "unsafe",
                    Keyword::Where => "where",

                    Keyword::As => "as",
                    Keyword::Fn => "fn",
                    Keyword::Loop => "loop",
                    Keyword::Move => "move",
                    Keyword::Pub => "pub",
                    Keyword::Return => "return",
                    Keyword::SelfValue => "self",
                    Keyword::Trait => "trait",
                    Keyword::Use => "use",
                    Keyword::While => "while",

                    Keyword::Abstract => "abstract",
                    Keyword::Become => "become",
                    Keyword::Box => "box",
                    Keyword::Do => "do",
                    Keyword::Final => "final",
                    Keyword::Override => "override",
                    Keyword::Priv => "priv",
                    Keyword::Typeof => "typeof",
                    Keyword::Union => "union",
                    Keyword::Unsized => "unsized",
                    Keyword::Virtual => "virtual",
                    Keyword::Yield => "yield",
                    Keyword::Try => "try",
                    Keyword::Gen => "gen",

                    Keyword::RsFunc => "func",
                    Keyword::RsMacro => "macro",
                    Keyword::RsRet => "ret",
                }
            }
            TokenCode::Assign(assign) => {
                match assign {
                    Assign::Assign => "=",
                    Assign::Declare => "=",
                    Assign::Add => "+=",
                    Assign::Sub => "-=",
                    Assign::Star => "*=",
                    Assign::Div => "/=",
                    Assign::Rem => "%=",
                    Assign::LeftShift => "<<=",
                    Assign::RightShift => ">>=",
                    Assign::BitAnd => "&=",
                    Assign::BitOr => "|=",
                    Assign::BitXor => "^=",
                }
            }
            TokenCode::Op(op) => {
                match op {
                    Op::Add => "+",
                    Op::Sub => "-",
                    Op::Star => "*",
                    Op::Div => "/",
                    Op::Rem => "%",
                    Op::PlusPlus => "+= 1",
                    Op::MinusMinus => "-= 1",
                    Op::Eq => "==",
                    Op::Neq => "!=",
                    Op::Less => "<",
                    Op::LessEq => "<=",
                    Op::Greater => ">",
                    Op::GreaterEq => ">=",
                    Op::LeftShift => "<<",
                    Op::RightShift => ">>",
                    Op::Not => "!",
                    Op::And => "&&",
                    Op::Or => "||",
                    Op::BitAnd => "&",
                    Op::BitOr => "|",
                    Op::BitXor => "^",
                    Op::Dot => ".",
                    Op::DotDot => "::",
                    Op::DotDotDot => "..",
                    Op::Tick => "`",
                    Op::TickTick => "..",
                    Op::TickTickEq => "..=",
                    Op::Pound => "#",
                    Op::Question => "?",
                    Op::At => "@",
                    Op::Comma => ",",
                    Op::CommaComma => "!",
                    Op::Colon => ":",
                    Op::Semicolon => ";",

                    Op::ColonColon => "::",
                    Op::DotDotEq => "..=",
                    Op::RArrow => "->",
                    Op::FatArrow => "=>",
                    Op::Dollar => "$",
                    Op::Backslash => "",

                    Op::RsDotDot => "..",
                    Op::RsDotDotDot => "...",
                }
            }
            TokenCode::Pair(pair) => {
                match pair {
                    Pair::ParenOpen => "(",
                    Pair::ParenClose => ")",
                    Pair::SquareOpen => "<",
                    Pair::SquareClose => ">",
                    Pair::BraceOpen => "{",
                    Pair::BraceClose => "}",
                    Pair::DoubleBraceOpen => "[",
                    Pair::DoubleBraceClose => "]",
                    Pair::OuterAttr => "#[",
                    Pair::InnerAttr => "#![",
                    Pair::EndAttr => "]",
                    Pair::RsSquareOpen => "[",
                    Pair::RsSquareClose => "]",
                }
            }
        };

        p.push_token(rs, delta_line, s.span);
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub enum TokenCode {
    #[default]
    Shebang,
    Blank,
    Comment(Comment),
    Literal(Literal),
    Identifier(Identifier),
    Lifetime(Identifier),
    Keyword(Keyword),
    Assign(Assign),
    Op(Op),
    Pair(Pair),
}

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct Literal {
    pub payload: LiteralPayload,
    pub prefix: Option<String>,
    pub suffix: Option<String>,
    pub span: Span,
}
impl Literal {
    fn new(payload: LiteralPayload) -> Literal {
        Literal {
            payload,
            prefix: None,
            suffix: None,
            span: Span::default(),
        }
    }
    fn new_with_prefix(
        payload: LiteralPayload,
        prefix: &mut Option<(String, usize)>,
        suffix: Option<String>,
    ) -> Literal {
        let p = prefix.clone();
        *prefix = None;
        Literal {
            payload,
            prefix: p.map(|it| it.0),
            suffix,
            span: Span::default(),
        }
    }
    fn to_rust_string(&self) -> String {
        let s = self;

        let mut r = String::new();
        if let Some(prefix) = &s.prefix {
            let prefix = Identifier::to_rust_string_no_prefix(&prefix);
            r.push_str(&prefix);
            if matches!(s.payload, LiteralPayload::Int(_) | LiteralPayload::Float(_)) {
                r.push('#');
            }
        }

        let a = match &s.payload {
            LiteralPayload::String(a) => &a.to_rust_string(),
            LiteralPayload::Char(a) => &a.to_rust_string(),
            LiteralPayload::Int(a) => a,
            LiteralPayload::Float(a) => &a.replace("~", "-"),
            LiteralPayload::Bool(a) => a,
        };
        r.push_str(a);

        if let Some(suffix) = &s.suffix {
            let suffix = Identifier::to_rust_string_no_prefix(&suffix);
            r.push_str(&suffix);
        }
        r
    }
}
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum LiteralPayload {
    Bool(String),
    String(LiteralString),
    Char(LiteralChar),
    Int(String),
    Float(String),
}
impl Default for LiteralPayload {
    fn default() -> Self {
        LiteralPayload::Bool("false".to_string())
    }
}
impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self;

        match &s.payload {
            LiteralPayload::String(a) => write!(f, "{:?}", a),
            LiteralPayload::Char(a) => write!(f, "'{}'", a.text),
            LiteralPayload::Int(a) => write!(f, "{}", a),
            LiteralPayload::Float(a) => write!(f, "{}", a),
            LiteralPayload::Bool(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralString {
    raw_count: usize, // number of pound sign
    text: String,
}
impl LiteralString {
    fn to_rust_string(&self) -> String {
        let s = self;

        let mut p = String::new();

        if s.raw_count > 0 {
            for _ in 0..s.raw_count {
                p.push('#');
            }
        }
        p.push('"');
        p.push_str(&s.text);
        p.push('"');
        for _ in 0..s.raw_count {
            p.push('#');
        }

        p
    }

    pub fn push_span(&self, p: &mut LangFormatter, span: Span) {
        let s = self;
        let a = s.to_rust_string();
        p.push_s(&a, span);
    }
}
impl fmt::Debug for LiteralString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self;
        let mut p = String::new();
        for _ in 0..s.raw_count {
            p.push('#');
        }
        p.push_str("\"");
        p.push_str(&s.text);
        p.push_str("\"");
        for _ in 0..s.raw_count {
            p.push('#');
        }
        write!(f, "{}", p)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralChar {
    text: String,
}
impl LiteralChar {
    fn to_rust_string(&self) -> String {
        let s = self;

        let mut p = String::new();

        p.push('\'');
        p.push_str(&s.text);
        p.push('\'');

        p
    }
}

// TODO
impl ToLang for Identifier {
    fn to_rust(&self, _p: &mut LangFormatter) {
    }
}
impl ToLang for Literal {
    fn to_rust(&self, _p: &mut LangFormatter) {
    }
}

pub const ALLOWED_KEYWORD_NAMES: &[&'static str] = &["func", "ret", "union"];

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub prefix: Option<String>,
    pub id: String,
    pub u: String,
    pub span: Span, // same as Token's span, added here for ease of use
}
impl Identifier {
    pub fn new(id: String, u: String) -> Identifier {
        Identifier {
            id,
            u,
            prefix: None,
            span: Span::default(),
        }
    }

    pub fn to_rust_string_with_prefix(&self) -> String {
        let s = self;

        let mut r = String::new();
        if let Some(prefix) = &s.prefix && !ALLOWED_KEYWORD_NAMES.contains(&s.id.as_str()) {
            r.push_str(&Identifier::to_rust_string_no_prefix(prefix));
            r.push('#');
        }
        r.push_str(&Identifier::to_rust_string_no_prefix(&s.id));
        r
    }

    pub fn to_rust_string_no_prefix(id: &str) -> String {
        let mut p = String::new();
        if id.is_empty() {
            return "".to_string();
        }

        let chars: Vec<char> = id.chars().collect();
        let len = chars.len();

        if len > 2 {
            if chars[len - 3] == '-' && chars[len - 2] == '-' {
                match chars[len - 1] {
                    'c' => {
                        if len == 3 {
                            p.push_str("__c");
                            return p;
                        }
                        let r = &chars[..len - 3];
                        for c in r {
                            if *c == '-' {
                                p.push('_');
                            } else {
                                let a = c.to_uppercase().to_string();
                                p.push_str(&a);
                            }
                        }
                        return p;
                    }
                    'r' => {
                        if len == 3 {
                            return "__r".to_string();
                        }
                        let r = &chars[..len - 3];
                        for c in r {
                            if *c == '-' {
                                p.push('_');
                            } else {
                                let a = c.to_string();
                                p.push_str(&a);
                            }
                        }
                        return p;
                    }
                    _ => {}
                }
            }
        }

        if chars[0].is_uppercase() {
            let list = chars.split(|c| *c == '-');
            for word in list {
                for (i, c) in word.iter().enumerate() {
                    if i == 0 {
                        p.push_str(&c.to_uppercase().to_string());
                    } else {
                        p.push_str(&c.to_string());
                    }
                }
                if word.is_empty() {
                    p.push('_');
                }
            }
            return p;
        }

        for c in &chars {
            if *c == '-' {
                p.push('_');
            } else {
                let a = c.to_string();
                p.push_str(&a);
            }
        }
        return p;
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Async,
    Await,
    Break,
    Const,
    Continue,
    Crate,
    Dyn,
    Else,
    Enum,
    Extern,
    For,
    Func,
    If,
    Impl,
    In,
    Let,
    Macro,
    Mod,
    Mut,
    Ref,
    Ret,
    SelfType,
    Static,
    Struct,
    Super,
    Match,
    Type,
    Unsafe,
    Where,

    As,
    Fn,
    Loop,
    Move,
    Pub,
    Return,
    SelfValue,
    Trait,
    Use,
    While,

    Abstract,
    Become,
    Box,
    Do,
    Final,
    Override,
    Priv,
    Typeof,
    Union,
    Unsized,
    Virtual,
    Yield,
    Try,
    Gen,

    RsFunc,
    RsMacro,
    RsRet,
}

impl fmt::Debug for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Async => write!(f, "async"),
            Self::Await => write!(f, "await"),
            Self::Break => write!(f, "break"),
            Self::Const => write!(f, "const"),
            Self::Continue => write!(f, "continue"),
            Self::Crate => write!(f, "crate"),
            Self::Dyn => write!(f, "dyn"),
            Self::Else => write!(f, "else"),
            Self::Enum => write!(f, "enum"),
            Self::Extern => write!(f, "extern"),
            Self::For => write!(f, "for"),
            Self::Func => write!(f, "func"),
            Self::If => write!(f, "if"),
            Self::Impl => write!(f, "impl"),
            Self::In => write!(f, "in"),
            Self::Let => write!(f, "let"),
            Self::Macro => write!(f, "macro"),
            Self::Mod => write!(f, "mod"),
            Self::Mut => write!(f, "mut"),
            Self::Ref => write!(f, "ref"),
            Self::Ret => write!(f, "ret"),
            Self::SelfType => write!(f, "Self"),
            Self::Static => write!(f, "static"),
            Self::Struct => write!(f, "struct"),
            Self::Super => write!(f, "super"),
            Self::Match => write!(f, "match"),
            Self::Type => write!(f, "type"),
            Self::Unsafe => write!(f, "unsafe"),
            Self::Where => write!(f, "where"),

            Self::As => write!(f, "as"),
            Self::Fn => write!(f, "fn"),
            Self::Loop => write!(f, "loop"),
            Self::Move => write!(f, "move"),
            Self::Pub => write!(f, "pub"),
            Self::Return => write!(f, "return"),
            Self::SelfValue => write!(f, "self"),
            Self::Trait => write!(f, "trait"),
            Self::Use => write!(f, "use"),
            Self::While => write!(f, "while"),

            Self::Abstract => write!(f, "abstract"),
            Self::Become => write!(f, "become"),
            Self::Box => write!(f, "box"),
            Self::Do => write!(f, "do"),
            Self::Final => write!(f, "final"),
            Self::Override => write!(f, "override"),
            Self::Priv => write!(f, "priv"),
            Self::Typeof => write!(f, "typeof"),
            Self::Union => write!(f, "union"),
            Self::Unsized => write!(f, "unsized"),
            Self::Virtual => write!(f, "virtual"),
            Self::Yield => write!(f, "yield"),
            Self::Try => write!(f, "try"),
            Self::Gen => write!(f, "gen"),

            Self::RsFunc => write!(f, "func"),
            Self::RsMacro => write!(f, "macro"),
            Self::RsRet => write!(f, "ret"),
        }
    }
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(a: &str) -> Result<Self, Self::Err> {
        match a {
            "async" => Ok(Self::Async),
            "await" => Ok(Self::Await),
            "break" => Ok(Self::Break),
            "const" => Ok(Self::Const),
            "continue" => Ok(Self::Continue),
            "crate" => Ok(Self::Crate),
            "dyn" => Ok(Self::Dyn),
            "else" => Ok(Self::Else),
            "enum" => Ok(Self::Enum),
            "extern" => Ok(Self::Extern),
            "for" => Ok(Self::For),
            "func" => Ok(Self::Func),
            "if" => Ok(Self::If),
            "impl" => Ok(Self::Impl),
            "in" => Ok(Self::In),
            "let" => Ok(Self::Let),
            "macro" => Ok(Self::Macro),
            "mod" => Ok(Self::Mod),
            "mut" => Ok(Self::Mut),
            "ref" => Ok(Self::Ref),
            "ret" => Ok(Self::Ret),
            "Self" => Ok(Self::SelfType),
            "static" => Ok(Self::Static),
            "struct" => Ok(Self::Struct),
            "super" => Ok(Self::Super),
            "match" => Ok(Self::Match),
            "type" => Ok(Self::Type),
            "unsafe" => Ok(Self::Unsafe),
            "where" => Ok(Self::Where),

            "as" => Ok(Self::As),
            "fn" => Ok(Self::Fn),
            "loop" => Ok(Self::Loop),
            "move" => Ok(Self::Move),
            "pub" => Ok(Self::Pub),
            "return" => Ok(Self::Return),
            "self" => Ok(Self::SelfValue),
            "trait" => Ok(Self::Trait),
            "use" => Ok(Self::Use),
            "while" => Ok(Self::While),

            "abstract" => Ok(Self::Abstract),
            "become" => Ok(Self::Become),
            "box" => Ok(Self::Box),
            "do" => Ok(Self::Do),
            "final" => Ok(Self::Final),
            "override" => Ok(Self::Override),
            "priv" => Ok(Self::Priv),
            "typeof" => Ok(Self::Typeof),
            "union" => Ok(Self::Union),
            "unsized" => Ok(Self::Unsized),
            "virtual" => Ok(Self::Virtual),
            "yield" => Ok(Self::Yield),

            _ => Err(()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Assign {
    Assign,
    Declare,
    Add,
    Sub,
    Star,
    Div,
    Rem,
    LeftShift,
    RightShift,
    BitAnd,
    BitOr,
    BitXor,
}

impl fmt::Debug for Assign {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::Declare => write!(f, ":="),
            Self::Add => write!(f, "+="),
            Self::Sub => write!(f, "~="),
            Self::Star => write!(f, "*="),
            Self::Div => write!(f, "/="),
            Self::Rem => write!(f, "%="),
            Self::LeftShift => write!(f, "<<="),
            Self::RightShift => write!(f, ">>="),
            Self::BitAnd => write!(f, "&="),
            Self::BitOr => write!(f, "|="),
            Self::BitXor => write!(f, "^="),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Op {
    Add,
    Sub,
    Star,
    Div,
    Rem,
    PlusPlus,
    MinusMinus,
    Eq,
    Neq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    LeftShift,
    RightShift,
    Not,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Dot,
    DotDot,
    DotDotDot,
    Tick,
    TickTick,
    TickTickEq,
    Pound,
    Question,
    At,
    Comma,
    CommaComma,
    Colon,
    Semicolon,

    ColonColon,
    DotDotEq,
    RArrow,
    FatArrow,
    Dollar,
    Backslash,

    RsDotDot,
    RsDotDotDot,
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "~"),
            Self::Star => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),
            Self::PlusPlus => write!(f, "++"),
            Self::MinusMinus => write!(f, "~~"),
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),
            Self::LeftShift => write!(f, "<<"),
            Self::RightShift => write!(f, ">>"),
            Self::Not => write!(f, "!"),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::BitAnd => write!(f, "&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),
            Self::Dot => write!(f, "."),
            Self::DotDot => write!(f, ".."),
            Self::DotDotDot => write!(f, "..."),
            Self::Tick => write!(f, "`"),
            Self::TickTick => write!(f, "``"),
            Self::TickTickEq => write!(f, "``="),
            Self::Pound => write!(f, "#"),
            Self::Question => write!(f, "?"),
            Self::At => write!(f, "@"),
            Self::Comma => write!(f, ","),
            Self::CommaComma => write!(f, ",,"),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),

            Self::ColonColon => write!(f, "::"),
            Self::DotDotEq => write!(f, "..="),
            Self::RArrow => write!(f, "->"),
            Self::FatArrow => write!(f, "=>"),
            Self::Dollar => write!(f, "$"),
            Self::Backslash => write!(f, "\\"),

            Self::RsDotDot => write!(f, ".."),
            Self::RsDotDotDot => write!(f, "..."),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Pair {
    ParenOpen,
    ParenClose,
    SquareOpen,
    SquareClose,
    BraceOpen,
    BraceClose,
    DoubleBraceOpen,
    DoubleBraceClose,
    OuterAttr,
    InnerAttr,
    EndAttr,
    RsSquareOpen,
    RsSquareClose,
}

impl fmt::Debug for Pair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ParenOpen => write!(f, "("),
            Self::ParenClose => write!(f, ")"),
            Self::SquareOpen => write!(f, "["),
            Self::SquareClose => write!(f, "]"),
            Self::BraceOpen => write!(f, "{{"),
            Self::BraceClose => write!(f, "}}"),
            Self::DoubleBraceOpen => write!(f, "{{{{"),
            Self::DoubleBraceClose => write!(f, "}}}}"),
            Self::OuterAttr => write!(f, "#["),
            Self::InnerAttr => write!(f, "#!["),
            Self::EndAttr => write!(f, "]"),
            Self::RsSquareOpen => write!(f, "["),
            Self::RsSquareClose => write!(f, "]"),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Comment {
    pub text: String,
    pub is_line: bool,
    pub is_doc: bool,
    pub inner_doc: bool,
}

// --------------------------------------------------------------------------------
#[macro_export]
macro_rules! T {
	// keyword
	[ async ] => { TokenCode::Keyword(Keyword::Async) };
	[ await ] => { TokenCode::Keyword(Keyword::Await) };
	[ break ] => { TokenCode::Keyword(Keyword::Break) };
	[ const ] => { TokenCode::Keyword(Keyword::Const) };
	[ continue ] => { TokenCode::Keyword(Keyword::Continue) };
	[ crate ] => { TokenCode::Keyword(Keyword::Crate) };
	[ dyn ] => { TokenCode::Keyword(Keyword::Dyn) };
	[ else ] => { TokenCode::Keyword(Keyword::Else) };
	[ enum ] => { TokenCode::Keyword(Keyword::Enum) };
	[ extern ] => { TokenCode::Keyword(Keyword::Extern) };
	[ for ] => { TokenCode::Keyword(Keyword::For) };
	[ func ] => { TokenCode::Keyword(Keyword::Func) };
	[ if ] => { TokenCode::Keyword(Keyword::If) };
	[ impl ] => { TokenCode::Keyword(Keyword::Impl) };
	[ in ] => { TokenCode::Keyword(Keyword::In) };
	[ let ] => { TokenCode::Keyword(Keyword::Let) };
	[ macro ] => { TokenCode::Keyword(Keyword::Macro) };
	[ mod ] => { TokenCode::Keyword(Keyword::Mod) };
	[ mut ] => { TokenCode::Keyword(Keyword::Mut) };
	[ ref ] => { TokenCode::Keyword(Keyword::Ref) };
	[ ret ] => { TokenCode::Keyword(Keyword::Ret) };
	[ Self ] => { TokenCode::Keyword(Keyword::SelfType) };
	[ static ] => { TokenCode::Keyword(Keyword::Static) };
	[ struct ] => { TokenCode::Keyword(Keyword::Struct) };
	[ super ] => { TokenCode::Keyword(Keyword::Super) };
	[ match ] => { TokenCode::Keyword(Keyword::Match) };
	[ type ] => { TokenCode::Keyword(Keyword::Type) };
	[ unsafe ] => { TokenCode::Keyword(Keyword::Unsafe) };
	[ where ] => { TokenCode::Keyword(Keyword::Where) };

	[ as ] => { TokenCode::Keyword(Keyword::As) };
	[ fn ] => { TokenCode::Keyword(Keyword::Fn) };
	[ loop ] => { TokenCode::Keyword(Keyword::Loop) };
	[ move ] => { TokenCode::Keyword(Keyword::Move) };
	[ pub ] => { TokenCode::Keyword(Keyword::Pub) };
	[ return ] => { TokenCode::Keyword(Keyword::Return) };
	[ self ] => { TokenCode::Keyword(Keyword::SelfValue) };
	[ trait ] => { TokenCode::Keyword(Keyword::Trait) };
	[ use ] => { TokenCode::Keyword(Keyword::Use) };
	[ while ] => { TokenCode::Keyword(Keyword::While) };

	[ abstract ] => { TokenCode::Keyword(Keyword::Abstract) };
	[ become ] => { TokenCode::Keyword(Keyword::Become) };
	[ box ] => { TokenCode::Keyword(Keyword::Box) };
	[ do ] => { TokenCode::Keyword(Keyword::Do) };
	[ final ] => { TokenCode::Keyword(Keyword::Final) };
	[ override ] => { TokenCode::Keyword(Keyword::Override) };
	[ priv ] => { TokenCode::Keyword(Keyword::Priv) };
	[ typeof ] => { TokenCode::Keyword(Keyword::Typeof) };
	[ union ] => { TokenCode::Keyword(Keyword::Union) };
	[ unsized ] => { TokenCode::Keyword(Keyword::Unsized) };
	[ virtual ] => { TokenCode::Keyword(Keyword::Virtual) };
	[ yield ] => { TokenCode::Keyword(Keyword::Yield) };
	[ try ] => { TokenCode::Keyword(Keyword::Try) };
	[ gen ] => { TokenCode::Keyword(Keyword::Gen) };

	// op
	[ + ] => { TokenCode::Op(Op::Add) };
	[ ~ ] => { TokenCode::Op(Op::Sub) };
	[ * ] => { TokenCode::Op(Op::Star) };
	[ / ] => { TokenCode::Op(Op::Div) };
	[ % ] => { TokenCode::Op(Op::Rem) };
	[ ++ ] => { TokenCode::Op(Op::PlusPlus) };
	[ ~~ ] => { TokenCode::Op(Op::MinusMinus) };
	[ == ] => { TokenCode::Op(Op::Eq) };
	[ != ] => { TokenCode::Op(Op::Neq) };
	[ < ] => { TokenCode::Op(Op::Less) };
	[ <= ] => { TokenCode::Op(Op::LessEq) };
	[ > ] => { TokenCode::Op(Op::Greater) };
	[ >= ] => { TokenCode::Op(Op::GreaterEq) };
	[ << ] => { TokenCode::Op(Op::LeftShift) };
	[ >> ] => { TokenCode::Op(Op::RightShift) };
	[ ! ] => { TokenCode::Op(Op::Not) };
	[ && ] => { TokenCode::Op(Op::And) };
	[ || ] => { TokenCode::Op(Op::Or) };
	[ & ] => { TokenCode::Op(Op::BitAnd) };
	[ | ] => { TokenCode::Op(Op::BitOr) };
	[ ^ ] => { TokenCode::Op(Op::BitXor) };
	[ . ] => { TokenCode::Op(Op::Dot) };
	[ .. ] => { TokenCode::Op(Op::DotDot) };
	[ ... ] => { TokenCode::Op(Op::DotDotDot) };
	[ "``" ] => { TokenCode::Op(Op::TickTick) };
	[ "``=" ] => { TokenCode::Op(Op::TickTickEq) };
	[ # ] => { TokenCode::Op(Op::Pound) };
	[ ? ] => { TokenCode::Op(Op::Question) };
	[ @ ] => { TokenCode::Op(Op::At) };
	[ , ] => { TokenCode::Op(Op::Comma) };
	[ ,, ] => { TokenCode::Op(Op::CommaComma) };
	[ : ] => { TokenCode::Op(Op::Colon) };
	[ ; ] => { TokenCode::Op(Op::Semicolon) };

	[ :: ] => { TokenCode::Op(Op::ColonColon) };
	[ ..= ] => { TokenCode::Op(Op::DotDotEq) };
	[ -> ] => { TokenCode::Op(Op::RArrow) };
	[ => ] => { TokenCode::Op(Op::FatArrow) };
	[ $ ] => { TokenCode::Op(Op::Dollar) };
	[ "\\" ] => { TokenCode::Op(Op::Backslash) };

	// assign
	[ = ] => { TokenCode::Assign(Assign::Assign) };
	[ := ] => { TokenCode::Assign(Assign::Declare) };
	[ += ] => { TokenCode::Assign(Assign::Add) };
	[ ~= ] => { TokenCode::Assign(Assign::Sub) };
	[ *= ] => { TokenCode::Assign(Assign::Star) };
	[ /= ] => { TokenCode::Assign(Assign::Div) };
	[ %= ] => { TokenCode::Assign(Assign::Rem) };
	[ <<= ] => { TokenCode::Assign(Assign::LeftShift) };
	[ >>= ] => { TokenCode::Assign(Assign::RightShift) };
	[ &= ] => { TokenCode::Assign(Assign::BitAnd) };
	[ |= ] => { TokenCode::Assign(Assign::BitOr) };
	[ ^= ] => { TokenCode::Assign(Assign::BitXor) };

	// pair
	[ "(" ] => { TokenCode::Pair(Pair::ParenOpen) };
	[ ")" ] => { TokenCode::Pair(Pair::ParenClose) };
	[ "[" ] => { TokenCode::Pair(Pair::SquareOpen) };
	[ "]" ] => { TokenCode::Pair(Pair::SquareClose) };
	[ "{" ] => { TokenCode::Pair(Pair::BraceOpen) };
	[ "}" ] => { TokenCode::Pair(Pair::BraceClose) };
	[ "{{" ] => { TokenCode::Pair(Pair::DoubleBraceOpen) };
	[ "}}" ] => { TokenCode::Pair(Pair::DoubleBraceClose) };
	[ "#[" ] => { TokenCode::Pair(Pair::OuterAttr) };
	[ "#![" ] => { TokenCode::Pair(Pair::InnerAttr) };

    // pure Rust, used in macro
	[ "rs:func" ] => { TokenCode::Keyword(Keyword::RsFunc) };
	[ "rs:macro" ] => { TokenCode::Keyword(Keyword::RsMacro) };
	[ "rs:ret" ] => { TokenCode::Keyword(Keyword::RsRet) };
	[ "rs:.." ] => { TokenCode::Op(Op::RsDotDot) };
	[ "rs:..." ] => { TokenCode::Op(Op::RsDotDotDot) };
	[ "rs:[" ] => { TokenCode::Pair(Pair::RsSquareOpen) };
	[ "rs:]" ] => { TokenCode::Pair(Pair::RsSquareClose) };
}
