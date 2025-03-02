use super::ast::*;
use super::parse::*;
use std::fmt;
use std::str::FromStr;

pub struct Lex {
    pub tokens: Vec<Token>,
    pub comments: Vec<Comment>,

    line: usize,
    column: usize,
}

impl Lex {
    pub fn new() -> Lex {
        Lex {
            tokens: Vec::new(),
            comments: Vec::new(),
            line: 1,
            column: 1,
        }
    }

    fn panic(&mut self, a: &str) -> ParseResult<()> {
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

    fn add_token(&mut self, code: TokenCode) {
        let s = self;

        let width = match &code {
            TokenCode::Literal(a) => {
                let p_width = if let Some(prefix) = &a.prefix {
                    prefix.len()
                } else {
                    0
                };
                let w = match &a.payload {
                    LiteralPayload::Bool(b) | LiteralPayload::Int(b) | LiteralPayload::Float(b) => {
                        let length = b.len();
                        if p_width == 0 {
                            length
                        } else {
                            length + 1
                        }
                    }
                    LiteralPayload::String(b) => {
                        let a = 2 + b.text.len() + b.raw_count * 2;
                        if b.u_path {
                            // u-path"a" to "../a"
                            a + 3
                        } else {
                            a
                        }
                    }
                    LiteralPayload::Char(b) => 2 + b.text.len(),
                };
                let s_width = if let Some(suffix) = &a.suffix {
                    suffix.len()
                } else {
                    0
                };
                p_width + w + s_width
            }
            TokenCode::Shebang(a) => a.len(),
            TokenCode::Identifier(a) => {
                let p_width = if let Some(prefix) = &a.prefix {
                    prefix.len()
                } else {
                    0
                };
                let length = a.id.len();
                if p_width == 0 {
                    length
                } else {
                    length + 1
                }
            }
            TokenCode::Lifetime(a) => a.id.len() + 1,
            TokenCode::Keyword(a) => format!("{:?}", a).len(),
            TokenCode::Assign(a) => format!("{:?}", a).len(),
            TokenCode::Op(a) => format!("{:?}", a).len(),
            TokenCode::Pair(a) => match a {
                Pair::DoubleBraceOpen | Pair::DoubleBraceClose => 2,
                _ => 1,
            },
        };

        let span = Span {
            line: s.line,
            column: s.column,
            width,
        };

        let mut code = code;
        match &mut code {
            TokenCode::Identifier(id) => {
                id.span = span;
            }
            TokenCode::Literal(id) => {
                id.span = span;
            }
            _ => {}
        }

        let new_token = Token { code, span };
        s.tokens.push(new_token);

        s.column += width;
    }

    fn add_semicolon(&mut self) {
        let s = self;
        let new_token = Token {
            code: TokenCode::Op(Op::Semicolon),
            span: Span {
                line: s.line,
                column: std::usize::MAX,
                width: 1,
            },
        };
        s.tokens.push(new_token);
    }

    // find the last non-comment token
    fn last_non_comment_token(&mut self) -> Result<&Token, ParseError> {
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

    fn add_comment(
        &mut self,
        text: String,
        is_line: bool,
        is_doc: bool,
        inner_doc: bool,
        start_line: usize,
    ) {
        let s = self;

        // might be inaccurate, but it doesn't matter(the worst case is adding one more newline before the comment)
        // and always add a newline after the comment
        let newline_before = match s.tokens.last() {
            Some(token) => token.span.line < start_line,
            None => false,
        };

        let width = text.len();
        s.comments.push(Comment {
            text,
            is_line,
            is_doc,
            inner_doc,
            newline_before,
            span: Span {
                line: start_line,
                column: s.column,
                width,
            },
        });
    }

    // --------------------------------------------------------------------------------
    pub fn lex(&mut self, mut data: String) -> Result<(), ParseError> {
        let s = self;

        data.push('\n');
        let chars: Vec<char> = data.chars().collect();
        let total_len = chars.len();

        let mut id_prefix: Option<String> = None;
        let mut i = 0;

        let get_literal_suffix = |new_i: usize| -> (Option<String>, usize) {
            if new_i >= total_len {
                return (None, 0);
            }
            let c_first = chars[new_i];
            if !(c_first.is_alphabetic() || c_first != '-') {
                return (None, 0);
            }
            let mut end = new_i;
            for k in new_i..total_len {
                end = k;
                let c = chars[k];
                if !(c.is_alphanumeric() || c == '-') {
                    break;
                }
            }
            (Some(chars[new_i..end].iter().collect()), end)
        };

        // possible shebang #! at the begining of the file
        if total_len > 2 && chars[0] == '#' && chars[1] == '!' && chars[2] != '[' {
            while i < total_len {
                if chars[i] == '\n' {
                    let a = chars[0..i].iter().collect();
                    s.add_token(TokenCode::Shebang(a));
                    break;
                }
                i += 1;
            }
        }

        // skip newlines and whitespaces after possible shebang
        while i < total_len {
            let ch = chars[i];
            if ch == '\n' {
                s.line += 1;
                s.column = 1;
                i += 1;
                continue;
            }
            if ch.is_whitespace() {
                s.column += 1;
                i += 1;
                continue;
            }
            break;
        }

        // main loop
        'outer: while i < total_len {
            let ch = chars[i];
            let mut next_ch = '\n';
            if i + 1 < total_len {
                next_ch = chars[i + 1];
            }

            // newline
            if ch == '\n' {
                let last_token = match s.last_non_comment_token() {
                    Ok(ok) => ok,
                    Err(_) => {
                        s.line += 1;
                        s.column = 1;
                        i += 1;
                        continue;
                    }
                };
                let need_semicolon = match &last_token.code {
                    TokenCode::Identifier(_)
                    | TokenCode::Lifetime(_)
                    | TokenCode::Keyword(_)
                    | TokenCode::Literal(_) => true,
                    TokenCode::Op(op) => match op {
                        Op::PlusPlus | Op::MinusMinus | Op::Question | Op::Dot | Op::DotDot => true,
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
                if need_semicolon {
                    s.add_semicolon();
                }
                s.line += 1;
                s.column = 1;
                i += 1;
                continue;
            }

            // whitespace
            if ch.is_whitespace() {
                i += 1;
                s.column += 1;
                continue;
            }

            // ->
            if ch == '-' && next_ch == '>' {
                s.add_token(TokenCode::Op(Op::RArrow));
                i += 2;
                continue;
            }

            // identifier
            if ch.is_alphabetic() || ch == '-' {
                let start = i;
                for delta in start + 1..total_len {
                    let c = chars[delta];
                    if c.is_alphanumeric() || c == '-' {
                        continue;
                    }

                    let a: String = chars[start..delta].iter().collect();
                    if c == '"' || c == '\'' {
                        id_prefix = Some(a);
                        i = delta;
                        continue 'outer;
                    }
                    if c == '#' {
                        let ch = chars[delta + 1];

                        // id#"
                        // id##"
                        if ch == '"' || ch == '#' {
                            id_prefix = Some(a);
                            i = delta;
                            continue 'outer;
                        }

                        // id#id, id#123
                        if !(ch.is_alphanumeric() || ch == '-') {
                            return s.panic("expected identifier or digit after #");
                        }

                        id_prefix = Some(a);
                        i = delta + 1;
                        continue 'outer;
                    }

                    if id_prefix.is_some() {
                        s.add_token(TokenCode::Identifier(Identifier::new_with_prefix(
                            a,
                            &mut id_prefix,
                            s.line,
                            s.column,
                        )));
                    } else {
                        if a == "true" || a == "false" {
                            s.add_token(TokenCode::Literal(Literal::new(LiteralPayload::Bool(
                                a.to_string(),
                            ))));
                        } else {
                            let keyword = Keyword::from_str(&a);
                            if let Ok(key) = keyword {
                                s.add_token(TokenCode::Keyword(key));
                            } else {
                                s.add_token(TokenCode::Identifier(Identifier::new(
                                    a, s.line, s.column,
                                )));
                            }
                        }
                    }
                    i = delta;
                    continue 'outer;
                }
            }

            // number
            if ch.is_digit(10) {
                let start = i;
                let mut new_i = start;
                if ch == '0' {
                    match next_ch {
                        // hex
                        'x' | 'X' => {
                            for delta in start + 2..total_len {
                                let c = chars[delta];
                                if ('0'..='9').contains(&c)
                                    || ('a'..='f').contains(&c)
                                    || ('A'..='F').contains(&c)
                                    || c == '_'
                                {
                                    continue;
                                }
                                new_i = delta;
                                break;
                            }
                        }
                        // octal
                        'o' | 'O' => {
                            for delta in start + 2..total_len {
                                let c = chars[delta];
                                if ('0'..='7').contains(&c) || c == '_' {
                                    continue;
                                }
                                new_i = delta;
                                break;
                            }
                        }
                        // binary
                        'b' | 'B' => {
                            for delta in start + 2..total_len {
                                let c = chars[delta];
                                if c == '0' || c == '1' || c == '_' {
                                    continue;
                                }
                                new_i = delta;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                // add number
                if new_i - start == 2 {
                    return s.panic("expected digit");
                }
                if new_i > start {
                    // possible suffix
                    let (suffix, end) = get_literal_suffix(new_i);
                    let a = chars[start..new_i].iter().collect();
                    s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                        LiteralPayload::Int(a),
                        &mut id_prefix,
                        suffix,
                    )));
                    i = end;
                    continue;
                }

                /* decimal
                DEC_LITERAL:
                   DEC_DIGIT (DEC_DIGIT|_)*

                FLOAT_LITERAL:
                    DEC_LITERAL (. DEC_LITERAL)? FLOAT_EXPONENT? FLOAT_SUFFIX?
                FLOAT_EXPONENT :
                   (e|E) (+|~)? (DEC_DIGIT|_)* DEC_DIGIT (DEC_DIGIT|_)*
                */

                let mut is_float = false;

                // skip DEC_LITERAL
                for delta in new_i..total_len {
                    let c = chars[delta];
                    if ('0'..='9').contains(&c) || c == '_' {
                        continue;
                    }
                    new_i = delta;
                    break;
                }
                // possible .
                if chars[new_i] == '.' {
                    new_i += 1;
                    // skip DEC_LITERAL
                    let new_i_start = new_i;
                    for delta in new_i..total_len {
                        let c = chars[delta];
                        if ('0'..='9').contains(&c) || c == '_' {
                            continue;
                        }
                        new_i = delta;
                        break;
                    }
                    if new_i == new_i_start {
                        let a = chars[start..(new_i - 1)].iter().collect();
                        s.add_token(TokenCode::Literal(Literal {
                            payload: LiteralPayload::Int(a),
                            prefix: None,
                            suffix: None,
                            span: Span::default(),
                        }));
                        s.add_token(TokenCode::Op(Op::Dot));
                        i = new_i;
                        continue 'outer;
                    }
                    is_float = true;
                }
                // possible e | E
                if chars[new_i] == 'e' || chars[new_i] == 'E' {
                    is_float = true;
                    new_i += 1;
                    // possible + | ~
                    if chars[new_i] == '+' || chars[new_i] == '~' {
                        new_i += 1;
                    }
                    // possible _
                    if chars[new_i] == '_' {
                        new_i += 1;
                    }
                    // digit
                    if !(('0'..='9').contains(&chars[new_i]) || chars[new_i] == '_') {
                        return s.panic("expected digit");
                    }
                    new_i += 1;
                    // skip DEC_LITERAL
                    for delta in new_i..total_len {
                        let c = chars[delta];
                        if ('0'..='9').contains(&c) || c == '_' {
                            continue;
                        }
                        new_i = delta;
                        break;
                    }
                }
                // possible suffix
                let (suffix, end) = get_literal_suffix(new_i);
                let a = chars[start..new_i].iter().collect();
                if is_float {
                    s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                        LiteralPayload::Float(a),
                        &mut id_prefix,
                        suffix,
                    )));
                } else {
                    s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                        LiteralPayload::Int(a),
                        &mut id_prefix,
                        suffix,
                    )));
                }
                i = end;
                continue;
            }

            match ch {
                '/' => match next_ch {
                    // //
                    '/' => {
                        let mut start = i + 2;
                        let (is_doc, inner_doc) = if chars[start] == '/' && chars[start + 1] != '/'
                        {
                            start += 1;
                            (true, false)
                        } else if chars[start] == '!' {
                            start += 1;
                            (true, true)
                        } else {
                            (false, false)
                        };
                        for delta in start..total_len {
                            if chars[delta] == '\n' {
                                i = delta;
                                let a = chars[start..delta].iter().collect();
                                s.add_comment(a, true, is_doc, inner_doc, s.line);
                                continue 'outer;
                            }
                        }
                        return s.panic("expected newline after //");
                    }
                    // /* */
                    '*' => {
                        let start_line = s.line;
                        s.column += 2;
                        let mut start = i + 2;
                        let (is_doc, inner_doc) = if chars[start] == '*' && chars[start + 1] != '/'
                        {
                            start += 1;
                            (true, false)
                        } else if chars[start] == '!' {
                            start += 1;
                            (true, true)
                        } else {
                            (false, false)
                        };

                        let mut comment_count = 1;
                        for delta in start..total_len {
                            let c = chars[delta];
                            let mut next_c = ' ';
                            if delta + 1 < total_len {
                                next_c = chars[delta + 1];
                            }
                            if c == '\n' {
                                s.line += 1;
                                s.column = 0;
                            }
                            if c == '/' && next_c == '*' {
                                comment_count += 1;
                                continue;
                            }
                            if c == '*' && next_c == '/' {
                                comment_count -= 1;
                                if comment_count == 0 {
                                    let a = chars[start..delta].iter().collect();
                                    s.add_comment(a, false, is_doc, inner_doc, start_line);
                                    i = delta + 2;
                                    s.column += 2;
                                    continue 'outer;
                                }
                            }
                            s.column += 1;
                        }
                        return s.panic("expected */ after /*");
                    }
                    // /=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::Div));
                        i += 1;
                    }
                    // /
                    _ => {
                        s.add_token(TokenCode::Op(Op::Div));
                    }
                },

                // string
                '"' => {
                    let start_line = s.line;
                    let start = i + 1;
                    let mut skip = false;
                    for delta in start..total_len {
                        if skip {
                            skip = false;
                            continue;
                        }
                        let c = chars[delta];
                        if c == '\n' {
                            s.line += 1;
                        }
                        if c == '\\' {
                            skip = true;
                            continue;
                        }
                        if c == '"' {
                            let new_line = s.line;
                            s.line = start_line;
                            let (suffix, end) = get_literal_suffix(delta + 1);
                            let a = chars[start..delta].iter().collect();
                            let lit_str = Literal::new_string_with_prefix(
                                LiteralString {
                                    raw_count: 0,
                                    text: a,
                                    u_tokens: Vec::new(),
                                    u_path: false,
                                },
                                &mut id_prefix,
                                suffix,
                            );
                            let lit_str = match lit_str {
                                Ok(a) => a,
                                Err(err) => {
                                    return s.panic(err);
                                }
                            };
                            s.add_token(TokenCode::Literal(lit_str));
                            i = end;
                            s.line = new_line;
                            continue 'outer;
                        }
                    }
                    return s.panic("expected closing \"");
                }

                '\'' => {
                    if next_ch == '\'' {
                        return s.panic("expected char in ''");
                    }

                    // lifetime
                    if next_ch.is_alphabetic() || next_ch == '-' {
                        let is_lifetime = if i + 2 < total_len {
                            chars[i + 2] != '\''
                        } else {
                            true
                        };
                        if is_lifetime {
                            let start = i + 1;
                            let mut end = start;
                            for k in start..total_len {
                                let c = chars[k];
                                if !(c.is_alphanumeric() || c == '-') {
                                    break;
                                }
                                end += 1;
                            }
                            let a: String = chars[start..end].iter().collect();
                            s.add_token(TokenCode::Lifetime(Identifier::new(
                                a,
                                s.line,
                                s.column + 1,
                            )));
                            i = end;
                            continue;
                        }
                    }

                    // char
                    let start = i + 1;
                    let mut skip = false;
                    for delta in start..total_len {
                        if skip {
                            skip = false;
                            continue;
                        }
                        let c = chars[delta];
                        if c == '\\' {
                            skip = true;
                            continue;
                        }
                        if c == '\'' {
                            let (suffix, end) = get_literal_suffix(delta + 1);
                            let a: String = chars[start..delta].iter().collect();
                            s.add_token(TokenCode::Literal(Literal::new_with_prefix(
                                LiteralPayload::Char(LiteralChar { text: a }),
                                &mut id_prefix,
                                suffix,
                            )));
                            i = end;
                            continue 'outer;
                        }
                    }
                }

                '+' => match next_ch {
                    // ++
                    '+' => {
                        s.add_token(TokenCode::Op(Op::PlusPlus));
                        i += 1;
                    }
                    // +=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::Add));
                        i += 1;
                    }
                    // +
                    _ => {
                        s.add_token(TokenCode::Op(Op::Add));
                    }
                },

                '~' => match next_ch {
                    // ~~
                    '~' => {
                        s.add_token(TokenCode::Op(Op::MinusMinus));
                        i += 1;
                    }
                    // ~=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::Sub));
                        i += 1;
                    }
                    // ~
                    _ => {
                        s.add_token(TokenCode::Op(Op::Sub));
                    }
                },

                '*' => match next_ch {
                    // *=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::Star));
                        i += 1;
                    }
                    // *
                    _ => {
                        s.add_token(TokenCode::Op(Op::Star));
                    }
                },

                '%' => match next_ch {
                    // %=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::Rem));
                        i += 1;
                    }
                    // %
                    _ => {
                        s.add_token(TokenCode::Op(Op::Rem));
                    }
                },

                '!' => match next_ch {
                    // !=
                    '=' => {
                        s.add_token(TokenCode::Op(Op::Neq));
                        i += 1;
                    }
                    // !
                    _ => {
                        s.add_token(TokenCode::Op(Op::Not));
                    }
                },

                '^' => match next_ch {
                    // ^=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::BitXor));
                        i += 1;
                    }
                    // ^
                    _ => {
                        s.add_token(TokenCode::Op(Op::BitXor));
                    }
                },

                '|' => match next_ch {
                    // ||
                    '|' => {
                        s.add_token(TokenCode::Op(Op::Or));
                        i += 1;
                    }
                    // |=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::BitOr));
                        i += 1;
                    }
                    // |
                    _ => {
                        s.add_token(TokenCode::Op(Op::BitOr));
                    }
                },

                '=' => match next_ch {
                    // ==
                    '=' => {
                        s.add_token(TokenCode::Op(Op::Eq));
                        i += 1;
                    }
                    // =>
                    '>' => {
                        s.add_token(TokenCode::Op(Op::FatArrow));
                        i += 1;
                    }
                    // =
                    _ => {
                        s.add_token(TokenCode::Assign(Assign::Assign));
                    }
                },

                ':' => match next_ch {
                    // :=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::Declare));
                        i += 1;
                    }
                    // ::
                    ':' => {
                        s.add_token(TokenCode::Op(Op::ColonColon));
                        i += 1;
                    }
                    // :
                    _ => {
                        s.add_token(TokenCode::Op(Op::Colon));
                    }
                },

                ',' => match next_ch {
                    // ,,
                    ',' => {
                        i += 1;
                        s.add_token(TokenCode::Op(Op::CommaComma));
                    }
                    // ,
                    _ => {
                        s.add_token(TokenCode::Op(Op::Comma));
                    }
                },

                '`' => match next_ch {
                    '`' => {
                        if i + 2 >= total_len {
                            // ``
                            s.add_token(TokenCode::Op(Op::TickTick));
                            i += 1;
                        } else {
                            match chars[i + 2] {
                                '=' => {
                                    // ``=
                                    s.add_token(TokenCode::Op(Op::TickTickEq));
                                    i += 2;
                                }
                                _ => {
                                    // ``
                                    s.add_token(TokenCode::Op(Op::TickTick));
                                    i += 1;
                                }
                            }
                        }
                    }
                    _ => {
                        return s.panic("expected ` after `");
                    }
                },

                '.' => match next_ch {
                    '.' => {
                        if i + 2 >= total_len {
                            // ..
                            s.add_token(TokenCode::Op(Op::DotDot));
                            i += 1;
                        } else {
                            match chars[i + 2] {
                                '.' => {
                                    // ...
                                    s.add_token(TokenCode::Op(Op::DotDotDot));
                                    i += 2;
                                }
                                '=' => {
                                    // ..=
                                    s.add_token(TokenCode::Op(Op::DotDotEq));
                                    i += 2;
                                }
                                _ => {
                                    // ..
                                    s.add_token(TokenCode::Op(Op::DotDot));
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
                        let last_token = s.last_non_comment_token()?;
                        if last_token.is_added_semicolon() {
                            s.tokens.pop();
                        }
                        s.add_token(TokenCode::Op(Op::Dot));
                    }
                },

                '<' => match next_ch {
                    // <=
                    '=' => {
                        s.add_token(TokenCode::Op(Op::LessEq));
                        i += 1;
                    }
                    '<' => {
                        if i + 2 < total_len && chars[i + 2] == '=' {
                            // <<=
                            s.add_token(TokenCode::Assign(Assign::LeftShift));
                            i += 2;
                        } else {
                            // <<
                            s.add_token(TokenCode::Op(Op::LeftShift));
                            i += 1;
                        }
                    }
                    // <
                    _ => {
                        s.add_token(TokenCode::Op(Op::Less));
                    }
                },

                '>' => match next_ch {
                    // >=
                    '=' => {
                        s.add_token(TokenCode::Op(Op::GreaterEq));
                        i += 1;
                    }
                    '>' => {
                        if i + 2 < total_len && chars[i + 2] == '=' {
                            // >>=
                            s.add_token(TokenCode::Assign(Assign::RightShift));
                            i += 2;
                        } else {
                            // >>
                            s.add_token(TokenCode::Op(Op::RightShift));
                            i += 1;
                        }
                    }
                    // >
                    _ => {
                        s.add_token(TokenCode::Op(Op::Greater));
                    }
                },

                '&' => match next_ch {
                    // &&
                    '&' => {
                        s.add_token(TokenCode::Op(Op::And));
                        i += 1;
                    }
                    // &=
                    '=' => {
                        s.add_token(TokenCode::Assign(Assign::BitAnd));
                        i += 1;
                    }
                    // &
                    _ => {
                        s.add_token(TokenCode::Op(Op::BitAnd));
                    }
                },

                '{' => match next_ch {
                    // {{
                    '{' => {
                        s.add_token(TokenCode::Pair(Pair::DoubleBraceOpen));
                        i += 1;
                    }
                    // {
                    _ => {
                        s.add_token(TokenCode::Pair(Pair::BraceOpen));
                    }
                },
                '}' => match next_ch {
                    // }}
                    '}' => {
                        s.add_token(TokenCode::Pair(Pair::DoubleBraceClose));
                        i += 1;
                    }
                    // }
                    _ => {
                        s.add_token(TokenCode::Pair(Pair::BraceClose));
                    }
                },

                '#' => match next_ch {
                    // #[
                    '[' => {
                        s.add_token(TokenCode::Op(Op::OuterAttr));
                        i += 1;
                    }
                    // #![
                    '!' => {
                        if i + 2 >= total_len || chars[i + 2] != '[' {
                            return s.panic("expected [ after #!");
                        }
                        s.add_token(TokenCode::Op(Op::InnerAttr));
                        i += 2;
                    }
                    // #" "#
                    // ##" "##
                    // ###" "###
                    '"' | '#' => {
                        let mut raw_count = 0;
                        for delta in i..total_len {
                            let c = chars[delta];
                            if c == '#' {
                                raw_count += 1;
                                continue;
                            }
                            if raw_count > 0 && c != '"' {
                                return s.panic("expected \" after #");
                            }
                            break;
                        }

                        let start_line = s.line;
                        let start = i + raw_count + 1;
                        for delta in start..total_len {
                            let c = chars[delta];
                            if c == '\n' {
                                s.line += 1;
                                continue;
                            }
                            // find "###
                            if c == '"' {
                                let mut count = 0;
                                for j in delta + 1..total_len {
                                    let c = chars[j];
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

                                let new_line = s.line;
                                s.line = start_line;
                                let (suffix, end) = get_literal_suffix(delta + raw_count + 1);
                                let a = chars[start..delta].iter().collect();
                                let lit_str = Literal::new_string_with_prefix(
                                    LiteralString {
                                        raw_count,
                                        text: a,
                                        u_tokens: Vec::new(),
                                        u_path: false,
                                    },
                                    &mut id_prefix,
                                    suffix,
                                );
                                let lit_str = match lit_str {
                                    Ok(a) => a,
                                    Err(err) => {
                                        return s.panic(err);
                                    }
                                };
                                s.add_token(TokenCode::Literal(lit_str));
                                i = end;
                                s.line = new_line;
                                continue 'outer;
                            }
                        }
                        return s.panic("expected closing \"#");
                    }
                    _ => s.add_token(TokenCode::Op(Op::Pound)),
                },

                '(' => s.add_token(TokenCode::Pair(Pair::ParenOpen)),
                ')' => s.add_token(TokenCode::Pair(Pair::ParenClose)),
                '[' => s.add_token(TokenCode::Pair(Pair::SquareOpen)),
                ']' => s.add_token(TokenCode::Pair(Pair::SquareClose)),
                ';' => s.add_token(TokenCode::Op(Op::Semicolon)),
                '?' => s.add_token(TokenCode::Op(Op::Question)),
                '@' => s.add_token(TokenCode::Op(Op::At)),
                '$' => s.add_token(TokenCode::Op(Op::Dollar)),

                _ => return s.panic(&format!("illegal char: {}", ch)),
            }

            i += 1;
        }

        // make sure the last token is always semicolon to help parse
        let mut i = total_len - 1;
        while i > 0 {
            let ch = chars[i];
            if ch == '\n' {
                if s.line > 0 {
                    s.line -= 1;
                }
                i -= 1;
            } else {
                break;
            }
        }
        s.add_semicolon();

        // println!("{:?}", s);
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
#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct Token {
    pub code: TokenCode,
    pub span: Span,
}

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub width: usize,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self;
        write!(f, "{:?}", s.code)
        // write!(f, "{:?}{}", s.code, s.column)
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
}
impl ToLang for Token {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        match &s.code {
            TokenCode::Shebang(a) => p.push_str(a, s.span),
            TokenCode::Literal(literal) => p.push_rust(literal),
            TokenCode::Identifier(identifier) => p.push_rust(identifier),
            TokenCode::Lifetime(identifier) => {
                p.push_str(
                    "'",
                    Span {
                        line: s.span.line,
                        column: s.span.column - 1,
                        width: 1,
                    },
                );
                p.push_rust(identifier);
            }
            TokenCode::Keyword(keyword) => {
                let a = match keyword {
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
                    Keyword::Import => "use",
                    Keyword::In => "in",
                    Keyword::Let => "let",
                    Keyword::Interface => "trait",
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
                };
                p.push_str(a, s.span);
            }
            TokenCode::Assign(assign) => {
                let a = match assign {
                    Assign::Assign => "=",
                    Assign::Declare => ":=",
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
                };
                p.push_str(a, s.span);
            }
            TokenCode::Op(op) => {
                let a = match op {
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
                    Op::DotDotDot => "...",
                    Op::Tick => "`",
                    Op::TickTick => "..",
                    Op::TickTickEq => "..=",
                    Op::Pound => "#",
                    Op::OuterAttr => "#[",
                    Op::InnerAttr => "#![",
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
                };
                p.push_str(a, s.span);
            }
            TokenCode::Pair(pair) => {
                let a = match pair {
                    Pair::ParenOpen => "(",
                    Pair::ParenClose => ")",
                    Pair::SquareOpen => "<",
                    Pair::SquareClose => ">",
                    Pair::BraceOpen => "{",
                    Pair::BraceClose => "}",
                    Pair::DoubleBraceOpen => "[",
                    Pair::DoubleBraceClose => "]",
                };
                p.push_str(a, s.span);
            }
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match &s.code {
            TokenCode::Shebang(a) => p.push_raw(a),
            TokenCode::Literal(literal) => p.push_u(literal),
            TokenCode::Identifier(identifier) => p.push_u(identifier),
            TokenCode::Lifetime(identifier) => {
                p.push_raw("'");
                p.push_u(identifier);
            }
            TokenCode::Keyword(keyword) => match keyword {
                Keyword::Async => p.push_raw("async"),
                Keyword::Await => p.push_raw("await"),
                Keyword::Break => p.push_raw("break"),
                Keyword::Const => p.push_raw("const"),
                Keyword::Continue => p.push_raw("continue"),
                Keyword::Crate => p.push_raw("crate"),
                Keyword::Dyn => p.push_raw("dyn"),
                Keyword::Else => p.push_raw("else"),
                Keyword::Enum => p.push_raw("enum"),
                Keyword::Extern => p.push_raw("extern"),
                Keyword::For => p.push_raw("for"),
                Keyword::Func => p.push_raw("func"),
                Keyword::If => p.push_raw("if"),
                Keyword::Impl => p.push_raw("impl"),
                Keyword::Import => p.push_raw("import"),
                Keyword::In => p.push_raw("in"),
                Keyword::Let => p.push_raw("let"),
                Keyword::Interface => p.push_raw("interface"),
                Keyword::Macro => p.push_raw("macro"),
                Keyword::Mod => p.push_raw("mod"),
                Keyword::Mut => p.push_raw("mut"),
                Keyword::Ref => p.push_raw("ref"),
                Keyword::Ret => p.push_raw("ret"),
                Keyword::SelfType => p.push_raw("Self"),
                Keyword::Static => p.push_raw("static"),
                Keyword::Struct => p.push_raw("struct"),
                Keyword::Super => p.push_raw("super"),
                Keyword::Match => p.push_raw("match"),
                Keyword::Type => p.push_raw("type"),
                Keyword::Unsafe => p.push_raw("unsafe"),
                Keyword::Where => p.push_raw("where"),

                Keyword::As => p.push_raw("as"),
                Keyword::Fn => p.push_raw("fn"),
                Keyword::Loop => p.push_raw("loop"),
                Keyword::Move => p.push_raw("move"),
                Keyword::Pub => p.push_raw("pub"),
                Keyword::Return => p.push_raw("return"),
                Keyword::SelfValue => p.push_raw("self"),
                Keyword::Trait => p.push_raw("trait"),
                Keyword::Use => p.push_raw("use"),
                Keyword::While => p.push_raw("while"),

                Keyword::Abstract => p.push_raw("abstract"),
                Keyword::Become => p.push_raw("become"),
                Keyword::Box => p.push_raw("box"),
                Keyword::Do => p.push_raw("do"),
                Keyword::Final => p.push_raw("final"),
                Keyword::Override => p.push_raw("override"),
                Keyword::Priv => p.push_raw("priv"),
                Keyword::Typeof => p.push_raw("typeof"),
                Keyword::Union => p.push_raw("union"),
                Keyword::Unsized => p.push_raw("unsized"),
                Keyword::Virtual => p.push_raw("virtual"),
                Keyword::Yield => p.push_raw("yield"),
                Keyword::Try => p.push_raw("try"),
                Keyword::Gen => p.push_raw("gen"),
            },
            TokenCode::Assign(assign) => match assign {
                Assign::Assign => p.push_raw("="),
                Assign::Declare => p.push_raw(":="),
                Assign::Add => p.push_raw("+="),
                Assign::Sub => p.push_raw("~="),
                Assign::Star => p.push_raw("*="),
                Assign::Div => p.push_raw("/="),
                Assign::Rem => p.push_raw("%="),
                Assign::LeftShift => p.push_raw("<<="),
                Assign::RightShift => p.push_raw(">>="),
                Assign::BitAnd => p.push_raw("&="),
                Assign::BitOr => p.push_raw("|="),
                Assign::BitXor => p.push_raw("^="),
            },
            TokenCode::Op(op) => match op {
                Op::Add => p.push_raw("+"),
                Op::Sub => p.push_raw("~"),
                Op::Star => p.push_raw("*"),
                Op::Div => p.push_raw("/"),
                Op::Rem => p.push_raw("%"),
                Op::PlusPlus => p.push_raw("++"),
                Op::MinusMinus => p.push_raw("~~"),
                Op::Eq => p.push_raw("=="),
                Op::Neq => p.push_raw("!="),
                Op::Less => p.push_raw("<"),
                Op::LessEq => p.push_raw("<="),
                Op::Greater => p.push_raw(">"),
                Op::GreaterEq => p.push_raw(">="),
                Op::LeftShift => p.push_raw("<<"),
                Op::RightShift => p.push_raw(">>"),
                Op::Not => p.push_raw("!"),
                Op::And => p.push_raw("&&"),
                Op::Or => p.push_raw("||"),
                Op::BitAnd => p.push_raw("+"),
                Op::BitOr => p.push_raw("|"),
                Op::BitXor => p.push_raw("^"),
                Op::Dot => p.push_raw("."),
                Op::DotDot => p.push_raw(".."),
                Op::DotDotDot => p.push_raw("..."),
                Op::Tick => p.push_raw("`"),
                Op::TickTick => p.push_raw("``"),
                Op::TickTickEq => p.push_raw("``="),
                Op::Pound => p.push_raw("#"),
                Op::OuterAttr => p.push_raw("#["),
                Op::InnerAttr => p.push_raw("#!["),
                Op::Question => p.push_raw("?"),
                Op::At => p.push_raw("@"),
                Op::Comma => p.push_raw(", "),
                Op::CommaComma => p.push_raw("!"),
                Op::Colon => p.push_raw(":"),
                Op::Semicolon => p.push_raw(";"),

                Op::ColonColon => p.push_raw("::"),
                Op::DotDotEq => p.push_raw("..="),
                Op::RArrow => p.push_raw("->"),
                Op::FatArrow => p.push_raw("=>"),
                Op::Dollar => p.push_raw("$"),
            },
            TokenCode::Pair(pair) => match pair {
                Pair::ParenOpen => p.push_raw("("),
                Pair::ParenClose => p.push_raw(")"),
                Pair::SquareOpen => p.push_raw("["),
                Pair::SquareClose => p.push_raw("]"),
                Pair::BraceOpen => p.push_raw("{"),
                Pair::BraceClose => p.push_raw("}"),
                Pair::DoubleBraceOpen => p.push_raw("{{"),
                Pair::DoubleBraceClose => p.push_raw("}}"),
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TokenCode {
    Shebang(String),
    Literal(Literal),
    Identifier(Identifier),
    Lifetime(Identifier),
    Keyword(Keyword),
    Assign(Assign),
    Op(Op),
    Pair(Pair),
}

impl fmt::Debug for TokenCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Shebang(a) => write!(f, "{}", a),
            Self::Literal(a) => write!(f, "{:?}", a),
            Self::Identifier(a) => write!(f, "{:?}", a),
            Self::Lifetime(a) => write!(f, "'{:?}", a),
            Self::Keyword(a) => write!(f, "{:?}", a),
            Self::Assign(a) => write!(f, "{:?}", a),
            Self::Op(a) => write!(f, "{:?}", a),
            Self::Pair(a) => write!(f, "{:?}", a),
        }
    }
}

impl Default for TokenCode {
    fn default() -> Self {
        TokenCode::Op(Op::Eq)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralString {
    raw_count: usize, // number of pound sign
    pub text: String,
    u_tokens: Vec<Token>,
    u_path: bool,
}
impl LiteralString {
    fn to_rust_string(&self) -> String {
        let s = self;

        let mut p = String::new();

        if s.raw_count > 0 {
            p.push('r');
            for _ in 0..s.raw_count {
                p.push('#');
            }
        }
        p.push('"');

        if s.u_tokens.is_empty() {
            p.push_str(&s.text);
        } else {
            let mut fmtter = LangFormatter::new(true, Vec::new());
            for token in &s.u_tokens {
                fmtter.push_rust(token);
            }
            p.push_str(&fmtter.buf())
        }

        p.push('"');
        for _ in 0..s.raw_count {
            p.push('#');
        }

        p
    }

    pub fn push_span(&self, p: &mut LangFormatter, span: Span) {
        let s = self;
        let a = s.to_rust_string();
        p.push_str(&a, span);
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
impl ToLang for LiteralString {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw(&s.to_rust_string());
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        for _ in 0..s.raw_count {
            p.push_raw("#");
        }
        p.push_raw("\"");
        p.push_raw(&s.text);
        p.push_raw("\"");
        for _ in 0..s.raw_count {
            p.push_raw("#");
        }
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
impl ToLang for LiteralChar {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw(&s.to_rust_string());
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        p.push_raw("'");
        p.push_raw(&s.text);
        p.push_raw("'");
    }
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
        prefix: &mut Option<String>,
        suffix: Option<String>,
    ) -> Literal {
        let p = prefix.clone();
        *prefix = None;
        Literal {
            payload,
            prefix: p,
            suffix,
            span: Span::default(),
        }
    }

    fn new_string_with_prefix(
        lit_str: LiteralString,
        prefix: &mut Option<String>,
        suffix: Option<String>,
    ) -> Result<Literal, &'static str> {
        let p = prefix.clone();
        *prefix = None;

        let prefix = p.as_ref().map(String::as_str);
        let lit_str = match prefix {
            Some("u") => {
                let mut lit_str = lit_str;

                let mut l = Lex::new();
                if l.lex(lit_str.text.clone()).is_err() {
                    return Err("illegal u string");
                }
                let tokens: Vec<Token> = l
                    .tokens
                    .into_iter()
                    .filter(|token| !token.is_added_semicolon())
                    .collect();
                lit_str.u_tokens = tokens;
                lit_str
            }
            Some("u-path") => {
                let mut lit_str = lit_str;
                lit_str.text = format!("../{}", lit_str.text);
                lit_str.u_path = true;
                return Ok(Literal {
                    payload: LiteralPayload::String(lit_str),
                    prefix: None,
                    suffix,
                    span: Span::default(),
                });
            }
            _ => lit_str,
        };

        Ok(Literal {
            payload: LiteralPayload::String(lit_str),
            prefix: p,
            suffix,
            span: Span::default(),
        })
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
impl ToLang for Literal {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        let mut push_str = |a, is_num| {
            let mut r = String::new();
            if let Some(prefix) = &s.prefix {
                let prefix = Identifier::id_to_rust(&prefix);
                r.push_str(&prefix);
                if is_num {
                    r.push('#');
                }
            }
            r.push_str(a);
            if let Some(suffix) = &s.suffix {
                // NOTE builtin literal suffix for String
                if suffix == "s" {
                    r.push_str(".to_owned()");
                } else {
                    let suffix = Identifier::id_to_rust(&suffix);
                    r.push_str(&suffix);
                }
            }
            p.push_str(&r, s.span);
        };

        match &s.payload {
            LiteralPayload::String(a) => {
                if a.u_tokens.is_empty() {
                    // NOTE don't output prefix and suffix for u string
                    push_str(&a.to_rust_string(), false);
                } else {
                    p.push_str(&a.to_rust_string(), s.span)
                }

                let mut n = 0;
                let mut column = 1;
                for c in a.text.chars() {
                    if c == '\n' {
                        n += 1;
                        column = 1;
                    } else {
                        column += 1;
                    }
                }
                if n > 0 {
                    column += a.raw_count;
                    p.push_str(
                        "",
                        Span {
                            line: s.span.line + n,
                            column,
                            width: column,
                        },
                    );
                }
            }
            LiteralPayload::Char(a) => push_str(&a.to_rust_string(), false),
            LiteralPayload::Int(a) => push_str(a, true),
            LiteralPayload::Float(a) => push_str(&a.replace("~", "-"), true),

            // bool literal has neither prefix nor suffix
            LiteralPayload::Bool(a) => p.push_str(a, s.span),
        }
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;

        match &s.payload {
            LiteralPayload::String(a) => p.push_u(a),
            LiteralPayload::Char(a) => p.push_u(a),
            LiteralPayload::Int(a) => p.push_raw(a),
            LiteralPayload::Float(a) => p.push_raw(a),
            LiteralPayload::Bool(a) => p.push_raw(a),
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub prefix: Option<String>,
    pub id: String,
    pub span: Span, // same as Token's span, added here for ease of use
}
impl Identifier {
    pub fn new(id: String, line: usize, column: usize) -> Identifier {
        let width = id.len();
        Identifier {
            prefix: None,
            id,
            span: Span {
                line,
                column,
                width,
            },
        }
    }
    pub fn new_with_prefix(
        id: String,
        prefix: &mut Option<String>,
        line: usize,
        column: usize,
    ) -> Identifier {
        let mut width = id.len();
        if let Some(p) = prefix {
            width += p.len();
        }
        let p = prefix.clone();
        *prefix = None;
        Identifier {
            prefix: p,
            id,
            span: Span {
                line,
                column,
                width,
            },
        }
    }

    pub fn id_to_rust(id: &str) -> String {
        let mut p = String::new();
        if id.is_empty() {
            return "".to_string();
        }
        if id == "string" {
            return "String".to_string();
        }

        let chars: Vec<char> = id.chars().collect();
        let len = chars.len();

        if len > 2 {
            if chars[len - 3] == '-' && chars[len - 2] == '-' {
                match chars[len - 1] {
                    'c' | 'g' => {
                        if len == 3 {
                            p.push_str("__");
                            if chars[len - 1] == 'c' {
                                p.push('c');
                            } else {
                                p.push('g');
                            }
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
impl ToLang for Identifier {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        let mut r = String::new();
        if let Some(prefix) = &s.prefix {
            r.push_str(&Identifier::id_to_rust(prefix));
            r.push('#');
        }
        r.push_str(&Identifier::id_to_rust(&s.id));
        p.push_str(&r, s.span);
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;
        p.push_raw(&s.id);
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
    Import,
    In,
    Let,
    Interface,
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
            Self::Import => write!(f, "import"),
            Self::In => write!(f, "in"),
            Self::Let => write!(f, "let"),
            Self::Interface => write!(f, "interface"),
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
            "import" => Ok(Self::Import),
            "in" => Ok(Self::In),
            "let" => Ok(Self::Let),
            "interface" => Ok(Self::Interface),
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
    OuterAttr,
    InnerAttr,
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
            Self::OuterAttr => write!(f, "#["),
            Self::InnerAttr => write!(f, "#!["),
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
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Comment {
    pub text: String,
    pub is_line: bool,
    pub is_doc: bool,
    pub inner_doc: bool,

    pub newline_before: bool,

    pub span: Span,
}
impl ToLang for Comment {
    fn to_rust(&self, p: &mut LangFormatter) {
        let s = self;

        if s.newline_before {
            p.push_raw("\n");
            p.indent();
        }

        if s.is_line {
            if s.is_doc {
                if s.inner_doc {
                    p.push_raw("//!")
                } else {
                    p.push_raw("///")
                }
            } else {
                p.push_raw("//")
            }
        } else {
            if s.is_doc {
                if s.inner_doc {
                    p.push_raw("/*!")
                } else {
                    p.push_raw("/**")
                }
            } else {
                p.push_raw("/*")
            }
        }

        if s.is_doc {
            // turn ```\n sth\n``` to ```u\n sth\n```
            let list: Vec<_> = s.text.split("```").collect();
            let mut text = String::new();
            for (i, item) in list.iter().enumerate() {
                if i % 2 == 1 {
                    text.push_str("```u");
                    text.push_str(item);
                    continue;
                }
                if i != 0 {
                    text.push_str("```");
                }

                let subs: Vec<_> = item.split('`').collect();
                for (j, token) in subs.iter().enumerate() {
                    if j % 2 == 1 {
                        text.push('`');
                        text.push_str(&Identifier::id_to_rust(token));
                        continue;
                    }
                    if j != 0 {
                        text.push('`');
                    }
                    text.push_str(token);
                }
            }
            p.push_raw(&text);
        } else {
            p.push_raw(&s.text);
        }

        if !s.is_line {
            p.push_raw("*/");
        }
        p.push_raw("\n");
        p.indent();
    }
    fn to_u(&self, p: &mut LangFormatter) {
        let s = self;
        p.push_raw(&s.text)
    }
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
    [ import ] => { TokenCode::Keyword(Keyword::Import) };
    [ in ] => { TokenCode::Keyword(Keyword::In) };
    [ let ] => { TokenCode::Keyword(Keyword::Let) };
    [ interface ] => { TokenCode::Keyword(Keyword::Interface) };
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
    [ "#[" ] => { TokenCode::Op(Op::OuterAttr) };
    [ "#![" ] => { TokenCode::Op(Op::InnerAttr) };
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
}
