use std::collections::VecDeque;

use crate::{
    ast::{AstPool, ErrorInfo, ExprId, ProgramBuilder},
    token::{self, DelimKind, Ident, Location, Span, Token, TokenEx, WsToken},
};

pub struct Parser<'a, 'b> {
    pub(crate) pool: &'a mut AstPool,
    state: LexState<'b>,
    indent_level: u32,
    prev: Option<TokenEx>,
    toks: VecDeque<TokenEx>,
    errors: Vec<ErrorInfo>,
}

struct LexState<'a> {
    input: &'a str,
    kind: LexStateKind,
    chr: Option<char>,
    loc: Location,
    ws: WsToken,
    indent_stack: Vec<u32>,
}

/*
                                         *

                                         |
                                         |
                                         v
                           newline     +-------------------------+
  +----------------------------------> |                         |
  |                                    |       CheckIndent       |
  |                                    |                         |  newline
  |                         +--------- |                         | <-------+
  |                         |          +-------------------------+         |
  |                         |            |             |       ^           |
  |                         |            | indent      |       |           |
  |                         |            v             |       |           |
  |                         |          +------------+  |       |           |
  |                         |          | PushIndent |  |       |           |
  |                         |          +------------+  |       |           |
  |                         |            |             |       |           |
  |                         |            |             |       |           |
  |                         |            v             |       |           |
  |                         |          +------------+  |       |           |
  |                         |          |    Lex     | <+       |           |
  |                         |          +------------+          |           |
  |                         |            |                     |           |
  |                         |            | lex()               |           |
  |                         |            v                     |           |
+-----------+   dedent      |          +-------------------------+         |
|           | ---------+    |          |                         | --------+
| PopIndent |          |    |          |       CheckDedent       |
|           | <--------+    |          |                         |
+-----------+               |          +-------------------------+
  ^                         |            |                     |
  | dedent                  |            |                     |
  |                         |            v                     |
  |                         |          +------------+          |
  |                         +--------> | HandleEof  | -+       |
  |                                    +------------+  |       |
  |                                      |             |       |
  |                                      | dedent      |       |
  |                                      v             |       |
  |                           dedent   +------------+  |       |
  |                         +--------- |            |  |       |
  |                         |          | Terminate  |  | eof   |
  |                         +--------> |            |  |       |
  |                                    +------------+  |       |
  |                                      |             |       |
  |                                      | eof         |       |
  |                                      v             |       |
  |                                    +------------+  |       |
  |                                    |  Complete  | <+       |
  |                                    +------------+          |
  |                                                            |
  +------------------------------------------------------------+
*/
enum LexStateKind {
    CheckIndent,
    PushIndent,
    Lex,
    CheckDedent,
    PopIndent,
    Terminate,
    Complete,
}

impl<'a> LexState<'a> {
    fn new(input: &'a str) -> Self {
        /*
        ws = WsToken { leading_line: true, leading_spaces: 0 }
        if !eof
            try_lex_ws()
        ...
        */
        let loc = Location {
            byte_pos: 0,
            line: 1,
            col: 0,
        };
        let ws = WsToken {
            leading_line: true,
            leading_spaces: 0,
        };
        Self::new_at(input, loc, ws)
    }
    fn new_at(input: &'a str, loc: Location, ws: WsToken) -> Self {
        assert!(input.len() <= u32::MAX as usize);
        let chr = input.chars().next();
        let mut this = Self {
            input,
            chr,
            loc,
            kind: LexStateKind::CheckIndent,
            indent_stack: Vec::with_capacity(8),
            ws,
        };
        this.indent_stack.push(0);
        this
    }
    fn eof(&self) -> bool {
        self.chr.is_none()
    }
    fn peek_char(&self) -> char {
        self.chr.unwrap()
    }

    fn peek_char_ahead(&self, n: usize) -> Option<char> {
        self.input[self.loc.byte_pos as usize..]
            .chars()
            .skip(n)
            .next()
    }

    fn next_char(&mut self) -> char {
        assert!(!self.eof());
        let old_char = self.peek_char();
        self.loc.byte_pos += old_char.len_utf8() as u32;
        self.chr = self.input[self.loc.byte_pos as usize..].chars().next();
        self.loc.col += 1;
        if old_char == '\n' {
            self.loc.col = 1;
            self.loc.line += 1;
        }
        old_char
    }
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(builder: &'a mut ProgramBuilder, input: &'b str) -> Self {
        let lexer = LexState::new(input);
        let mut this = Self {
            pool: &mut builder.pool,
            state: lexer,
            indent_level: 0,
            prev: None,
            toks: VecDeque::new(),
            errors: Vec::new(),
        };
        if !this.state.eof() {
            this.try_lex_ws();
        }
        for _ in 0..2 {
            let tok = this.next_token();
            let is_eof = matches!(tok.tok, Token::Eof);
            this.toks.push_back(tok);
            if is_eof {
                break;
            }
        }
        this
    }
    pub(crate) fn parse(pool: &'a mut ProgramBuilder, input: &'b str) -> (ExprId, Vec<ErrorInfo>) {
        let mut this = Self::new(pool, input);
        let expr = this.parse_block(Token::Eof).unwrap();
        let expr = this.pool.exprs.alloc(expr);
        (expr, this.errors)
    }

    pub fn report_span(&mut self, msg: String, span: Span) {
        self.errors.push(ErrorInfo { msg, span });
    }
    pub fn report(&mut self, msg: String) {
        let span = self.peek_ex().span;
        self.report_span(msg, span);
    }
    pub fn peek(&self) -> Token {
        self.peek_ex().tok
    }
    pub fn peek_ex(&self) -> &TokenEx {
        self.toks.front().unwrap()
    }

    pub fn peek_ahead(&self, n: usize) -> Token {
        self.peek_ex_ahead(n).tok
    }
    pub fn peek_ex_ahead(&self, n: usize) -> &TokenEx {
        &self.toks[n]
    }

    pub fn peek_prev(&self) -> Token {
        self.peek_ex_prev().tok
    }
    pub fn peek_ex_prev(&self) -> &TokenEx {
        self.prev.as_ref().unwrap()
    }

    pub fn indent_level(&self) -> u32 {
        self.indent_level
    }

    pub fn bump(&mut self) {
        let prev = self.toks.pop_front().unwrap();
        match prev.tok {
            Token::OpenDelim(DelimKind::Indentation) => self.indent_level += 1,
            Token::CloseDelim(DelimKind::Indentation) => self.indent_level -= 1,
            _ => {}
        }
        self.prev = Some(prev);
        if let None
        | Some(TokenEx {
            tok: Token::Eof, ..
        }) = self.toks.back()
        {
        } else {
            let tok = self.next_token();
            self.toks.push_back(tok);
        }
    }
    pub fn expect(&mut self, expected: Token) -> Option<()> {
        if self.peek() == expected {
            self.bump();
            Some(())
        } else {
            self.report(format!(
                "expected '{:?}', found '{:?}'",
                expected,
                self.peek()
            ));
            None
        }
    }
    pub fn expect_ident(&mut self) -> Option<Ident> {
        if let Token::Ident(id) = self.peek() {
            self.bump();
            Some(id)
        } else {
            self.report(format!("expected identifier, found '{:?}'", self.peek()));
            None
        }
    }
    pub fn accept(&mut self, tok: Token) -> bool {
        let matched = self.peek() == tok;
        if matched {
            self.bump();
        }
        matched
    }

    pub fn parse_comma<F, T>(&mut self, delim: DelimKind, parser: F) -> Vec<T>
    where
        F: Fn(&mut Self) -> Option<T>,
    {
        let mut list = Vec::with_capacity(2);
        self.expect(Token::OpenDelim(delim));
        'outer: while !matches!(self.peek(), Token::CloseDelim(_)) {
            if let Some(elem) = parser(self) {
                list.push(elem);
            } else {
                loop {
                    match self.peek() {
                        Token::Comma => break,
                        Token::NewLine | Token::Eof => break 'outer,
                        _ => self.bump(),
                    }
                }
            }
            if !self.accept(Token::Comma) {
                // if allow_newline { accept newline }
                break;
            }
        }
        self.expect(Token::CloseDelim(delim));
        list
    }
}

impl Parser<'_, '_> {
    // [Indent? Token? (Dedent+ NewLine | NewLine)?] [Dedent] Eof
    fn next_token(&mut self) -> TokenEx {
        /*
        ...
        while !eof
            if ws.leading_line && ws.leading_spaces > indent_stack.top
                yield indent
                indent_stack.push(ws.leading_spaces)
            yield lex_token()
            if eof
                break
            ws=default()
            try_lex_ws()
            if ws.leading_line
                if ws.leading_spaces < indent_stack.top
                    if ws.leading_spaces in indent_stack
                        do
                            yield dedent
                            indent_stack.pop()
                        while ws.leading_spaces < indent_stack.top
                    else
                        yield misindent
                yield newline
        while indent_stack.len > 1
            yield dedent
            indent_stack.pop()
        indent_stack.pop()
        yield eof
        */
        use LexStateKind::*;
        fn handle_eof(lexer: &mut LexState) -> Token {
            if lexer.indent_stack.len() > 1 {
                lexer.kind = Terminate;
                Token::CloseDelim(DelimKind::Indentation)
            } else {
                emit_eof(lexer)
            }
        }
        fn emit_eof(lexer: &mut LexState) -> Token {
            let indent_zero = lexer.indent_stack.pop().unwrap();
            assert!(indent_zero == 0);
            lexer.kind = Complete;
            Token::Eof
        }
        let mut lo;
        let tok = loop {
            lo = self.state.loc;
            match self.state.kind {
                CheckIndent => {
                    if self.state.eof() {
                        break handle_eof(&mut self.state);
                    } else {
                        self.state.kind = Lex;
                        if self.state.ws.leading_line {
                            let outer_indent = *self.state.indent_stack.last().unwrap();
                            if self.state.ws.leading_spaces > outer_indent {
                                self.state.kind = PushIndent;
                                break Token::OpenDelim(DelimKind::Indentation);
                            }
                        }
                    }
                }
                PushIndent => {
                    self.state.kind = Lex;
                    self.state.indent_stack.push(self.state.ws.leading_spaces);
                }
                Lex => {
                    self.state.kind = CheckDedent;
                    match self.lex() {
                        Token::Err => {}
                        tok => break tok,
                    }
                }
                CheckDedent => {
                    if self.state.eof() {
                        break handle_eof(&mut self.state);
                    } else {
                        self.state.kind = CheckIndent;
                        self.state.ws = Default::default();
                        self.try_lex_ws();
                        if self.state.ws.leading_line {
                            lo = self.state.loc;
                            let outer_indent = *self.state.indent_stack.last().unwrap();
                            if self.state.ws.leading_spaces < outer_indent {
                                if self
                                    .state
                                    .indent_stack
                                    .contains(&self.state.ws.leading_spaces)
                                {
                                    self.state.kind = PopIndent;
                                    break Token::CloseDelim(DelimKind::Indentation);
                                } else {
                                    self.report("misindent".to_string());
                                }
                            }
                            break Token::NewLine;
                        }
                    }
                }
                PopIndent => {
                    self.state.indent_stack.pop().unwrap();
                    let outer_indent = *self.state.indent_stack.last().unwrap();
                    if self.state.ws.leading_spaces < outer_indent {
                        break Token::CloseDelim(DelimKind::Indentation);
                    } else {
                        assert!(self.state.ws.leading_spaces == outer_indent);
                        self.state.kind = CheckIndent;
                        break Token::NewLine;
                    }
                }
                Terminate => {
                    self.state.indent_stack.pop().unwrap();
                    if self.state.indent_stack.len() > 1 {
                        break Token::CloseDelim(DelimKind::Indentation);
                    } else {
                        break emit_eof(&mut self.state);
                    }
                }
                Complete => panic!(),
            }
        };
        let hi = self.state.loc;
        TokenEx {
            tok,
            span: lo.to(hi),
        }
    }

    fn try_lex_ws(&mut self) {
        loop {
            match self.state.peek_char() {
                ' ' => {
                    self.state.ws.leading_spaces += 1;
                    self.state.next_char();
                }
                '\n' => {
                    self.state.ws.leading_spaces = 0;
                    self.state.ws.leading_line = true;
                    self.state.next_char();
                }
                '/' => {
                    match self.state.peek_char_ahead(1) {
                        Some('/') => {
                            self.state.ws = Default::default();
                            self.state.next_char();
                            self.state.next_char();
                            let _start = self.state.loc.byte_pos;
                            while !self.state.eof() && self.state.peek_char() != '\n' {
                                self.state.next_char();
                            }
                            let _end = self.state.loc.byte_pos;
                        }
                        Some('*') => {
                            self.state.ws = Default::default();
                            let mut prev = self.state.next_char(); // /
                            let mut level = 0;
                            let _start = self.state.loc.byte_pos + 1;
                            let _end = loop {
                                match (prev, self.state.next_char()) {
                                    ('/', '*') => {
                                        level += 1;
                                    }
                                    ('*', '/') => {
                                        level -= 1;
                                        if level == 0 {
                                            let end = self.state.loc.byte_pos - 2;
                                            break Ok(end);
                                        }
                                    }
                                    (_, curr) => {
                                        // prev is updated here instead of loop body, in order to avoid those syntaxes:
                                        // /*/ (start new comment and then immediately terminate it)
                                        // */* (terminate old comment and start new comment)
                                        prev = curr;
                                    }
                                }
                                if self.state.eof() {
                                    let end = self.state.loc.byte_pos;
                                    break Err(end);
                                }
                            };
                        }
                        _ => {
                            break;
                        }
                    }
                }
                _ => break,
            }
            if self.state.eof() {
                break;
            }
        }
    }

    fn lex(&mut self) -> Token {
        use Token::*;
        fn match_or(input: &mut Parser, expected: char, then: Token, default: Token) -> Token {
            let input = &mut input.state;
            debug_assert_ne!(expected, '\n');
            if !input.eof() && input.peek_char() == expected {
                input.next_char();
                then
            } else {
                default
            }
        }
        let start_loc = self.state.loc;
        match self.state.next_char() {
            '(' => OpenDelim(DelimKind::Paren),
            '[' => OpenDelim(DelimKind::Square),
            '{' => OpenDelim(DelimKind::Curly),
            ')' => CloseDelim(DelimKind::Paren),
            ']' => CloseDelim(DelimKind::Square),
            '}' => CloseDelim(DelimKind::Curly),
            '.' => Dot,
            ',' => Comma,
            ';' => Semi,
            ':' => match_or(self, ':', Colon2, Colon),
            '+' => Plus,
            '-' => match_or(self, '>', ThinArrow, Minus),
            '*' => Star,
            '/' => Slash,
            '\\' => Backslash,
            '%' => Percent,
            '&' => match_or(self, '&', Amp2, Amp),
            '|' => match_or(self, '|', Pipe2, Pipe),
            '~' => Tilde,
            '^' => Caret,
            '!' => match_or(self, '=', NEq, ExclMark),
            '=' => match self.state.peek_char_ahead(0) {
                Some('=') => {
                    self.state.next_char();
                    Eq2
                }
                Some('>') => {
                    self.state.next_char();
                    FatArrow
                }
                _ => Eq,
            },
            '<' => match_or(self, '=', Le, Lt),
            '>' => match_or(self, '=', Ge, Gt),
            '\'' => Apostrophe,
            '"' => {
                let start = self.state.loc.byte_pos;
                loop {
                    let end = self.state.loc.byte_pos;
                    if self.state.eof() {
                        self.report("unterminated quote".to_string());
                        break Err;
                    } else {
                        match self.state.next_char() {
                            '"' => {
                                let intern = self.pool.interner.intern(
                                    self.state.input[start as usize..end as usize].as_bytes(),
                                );
                                break Quote(intern);
                            }
                            '\\' if !self.state.eof() && self.state.peek_char() == '"' => {
                                self.state.next_char(); // escape "
                            }
                            _ => {}
                        }
                    }
                }
            }
            c if c.is_ascii_alphabetic() || c == '_' => {
                while !self.state.eof()
                    && (self.state.peek_char().is_alphanumeric() || self.state.peek_char() == '_')
                {
                    self.state.next_char();
                }
                let end = self.state.loc.byte_pos;
                let s = &self.state.input[start_loc.byte_pos as usize..end as usize];
                if s == "_" {
                    Underscore
                } else {
                    use token::Keyword::*;
                    match s {
                        "module" => Keyword(Auto),
                        "struct" => Keyword(Struct),
                        "union" => Keyword(Union),
                        "enum" => Keyword(Enum),
                        "func" => Keyword(Func),
                        "type" => Keyword(Type),
                        "const" => Keyword(Const),
                        "let" => Keyword(Let),
                        "var" => Keyword(Var),
                        "break" => Keyword(Break),
                        "continue" => Keyword(Continue),
                        "return" => Keyword(Return),
                        "defer" => Keyword(Defer),
                        "undef" => Keyword(Undef),
                        "if" => Keyword(If),
                        "else" => Keyword(Else),
                        "do" => Keyword(Do),
                        "match" => Keyword(Match),
                        "while" => Keyword(While),
                        "for" => Keyword(For),
                        "try" => Keyword(Try),
                        "yield" => Keyword(Yield),
                        "await" => Keyword(Await),
                        "in" => Keyword(In),
                        _ => {
                            let intern = self.pool.interner.intern(s.as_bytes());
                            Ident(crate::token::Ident(intern))
                        }
                    }
                }
            }
            c if c.is_ascii_digit() => {
                while !self.state.eof() && self.state.peek_char().is_ascii_digit() {
                    self.state.next_char();
                }
                let end = self.state.loc.byte_pos;
                let s = &self.state.input[start_loc.byte_pos as usize..end as usize];
                if let Ok(n) = u64::from_str_radix(s, 10) {
                    let intern = self.pool.interner.intern(&n.to_le_bytes());
                    Number(intern)
                } else {
                    self.report("number is too big".to_string());
                    Err
                }
            }
            _ => {
                self.report("unknown character".to_string());
                Err
            }
        }
    }
}
