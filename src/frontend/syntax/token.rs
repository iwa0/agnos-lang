use data_structures::interner::{Intern, Interner};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TokenEx {
    pub tok: Token,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub lo: Location,
    pub hi: Location,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Location {
    pub byte_pos: u32,
    pub col: u32,
    pub line: u32,
}

#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub struct WsToken {
    pub leading_line: bool,
    pub leading_spaces: u32,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Ident(Intern);

pub fn mk_ident(int: &mut Interner, s: &str) -> Ident {
    let a = int.intern(s.as_bytes());
    Ident(a)
}

pub fn rd_ident(int: &Interner, id: Ident) -> &str {
    let s = int.get(id.0);
    std::str::from_utf8(s).unwrap()
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token {
    Err,
    Eof,
    NewLine,

    Keyword(Keyword),
    Ident(Ident),
    Number(Intern),
    Quote(Intern),

    OpenDelim(DelimKind),
    CloseDelim(DelimKind),

    Dot,
    Comma,
    Semi,
    Colon,
    Colon2,
    ThinArrow,
    FatArrow,
    Backslash,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Amp,
    Pipe,
    Tilde,
    Caret,

    Amp2,
    Pipe2,
    ExclMark,

    Lt,
    Le,
    Gt,
    Ge,

    Eq,
    Eq2,
    Ne,
    Apostrophe,

    Underscore,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DelimKind {
    Paren,
    Square,
    Curly,
    Indentation,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Keyword {
    Auto,
    Module,
    Struct,
    Union,
    Enum,
    Fn,
    Const,

    Mut,
    Let,
    Break,
    Continue,
    Return,
    Defer,

    Undef,
    If,
    Else,
    Do,
    Match,
    While,
    For,
    Case,
    Try,
    Yield,
    Await,
    In,
}

impl Location {
    pub fn to(self, hi: Location) -> Span {
        Span { lo: self, hi }
    }
}

impl Span {
    pub fn consecutive(self, hi: Span) -> bool {
        self.hi.byte_pos == hi.lo.byte_pos
    }
}
