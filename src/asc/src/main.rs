use frontend::{
    semantic,
    syntax::{self, ast::SemaBuilder, parser::Parser, token::Token},
};
use std::{mem::size_of, path::PathBuf};

fn main() {
    dbg!(size_of::<semantic::il::BasicBlock>());
    dbg!(size_of::<semantic::il::Operand>());
    dbg!(size_of::<semantic::il::Instruction>());
    dbg!(size_of::<semantic::il::Terminator>());
    dbg!(size_of::<semantic::ildecl::Scope>());
    dbg!(size_of::<semantic::ildecl::Type>());
    dbg!(size_of::<semantic::il::Branch>());
    //return;
    dbg!(size_of::<syntax::ast::SourceFile>());
    dbg!(size_of::<syntax::ast::Item>());
    dbg!(size_of::<syntax::ast::Statement>());
    dbg!(size_of::<syntax::ast::Expression>());
    dbg!(size_of::<syntax::ast::Pattern>());

    dbg!(size_of::<syntax::ast::Id>());
    dbg!(size_of::<syntax::ast::Literal>());
    dbg!(size_of::<syntax::ast::PatternDecl>());
    dbg!(size_of::<syntax::ast::Field>());

    let mut path = std::env::current_dir().unwrap();
    path.push("as-src");

    if true {
        let mut sb = SemaBuilder::new();
        fn traverse_dir(dir: PathBuf, pb: &mut SemaBuilder) {
            for e in std::fs::read_dir(dir).unwrap() {
                let e = e.unwrap();
                let ty = e.file_type().unwrap();
                let path = e.path();
                if ty.is_dir() {
                    let name = path.file_name().unwrap().to_str();
                    let name = name.unwrap();
                    pb.push_dir(name);
                    traverse_dir(path, pb);
                    pb.pop_dir();
                } else if ty.is_file() && path.extension().unwrap() == "as" {
                    let no_ext = path.with_extension("");
                    let name = no_ext.file_name().unwrap().to_str();
                    let name = name.unwrap();
                    let data = std::fs::read(e.path()).unwrap();
                    let data = std::str::from_utf8(data.as_slice()).unwrap();
                    //print_tokens(data);
                    pb.add_file(name, data);
                }
            }
        }
        traverse_dir(path, &mut sb);
        let mut sema = sb.into_sema();
        sema.analyze();

        println!("{}", sema.dump().as_str());
    }
}

#[allow(dead_code)]
fn print_tokens(src: &str) {
    let mut sb = SemaBuilder::new();
    let mut parser = Parser::new(&mut sb, src);
    loop {
        let tok = parser.peek_ex();
        let range = tok.span.lo.byte_pos as usize..tok.span.hi.byte_pos as usize;
        println!(
            "{:<32} '{}'",
            format!(
                "{}. {}..{:?}",
                tok.span.lo.line,
                if range.is_empty() { "?" } else { " " },
                tok.tok
            ),
            &src[range],
        );
        let is_eof = matches!(tok.tok, Token::Eof);
        parser.bump();
        if is_eof {
            break;
        }
    }
}
