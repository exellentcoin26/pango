use pango::Lexer;

#[derive(Debug, Clone, Copy)]
enum TokenKind {
    A,
    B,
    C,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let lexer = Lexer::builder()
        .with_token("aaaa", TokenKind::A)?
        .with_token("b{4}", TokenKind::B)?
        .with_token("b{4}", TokenKind::C)?;

    for token in lexer.tokenize("bbbbbbbbaaaabbbb") {
        println!("{:?}", token);
    }

    Ok(())
}
