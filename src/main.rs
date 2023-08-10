use pango::Lexer;

#[derive(Debug, Clone, Copy)]
enum TokenKind {
    A,
    B,
    C,
    D { len: usize },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let lexer = Lexer::builder()
        .with_token("aaaa", TokenKind::A)?
        .with_token("b{4}", TokenKind::B)?
        .with_token("b{4}", TokenKind::C)?
        .with_token_map(
            "d*",
            Box::new(|token_source| TokenKind::D {
                len: token_source.len(),
            }),
        )?;

    for token in lexer.tokenize("bbbbbbbbaaaabbbbddddddddddd") {
        println!("{:?}", token);
    }

    Ok(())
}
