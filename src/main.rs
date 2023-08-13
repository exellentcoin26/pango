use pango::Lexer;

#[derive(Debug, Clone, Copy)]
enum Foo {
    A,
    B,
    C,
    D { len: usize },
    E,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let lexer = Lexer::builder()
        .with_token("aaaa", Foo::A)?
        .with_token("b{4,}", Foo::B)?
        .with_token("b{4}", Foo::C)?
        .with_token_map(
            "d*",
            Box::new(|token_source| Foo::D {
                len: token_source.len(),
            }),
        )?
        .with_token(r"/\* .*", Foo::A)?
        .with_token(r"/\* .* \*/", Foo::E)?;

    for token in lexer.tokenize("bbbbbbbaaaabbbbddddddddddddddddd/* foo bar baz **** *") {
        println!("{:?}", token);
    }

    Ok(())
}
