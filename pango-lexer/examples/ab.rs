use pango_lexer::Lexer;

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
        .with_token_unit("aaaa", Foo::A)?
        .with_token_unit("b{4,}", Foo::B)?
        .with_token_unit("b{4}", Foo::C)?
        .with_token_map("d*", |token_source| Foo::D {
            len: token_source.len(),
        })?
        .with_token_unit(r"/\* .*", Foo::A)?
        .with_token_unit(r"/\* .* \*/", Foo::E)?;

    for token in lexer.tokenize("bbbbbbbaaaabbbbddddddddddddddddd/* foo bar baz **** *") {
        println!("{:?}", token);
    }

    Ok(())
}
