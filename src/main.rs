use regex::Tokenizer;

mod regex;

fn main() {
    let tokenizer = Tokenizer::new(r"\f\n\r\\");
    println!("{:?}", tokenizer.tokenize());
}
