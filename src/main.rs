use regex::Tokenizer;

mod regex;

fn main() {
    let tokenizer = Tokenizer::new(r"\xFA\xfa\x1f\x30\u12Fb\u{13c}\f\n\r\\");
    println!("{:?}", tokenizer.tokenize());
}
