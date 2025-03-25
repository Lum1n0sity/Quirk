mod lexer;
mod ast;
mod error_handler;

fn main() {
    let tokens: Vec<lexer::Token> = lexer::get_tokens("/home/raphael/Development/Quirk/test.qk");

    for token in tokens {
        println!("{:?}", token);
    }
}
