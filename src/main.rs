#![allow(dead_code)]

mod lexer;
mod ast;
pub mod error_handler;

fn main() {
    let tokens: Vec<lexer::Token> = lexer::get_tokens("/home/raphael/Development/Quirk/test.qk");

    debug_print_tokens(&tokens);

    let ast: Vec<Box<ast::ASTNode>> = ast::generate_ast(tokens, "/home/raphael/Development/Quirk/test.qk");

    debug_print_ast(&ast);
}

fn debug_print_tokens(tokens: &Vec<lexer::Token>) {
    for token in tokens {
        println!("{:?}", token);
    }
}

fn debug_print_ast(ast: &Vec<Box<ast::ASTNode>>) {
    for node in ast {
        println!("{:#?}", node);
    }
}