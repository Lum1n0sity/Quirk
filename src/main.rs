#![allow(dead_code)]

mod lexer;
mod ast;
pub mod error_handler;

fn main() {
    let tokens: Vec<lexer::Token> = lexer::get_tokens("/home/raphael/Development/Quirk/test.qk");

    // debug_print_tokens(&tokens);

    let ast: Box<ast::ASTNode> = ast::generate_ast(tokens, "/home/raphael/Development/Quirk/test.qk");
    
    debug_print_ast(&ast);
}


/// Prints a debug representation of the tokens.
///
/// # Parameters
///
/// - `tokens`: A reference to a vector of `Token`s.
///
/// This function iterates over each token and prints its debug
/// representation to the console, aiding in debugging and visualization
/// of the token stream.
pub fn debug_print_tokens(tokens: &Vec<lexer::Token>) {
    for token in tokens {
        println!("{:?}", token);
    }
}


/// Prints a debug representation of the AST to the console.
///
/// # Parameters
///
/// - `node`: A reference to a boxed `ASTNode`.
///
/// This function prints the debug representation of `node` and its children
/// to the console, aiding in debugging and visualization of the Abstract
/// Syntax Tree (AST).
pub fn debug_print_ast(node: &Box<ast::ASTNode>) {
    println!("{:#?}", node);
}