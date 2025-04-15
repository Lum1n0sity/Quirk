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

/// Prints a formatted debug representation of the Abstract Syntax Tree (AST).
///
/// # Parameters
///
/// - `ast`: A reference to a vector of boxed `ASTNode`s representing the AST.
///
/// This function iterates over each node in the AST and prints its debug
/// representation to the console, aiding in debugging and visualization of
/// the AST structure.
pub fn debug_print_ast(node: &Box<ast::ASTNode>) {
    println!("{:#?}", node);
}