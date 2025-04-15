#![allow(unused_assignments)]

use crate::lexer::{Token, TokenType};
use crate::error_handler::*;
use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTNode {
    pub token_type: String,
    pub value:  String,
    pub children: Vec<Box<ASTNode>>
}

impl ASTNode {
    pub fn new(token: &Token) -> Self {
        ASTNode {
            token_type: format!("{:?}", token.token_type),
            value: token.value.clone(),
            children: Vec::new()
        }
    }
}

pub fn generate_ast(tokens: Vec<Token>, file_path: &str) -> Vec<Box<ASTNode>> {
    let mut ast: Vec<Box<ASTNode>> = Vec::new();
    let mut i: usize = 0;
    let mut current_parent: Option<Box<ASTNode>> = None;

    while i < tokens.len() {
        let token: &Token = &tokens[i];

        let current_token_type_str = format!("{:?}", token.token_type);
        let keyword_regex = Regex::new(r"^Keyword.*").unwrap();

        // Check if token.token_type starts with Keyword
        if keyword_regex.is_match(&current_token_type_str) {
            // Maybe put checks for token.token_type in a match

            if token.token_type == TokenType::KeywordFn {
                let mut fn_init_tokens: Vec<&Token> = Vec::new();
                let mut fn_int: Box<ASTNode> = Box::new(ASTNode::new(token));

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::PunctuationBraceOpen {
                    fn_init_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j

                // fn_init.children = generate_ast_fn_init(fn_init_tokens, file_path);

                // TODO: Handle function body
            }

            if token.token_type == TokenType::KeywordFree {
                let mut free_init: Box<ASTNode> = Box::new(ASTNode::new(token));
                let mut free_tokens: Vec<&Token> = Vec::new();

                let mut j: usize = i + 1;

                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    free_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                for token in free_tokens {
                    free_init.children.push(Box::new(ASTNode::new(token)));
                }

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    free_init.children.push(Box::new(ASTNode::new(&tokens[i])));
                    current_parent = Some(free_init);
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Missing semicolon",
                        token.line,
                        token.column
                    ).with_file(file_path).panic();
                }

                continue;
            }

            if token.token_type == TokenType::KeywordLet {
                let mut var_tokens: Vec<&Token> = Vec::new();
                let mut variable_init: Box<ASTNode> = Box::new(ASTNode::new(token));

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    var_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                variable_init.children = generate_ast_variable(var_tokens, file_path);

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    variable_init.children.push(Box::new(ASTNode::new(&tokens[i])));
                    current_parent = Some(variable_init);
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Missing semicolon",
                        token.line,
                        token.column
                    ).with_file(file_path).panic();
                }

                continue;
            }
        }

        if token.token_type == TokenType::Identifier{
            let is_next_token_assign: bool = if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::OperatorAssign {true} else {false};

            if is_next_token_assign {
                let mut var_tokens: Vec<&Token> = Vec::new();
                let mut variable_init: Box<ASTNode> = Box::new(ASTNode::new(token));

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    var_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                variable_init.children = generate_ast_variable(var_tokens, file_path);

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    variable_init.children.push(Box::new(ASTNode::new(&tokens[i])));
                    current_parent = Some(variable_init);
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Missing semicolon",
                        token.line,
                        token.column
                    ).with_file(file_path).panic();
                }
                continue;
            }
        }

        if token.token_type == TokenType::OperatorSemicolon {
            if let Some(parent) = current_parent.take() {
                ast.push(parent);
            }
        }

        i += 1;
    }

    ast
}

/// Generates an Abstract Syntax Tree (AST) from a variable declaration.
///
/// # Parameters
///
/// - `tokens`: The tokens of the variable declaration.
/// - `file_path`: The path of the file the variable declaration is in.
///
/// # Returns
///
/// A vector of `ASTNode`s representing the variable declaration.
///
/// # Errors
///
/// Prints an error message if `tokens` is empty and returns an empty vector.
fn generate_ast_variable(tokens: Vec<&Token>, file_path: &str) -> Vec<Box<ASTNode>> {
    let mut var_ast: Vec<Box<ASTNode>> = Vec::new();

    if tokens.is_empty() {
        let _err = Err::new(
            ErrorType::Syntax,
            "Variable declaration is empty",
            0,
            0
        ).with_file(file_path).panic();

        return var_ast;
    }

    for token in tokens {
        var_ast.push(Box::new(ASTNode::new(token)));
    }

    var_ast
}