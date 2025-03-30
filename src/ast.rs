use crate::lexer::{Token, TokenType};
use crate::error_handler::*;

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

        // if token.token_type == TokenType::Identifier{
        //     ast.push(Box::new(ASTNode::new(&tokens[i])));
        //     i += 1;
        // }

        if token.token_type == TokenType::KeywordLet {
            let mut var_tokens: Vec<&Token> = Vec::new();
            let mut variable_init: Box<ASTNode> = Box::new(ASTNode::new(token));

            let mut j: usize = i + 1;
            while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon {
                var_tokens.push(&tokens[j]);
                j += 1;
            }

            i = j;

            variable_init.children = generate_ast_variable(var_tokens, file_path);
            current_parent = Some(variable_init);
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

fn generate_ast_variable(tokens: Vec<&Token>, file_path: &str) -> Vec<Box<ASTNode>> {
    let mut var_ast: Vec<Box<ASTNode>> = Vec::new();

    if tokens.is_empty() {
        let _err = Err::new(
            ErrorType::Syntax,
            "Variable declaration is empty",
            tokens[0].line,
            tokens[0].column
        ).with_file(file_path).print();

        return var_ast;
    }

    for token in tokens {
        var_ast.push(Box::new(ASTNode::new(token)));
    }

    var_ast
}