#![allow(unused_assignments)]

use crate::lexer::{Token, TokenType};
use crate::error_handler::*;

use regex::Regex;
use std::rc::Rc;
use std::cell::RefCell;

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

pub fn generate_ast(tokens: Vec<Token>, file_path: &str) -> Box<ASTNode> {
    let root_token: &Token = &Token {
        token_type: TokenType::Root,
        value: "".to_string(),
        line: 0,
        column: 0
    };

    let root: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(root_token))));
    let mut current_parent_: Rc<RefCell<Box<ASTNode>>> = Rc::clone(&root);
    let mut scope_stack_: Vec<Rc<RefCell<Box<ASTNode>>>> = vec![Rc::clone(&root)];

    let mut i: usize = 0;

    while i < tokens.len() {
        let token: &Token = &tokens[i];

        let current_token_type_str = format!("{:?}", token.token_type);
        let keyword_regex = Regex::new(r"^Keyword.*").unwrap();

        // Check if token.token_type starts with Keyword
        if keyword_regex.is_match(&current_token_type_str) {
            // Maybe put checks for token.token_type in a match

            if token.token_type == TokenType::KeywordFn {
                let mut fn_init_tokens: Vec<&Token> = Vec::new();
                let mut fn_init: Box<ASTNode> = Box::new(ASTNode::new(token));

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
                    current_parent_.borrow_mut().children.push(free_init);

                    if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::PunctuationBraceClose {
                        let (new_scope_stack, new_current_parent) = pop_parent_node(scope_stack_, &mut current_parent_);
                        scope_stack_ = new_scope_stack;
                        current_parent_ = new_current_parent;
                    }
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

            if token.token_type == TokenType::KeywordReturn {
                let mut return_init: Box<ASTNode> = Box::new(ASTNode::new(token));
                let mut return_tokens: Vec<&Token> = Vec::new();

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    return_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                for token in return_tokens {
                    return_init.children.push(Box::new(ASTNode::new(token)));
                }

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    return_init.children.push(Box::new(ASTNode::new(&tokens[i])));
                    current_parent_.borrow_mut().children.push(return_init);

                    let (new_scope_stack, new_current_parent) = pop_parent_node(scope_stack_, &mut current_parent_);
                    scope_stack_ = new_scope_stack;
                    current_parent_ = new_current_parent;
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
                    current_parent_.borrow_mut().children.push(variable_init);

                    if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::PunctuationBraceClose {
                        let (new_scope_stack, new_current_parent) = pop_parent_node(scope_stack_, &mut current_parent_);
                        scope_stack_ = new_scope_stack;
                        current_parent_ = new_current_parent;
                    }
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
            if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::OperatorAssign {
                let mut identifier_tokens: Vec<&Token> = Vec::new();
                let mut identifier_init: Box<ASTNode> = Box::new(ASTNode::new(token));

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    identifier_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                identifier_init.children = generate_ast_variable(identifier_tokens, file_path);

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    identifier_init.children.push(Box::new(ASTNode::new(&tokens[i])));
                    current_parent_.borrow_mut().children.push(identifier_init);

                    if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::PunctuationBraceClose {
                        let (new_scope_stack, new_current_parent) = pop_parent_node(scope_stack_, &mut current_parent_);
                        scope_stack_ = new_scope_stack;
                        current_parent_ = new_current_parent;
                    }
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

            if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::PunctuationParenOpen {
                let (function_call, new_i) = generate_ast_function_call(&tokens, i, file_path);

                i = new_i;

                current_parent_.borrow_mut().children.push(function_call.unwrap());
                continue;
            }
        }

        i += 1;
    }

    scope_stack_.clear();
    drop(current_parent_);
    if let Ok(inner) = Rc::try_unwrap(root) {
        let boxed_ast: Box<ASTNode> = inner.into_inner();
        boxed_ast
    } else {
        let _err = Err::new(
            ErrorType::Other,
            "Internal Error: Failed to unwrap root ast node!",
            0,
            0
        ).panic();

        unreachable!();
    }
}

fn pop_parent_node(mut scope_stack: Vec<Rc<RefCell<Box<ASTNode>>>>, current_parent: &mut Rc<RefCell<Box<ASTNode>>>) -> (Vec<Rc<RefCell<Box<ASTNode>>>>, Rc<RefCell<Box<ASTNode>>>) {
    if !scope_stack.is_empty() {
        scope_stack.pop();
        if let Some(last) = scope_stack.last() {
            *current_parent = Rc::clone(last);
        } else {
            let _err = Err::new(
                ErrorType::Other,
                "Internal Error: Scope stack is empty — no valid current_parent!",
                0,
                0
            ).panic();
        }
    }

    (scope_stack, Rc::clone(current_parent))
}

// fn pop_parent_node<'a>(mut scope_stack: Vec<&'a mut Box<ASTNode>>, current_parent: &mut &'a mut Box<ASTNode>, ) -> (Vec<&'a mut Box<ASTNode>>, &'a mut Box<ASTNode>) {
//     if !scope_stack.is_empty() {
//         scope_stack.pop();
//         if let Some(last) = scope_stack.last_mut() {
//             *current_parent = *last;
//         } else {
//             let _err = Err::new(
//                 ErrorType::Other,
//                 "Internal Error: Scope stack is empty — no valid current_parent!",
//                 0,
//                 0
//             ).panic();
//         }
//     }
//
//     (scope_stack, current_parent)
// }

/// Generates an Abstract Syntax Tree (AST) from a function call.
///
/// # Parameters
///
/// - `tokens`: The tokens of the function call.
/// - `i`: The index of the function call token in `tokens`.
/// - `file_path`: The path of the file the function call is in.
///
/// # Returns
///
/// A tuple containing the generated `ASTNode` and the index of the last token processed.
///
/// # Errors
///
/// Prints an error message if `tokens` is empty and returns `None`.
///
/// # Notes
///
/// This function assumes that the first token is the function call token, and that the last token is a semicolon.
/// This function does not handle cases where the function call token is missing or the semicolon is missing.
fn generate_ast_function_call(tokens: &Vec<Token>, i: usize, file_path: &str) -> (Option<Box<ASTNode>>, usize){
    let mut function_call: Box<ASTNode> = Box::new(ASTNode::new(&tokens[i]));
    let mut function_call_tokens: Vec<&Token> = Vec::new();

    let mut j: usize = i + 1;
    let mut paren_count = 0;
    while j < tokens.len() && j + 1 < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
        if tokens[j].token_type == TokenType::PunctuationParenOpen {
            paren_count += 1;
        } else if tokens[j].token_type == TokenType::PunctuationParenClose {
            if paren_count == 0 {
                break;
            }
            paren_count -= 1;
        }
        function_call_tokens.push(&tokens[j]);
        j += 1;
    }

    for tokens in function_call_tokens {
        function_call.children.push(Box::new(ASTNode::new(tokens)));
    }

    if j < tokens.len() && tokens[j].token_type == TokenType::OperatorSemicolon {
        function_call.children.push(Box::new(ASTNode::new(&tokens[j])));
        (Some(function_call), j)
    } else {
        let _err = Err::new(
            ErrorType::Syntax,
            "Missing semicolon",
            tokens[i].line,
            tokens[i].column
        ).with_file(file_path).panic();

        (None, j)
    }
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