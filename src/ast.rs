#![allow(unused_assignments)]

use crate::lexer::{Token, TokenType};
use crate::error_handler::*;

use regex::Regex;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTNode {
    pub token_type: TokenType,
    pub value:  String,
    pub children: Vec<Rc<RefCell<Box<ASTNode>>>>
}

impl ASTNode {
    pub fn new(token: &Token) -> Self {
        ASTNode {
            token_type: token.token_type.clone(),
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

        // Update scope if token is a }
        if token.token_type == TokenType::PunctuationBraceClose {
            let (new_scope_stack, new_current_parent) = pop_parent_node(scope_stack_, &mut current_parent_);
            scope_stack_ = new_scope_stack;
            current_parent_ = new_current_parent;
            i += 1;
            continue;
        }

        // Check if token.token_type starts with Keyword
        let current_token_type_str = format!("{:?}", token.token_type);
        let keyword_regex = Regex::new(r"^Keyword.*").unwrap();

        if keyword_regex.is_match(&current_token_type_str) {
            // Maybe put checks for token.token_type in a match

            if token.token_type == TokenType::KeywordFn {
                let mut fn_init_tokens: Vec<&Token> = Vec::new();
                let mut fn_init: Box<ASTNode> = Box::new(ASTNode::new(token));

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::EOL {
                    fn_init_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                fn_init.children = generate_ast_fn_init(fn_init_tokens, file_path);

                let last_child: &&Rc<RefCell<Box<ASTNode>>> = &fn_init.children.last().unwrap();

                if (**last_child).borrow().token_type != TokenType::PunctuationBraceOpen {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Function initialization: Missing '{'",
                        tokens[i].line,
                        tokens[i].column
                    ).with_file(file_path).panic();
                }

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(fn_init)));

                let code_block_token: Token = Token{token_type: TokenType::CodeBlock, value: "".to_string(), line: tokens[i].line, column: tokens[i].column};
                let code_block: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(&code_block_token))));

                let code_block_clone: Rc<RefCell<Box<ASTNode>>> = Rc::clone(&code_block);

                current_parent_.borrow_mut().children.push(code_block);

                current_parent_ = code_block_clone;
                scope_stack_.push(Rc::clone(&current_parent_));

                continue;
            }

            if token.token_type == TokenType::KeywordReturn {
                let return_init: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(token))));
                let mut return_tokens: Vec<&Token> = Vec::new();

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    return_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                for token in return_tokens {
                    return_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token)))));
                }

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    return_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i])))));
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

            if token.token_type == TokenType::KeywordFree {
                let free_init: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(token))));
                let mut free_tokens: Vec<&Token> = Vec::new();

                let mut j: usize = i + 1;

                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    free_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                for token in free_tokens {
                    free_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token)))));
                }

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    free_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i])))));
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

            if token.token_type == TokenType::KeywordLet {
                let mut var_tokens: Vec<&Token> = Vec::new();
                let variable_init: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(token))));

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    var_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                variable_init.borrow_mut().children = generate_ast_variable(var_tokens, file_path);

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    variable_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i])))));
                    current_parent_.borrow_mut().children.push(variable_init);
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

            continue;
        }

        if token.token_type == TokenType::Identifier{
            if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::OperatorAssign {
                let mut identifier_tokens: Vec<&Token> = Vec::new();
                let identifier_init: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(token))));

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::OperatorSemicolon && tokens[j].token_type != TokenType::EOL {
                    identifier_tokens.push(&tokens[j]);
                    j += 1;
                }

                i = j;

                identifier_init.borrow_mut().children = generate_ast_variable(identifier_tokens, file_path);

                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    identifier_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i])))));
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

/// Pops the last element from the scope stack and sets `current_parent` to the last element on the stack.
///
/// # Parameters
///
/// - `scope_stack`: The stack of parent nodes.
/// - `current_parent`: A mutable reference to the current parent node.
///
/// # Returns
///
/// A tuple containing the modified `scope_stack` and the new `current_parent`.
///
/// # Panics
///
/// If the `scope_stack` is empty, an error is thrown.
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
fn generate_ast_function_call(tokens: &Vec<Token>, i: usize, file_path: &str) -> (Option<Rc<RefCell<Box<ASTNode>>>>, usize){
    let function_call: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i]))));
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
        function_call.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(tokens)))));
    }

    if j < tokens.len() && tokens[j].token_type == TokenType::OperatorSemicolon {
        function_call.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[j])))));
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

fn generate_ast_fn_init(tokens: Vec<&Token>, file_path: &str) -> Vec<Rc<RefCell<Box<ASTNode>>>> {
    let mut fn_init_ast: Vec<Rc<RefCell<Box<ASTNode>>>> = Vec::new();

    if tokens.is_empty() {
        let _err = Err::new(
            ErrorType::Syntax,
            "Function initialization is empty",
            0,
            0
        ).with_file(file_path).panic();

        return fn_init_ast;
    }

    for token in tokens {
        fn_init_ast.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token)))));
    }

    fn_init_ast
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
fn generate_ast_variable(tokens: Vec<&Token>, file_path: &str) -> Vec<Rc<RefCell<Box<ASTNode>>>> {
    let mut var_ast: Vec<Rc<RefCell<Box<ASTNode>>>> = Vec::new();

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
        var_ast.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token)))));
    }

    var_ast
}