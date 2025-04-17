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

    let mut i: usize = 0;

    while i < tokens.len() {
        let token: &Token = &tokens[i];

        // Update scope if token is a }
        if token.token_type == TokenType::PunctuationBraceClose {
            let new_current_parent = pop_current_parent(&root, &current_parent_, file_path);
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

                if j < tokens.len() {
                    i = j;
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Unexpected EOF after function initialization",
                        token.line,
                        token.column
                    ).with_file(file_path).panic();
                }

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

            if token.token_type == TokenType::KeywordIf {
                let mut if_init: Box<ASTNode> = Box::new(ASTNode::new(token));
                let mut condition_tokens: Vec<&Token> = Vec::new();

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::EOL {
                    condition_tokens.push(&tokens[j]);
                    j += 1;
                }

                if j < tokens.len() {
                    i = j;
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Unexpected EOF after if-condition",
                        token.line,
                        token.column
                    ).with_file(file_path).panic();
                }

                if_init.children = generate_condition_ast(condition_tokens, file_path);

                let last_child: &&Rc<RefCell<Box<ASTNode>>> = &if_init.children.last().unwrap();

                if (**last_child).borrow().token_type != TokenType::PunctuationBraceOpen {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Condition initialization: Missing '{'",
                        tokens[i].line,
                        tokens[i].column
                    ).with_file(file_path).panic();
                }

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(if_init)));

                let code_block_token: Token = Token{token_type: TokenType::CodeBlock, value: "".to_string(), line: tokens[i].line, column: tokens[i].column};
                let code_block: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(&code_block_token))));

                let code_block_clone: Rc<RefCell<Box<ASTNode>>> = Rc::clone(&code_block);

                current_parent_.borrow_mut().children.push(code_block);

                current_parent_ = code_block_clone;

                continue;
            }


            if token.token_type == TokenType::KeywordElseIf {
                let mut elif_init:Box<ASTNode> = Box::new(ASTNode::new(token));
                let mut condition_tokens: Vec<&Token> = Vec::new();

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::EOL {
                    condition_tokens.push(&tokens[j]);
                    j += 1;
                }

                if j < tokens.len() {
                    i = j;
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Unexpected EOF after elif-condition",
                        token.line,
                        token.column
                    ).with_file(file_path).panic();
                }

                elif_init.children = generate_condition_ast(condition_tokens, file_path);

                let last_child: &&Rc<RefCell<Box<ASTNode>>> = &elif_init.children.last().unwrap();

                if (**last_child).borrow().token_type != TokenType::PunctuationBraceOpen {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Condition initialization: Missing '{'",
                        tokens[i].line,
                        tokens[i].column
                    ).with_file(file_path).panic();
                }

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(elif_init)));

                let code_block_token: Token = Token{token_type: TokenType::CodeBlock, value: "".to_string(), line: tokens[i].line, column: tokens[i].column};
                let code_block: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(&code_block_token))));

                let code_block_clone: Rc<RefCell<Box<ASTNode>>> = Rc::clone(&code_block);

                current_parent_.borrow_mut().children.push(code_block);

                current_parent_ = code_block_clone;

                continue;
            }

            if token.token_type == TokenType::KeywordElse {
                let mut else_init:Box<ASTNode> = Box::new(ASTNode::new(token));

                if i + 1 >= tokens.len() || tokens[i + 1].token_type != TokenType::PunctuationBraceOpen {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Else statement: Missing '{'",
                        tokens[i].line,
                        tokens[i].column
                    ).with_file(file_path).panic();
                }

                else_init.children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i + 1])))));
                i += 1;

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(else_init)));

                let code_block_token: Token = Token{token_type: TokenType::CodeBlock, value: "".to_string(), line: tokens[i].line, column: tokens[i].column};
                let code_block: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(&code_block_token))));

                let code_block_clone: Rc<RefCell<Box<ASTNode>>> = Rc::clone(&code_block);

                current_parent_.borrow_mut().children.push(code_block);

                current_parent_ = code_block_clone;

                continue;
            }
            
            if token.token_type == TokenType::KeywordWhile {
                let mut while_init: Box<ASTNode> = Box::new(ASTNode::new(token));
                let mut condition_tokens: Vec<&Token> = Vec::new();
                
                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::EOL {
                    condition_tokens.push(&tokens[j]);
                    j += 1;
                }

                if j < tokens.len() {
                    i = j;
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Unexpected EOF after while-condition",
                        token.line,
                        token.column
                    ).with_file(file_path).panic();
                }
                
                while_init.children = generate_condition_ast(condition_tokens, file_path);
                
                let last_child: &&Rc<RefCell<Box<ASTNode>>> = &while_init.children.last().unwrap();
                
                if (**last_child).borrow().token_type != TokenType::PunctuationBraceOpen {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Condition initialization: Missing '{'",
                        tokens[i].line,
                        tokens[i].column
                    ).with_file(file_path).panic();
                }
                
                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(while_init)));

                let code_block_token: Token = Token{token_type: TokenType::CodeBlock, value: "".to_string(), line: tokens[i].line, column: tokens[i].column};
                let code_block: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(&code_block_token))));

                let code_block_clone: Rc<RefCell<Box<ASTNode>>> = Rc::clone(&code_block);

                current_parent_.borrow_mut().children.push(code_block);

                current_parent_ = code_block_clone;

                continue;
            }
            
            if token.token_type == TokenType::KeywordFor {
                let mut for_init: Box<ASTNode> = Box::new(ASTNode::new(token));
                let mut condition_tokens: Vec<&Token> = Vec::new();
                
                let mut j: usize = i;
                while i < tokens.len() && tokens[j].token_type != TokenType::EOL {
                    condition_tokens.push(&tokens[j]);
                    j += 1;
                }
                
                if j < tokens.len() {
                    i = j;
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Unexpected EOF after for-condition",
                        token.line,
                        token.column
                    ).with_file(file_path).panic();
                }
                                
                for_init.children = generate_for_loop_condition_ast(condition_tokens, file_path, tokens[i].line);
                
                let last_child: &&Rc<RefCell<Box<ASTNode>>> = &for_init.children.last().unwrap();
                if (**last_child).borrow().token_type != TokenType::PunctuationBraceOpen {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Condition initialization: Missing '{'",
                        tokens[i].line,
                        tokens[i].column
                    ).with_file(file_path).panic();
                }

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(for_init)));

                let code_block_token: Token = Token{token_type: TokenType::CodeBlock, value: "".to_string(), line: tokens[i].line, column: tokens[i].column};
                let code_block: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(&code_block_token))));

                let code_block_clone: Rc<RefCell<Box<ASTNode>>> = Rc::clone(&code_block);

                current_parent_.borrow_mut().children.push(code_block);

                current_parent_ = code_block_clone;

                continue;
            }
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
            
            if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::OperatorIncrease || tokens[i + 1].token_type == TokenType::OperatorDecrease {
                let identifier_init: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(token))));
                
                identifier_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i + 1])))));
                
                i += 2;
                
                if i < tokens.len() && tokens[i].token_type == TokenType::OperatorSemicolon {
                    identifier_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i])))));
                    current_parent_.borrow_mut().children.push(identifier_init);
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

fn generate_for_loop_condition_ast(tokens: Vec<&Token>, file_path: &str, line: u32) -> Vec<Rc<RefCell<Box<ASTNode>>>> {
    let mut condition_ast: Vec<Rc<RefCell<Box<ASTNode>>>> = Vec::new();
    
    if tokens.is_empty() {
      let _err = Err::new(
          ErrorType::Syntax,
          "Missing condtion",
          line,
           0
      ).with_file(file_path).panic();
    }
    
    let mut prev_token: &Token = tokens[0];
    
    for token in tokens {
        if prev_token.token_type == TokenType::Identifier && token.token_type == TokenType::OperatorIncrease || token.token_type == TokenType::OperatorDecrease {
            let node: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(prev_token))));
            node.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token)))));
            condition_ast.push(node);
        } else {
            let node: Rc<RefCell<Box<ASTNode>>> = Rc::new(RefCell::new(Box::new(ASTNode::new(token))));
            condition_ast.push(node);
        }
        
        prev_token = token;
        
    }
    
    condition_ast
}

/// Generates an Abstract Syntax Tree (AST) from a condition.
///
/// # Parameters
///
/// - `tokens`: A vector of references to `Token`s representing the condition.
/// - `file_path`: The path of the file the condition is in.
///
/// # Returns
///
/// A vector of `Rc<RefCell<Box<ASTNode>>>` representing the condition in the AST.
///
/// # Errors
///
/// If `tokens` is empty, a syntax error is triggered, indicating that the condition is missing.
fn generate_condition_ast(tokens: Vec<&Token>, file_path: &str) -> Vec<Rc<RefCell<Box<ASTNode>>>> {
    let mut condition_ast: Vec<Rc<RefCell<Box<ASTNode>>>> = Vec::new();

    if tokens.is_empty() {
        let _err = Err::new(
            ErrorType::Syntax,
            "Missing condition",
            0,
            0
        ).with_file(file_path).panic();

        unreachable!();
    }

    for token in tokens {
        condition_ast.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token)))));
    }

    condition_ast
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

/// Generates an Abstract Syntax Tree (AST) from a function initialization.
///
/// # Parameters
///
/// - `tokens`: A vector of references to `Token`s representing the function initialization.
/// - `file_path`: The path of the file the function initialization is in.
///
/// # Returns
///
/// A vector of `Rc<RefCell<Box<ASTNode>>>` representing the function initialization in the AST.
///
/// # Errors
///
/// If `tokens` is empty, a syntax error is triggered, indicating that the function initialization is empty.
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

/// Pops the current parent node from the AST scope stack.
///
/// # Parameters
///
/// - `root`: The root node of the AST.
/// - `current_parent`: The current parent node of the scope stack.
/// - `file_path`: The path of the file the AST is from.
///
/// # Returns
///
/// An `Option` containing the parent node of `current_parent` if found. If `current_parent` is the root node, returns `None`.
///
/// # Errors
///
/// Prints an error message if `current_parent` is the root node, and if the parent of `current_parent` is not found.
fn pop_current_parent(root: &Rc<RefCell<Box<ASTNode>>>, current_parent: &Rc<RefCell<Box<ASTNode>>>, file_path: &str) -> Rc<RefCell<Box<ASTNode>>> {
    if Rc::ptr_eq(root, current_parent) {
        let _err = Err::new(
            ErrorType::Syntax,
            "You cannot change the scope on root level!",
            0,
            0
        ).with_file(file_path).panic();

        unreachable!();
    }

    // Check if current_parent is empty
    let current_borrow = current_parent.try_borrow();
    let _current_borrow = match current_borrow {
        Ok(val) => val,
        Err(_) => {
            let _err = Err::new(
                ErrorType::Other,
                "Internal Error: Failed to borrow current_parent!",
                0,
                0
            ).with_file(file_path).panic();

            unreachable!();
        }
    };

    if let Some(found) = find_parent_of_current_parent(&root, &current_parent) {
        found
    } else {
        let _err = Err::new(
            ErrorType::Other,
            "Internal Error: Failed to find parent of current_parent!",
            0,
            0
        ).with_file(file_path).panic();

        unreachable!();
    }
}

/// Finds the parent node of the specified current parent node in the AST.
///
/// # Parameters
///
/// - `current`: A reference to the current `ASTNode` wrapped in `Rc<RefCell<Box<ASTNode>>>`.
/// - `current_parent`: A reference to the current parent `ASTNode` wrapped in `Rc<RefCell<Box<ASTNode>>>`.
///
/// # Returns
///
/// Returns an `Option` containing the parent node of `current_parent` if found, wrapped in `Rc<RefCell<Box<ASTNode>>>`.
/// If the parent is not found, returns `None`.
///
/// This function recursively traverses the children of the `current` node to locate the `current_parent` node and its
/// corresponding parent. If `current_parent` is found among the children of `current`, the function returns `current` as
/// the parent. Otherwise, it continues searching through the child nodes.
fn find_parent_of_current_parent(current: &Rc<RefCell<Box<ASTNode>>>, current_parent: &Rc<RefCell<Box<ASTNode>>>) -> Option<Rc<RefCell<Box<ASTNode>>>> {
    let current_borrow = current.borrow();

    for child in &current_borrow.children {
        if Rc::ptr_eq(child, current_parent) {
            return Some(Rc::clone(current));
        }

        if let Some(found) = find_parent_of_current_parent(child, current_parent) {
            return Some(found);
        }
    }

    None
}