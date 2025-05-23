#![allow(unused_assignments)]
#![allow(unused_doc_comments)]

use crate::lexer::{Token, TokenType};
use crate::error_handler::*;

use regex::Regex;
use std::rc::Rc;
use std::cell::RefCell;
use once_cell::sync::OnceCell;

static FILE_PATH: OnceCell<String> = OnceCell::new();

fn get_file_path() -> &'static str {
    FILE_PATH.get().expect("file path not set").as_str()
}

type Node = Rc<RefCell<Box<ASTNode>>>;

/// Represents a node in the Abstract Syntax Tree
#[derive(Debug, PartialEq, Clone)]
pub struct ASTNode {
    pub token_type: TokenType,
    pub value: String,
    pub children: Vec<Node>,
    pub line: u32,
    pub file: &'static str
}

/// Implementation of core functionality for ASTNode
impl ASTNode {
    /// Creates a new ASTNode from a given token
    ///
    /// # Arguments
    /// * `token` - The token to create the node from
    ///
    /// # Returns
    /// A new ASTNode instance with the token's type and value
    pub fn new(token: &Token, file_path: &'static str) -> Self {
        ASTNode {
            token_type: token.token_type.clone(),
            value: token.value.clone(),
            children: Vec::new(),
            line: token.line,
            file: file_path
        }
    }
}

pub fn generate_ast(tokens: Vec<Token>, file_path: &str) -> Box<ASTNode> {
    FILE_PATH.set(file_path.into()).expect("file path already set");

    let root_token: &Token = &Token {
        token_type: TokenType::Root,
        value: "".to_string(),
        line: 0,
        column: 0
    };

    let root: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(root_token, get_file_path()))));
    let mut current_parent_: Node = Rc::clone(&root);

    let mut i: usize = 0;

    /// Checks if the token at index `i` matches the expected `TokenType`.
    /// If so, adds it to `node_to_add`, then appends `node_to_add` to the `current_parent` node.
    /// Otherwise, panics with a syntax error.
    ///
    /// # Arguments
    /// * `i` - Current index in the token list.
    /// * `tokens` - Vector of all tokens.
    /// * `expected` - The expected `TokenType` to match.
    /// * `node_to_add` - AST node to which the token will be added as a child.
    /// * `current_parent` - The current parent node in the AST.
    /// * `error_msg` - Error message to display if the token doesn't match.
    let expect_token = |i: usize, tokens: &Vec<Token>, expected: TokenType, node_to_add: Node, current_parent: &Node, error_msg: &str| {
        if i < tokens.len() && tokens[i].token_type == expected {
            node_to_add.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i], get_file_path())))));
            current_parent.borrow_mut().children.push(node_to_add);
        } else {
            let token = &tokens[i.min(tokens.len().saturating_sub(1))];
            let _err = Err::new(
                ErrorType::Syntax,
                error_msg,
                token.line,
                token.column,
                get_file_path()
            ).panic();
        }
    };

    /// Checks whether the last child of the given AST node matches the expected `TokenType`.
    /// If not, panics with a syntax error.
    ///
    /// # Arguments
    /// * `node_init` - The AST node whose last child will be checked.
    /// * `expected` - The expected `TokenType` for the last child.
    /// * `error_msg` - Error message to display if the check fails.
    /// * `line` - Line number for error reporting.
    /// * `column` - Column number for error reporting.
    let check_last_child = |node_init: &Box<ASTNode>, expected: TokenType, error_msg: &str, line: u32, column: u32| {
        let last_child: &&Node = &node_init.children.last().unwrap();
        if (**last_child).borrow().token_type != expected {
            let _err = Err::new(
                ErrorType::Syntax,
                error_msg,
                line,
                column,
                get_file_path()
            ).panic();
        }
    };

    /// Creates a new `CodeBlock` AST node at the given token index and adds it as a child
    /// to the provided `current_parent`. Returns the newly created code block node to become
    /// the new parent for subsequent child nodes.
    ///
    /// # Arguments
    /// * `i` - Index of the current token used to create the `CodeBlock`.
    /// * `current_parent` - The current parent node to which the code block will be added.
    ///
    /// # Returns
    /// A new `Node` representing the generated `CodeBlock`, to be used as the new parent node.
    let generate_code_block = |i: usize, current_parent: &Node| -> Node {
        let code_block_token: Token = Token{token_type: TokenType::CodeBlock, value: "".to_string(), line: tokens[i].line, column: tokens[i].column};
        let code_block: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(&code_block_token, get_file_path()))));

        let code_block_clone: Node = Rc::clone(&code_block);

        current_parent.borrow_mut().children.push(code_block);

        println!("{:?}", code_block_clone);

        code_block_clone
    };

    while i < tokens.len() {
        let token: &Token = &tokens[i];

        // Update scope if token is a }
        if token.token_type == TokenType::PunctuationBraceClose {
            let new_current_parent = pop_current_parent(&root, &current_parent_);
            current_parent_ = new_current_parent;
            i += 1;
            continue;
        }

        // Check if token.token_type starts with Keyword
        let current_token_type_str = format!("{:?}", token.token_type);
        let keyword_regex = Regex::new(r"^Keyword.*").unwrap();

        if keyword_regex.is_match(&current_token_type_str) {
            if token.token_type == TokenType::KeywordReturn{
                let node_init: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path()))));

                let (node_tokens, new_i) = get_tokens_until(&i, &tokens, &[TokenType::OperatorSemicolon, TokenType::EOL], "Unexpected EOF after 'return'");
                i = new_i;

                for token in node_tokens {
                    node_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path())))));
                }

                expect_token(i, &tokens, TokenType::OperatorSemicolon, node_init, &current_parent_, "Missing token ';'");

                continue;
            }

            if token.token_type == TokenType::KeywordLet || token.token_type == TokenType::KeywordThis {
                let mut error_msg_tokens: &str = "";
                match token.token_type {
                    TokenType::KeywordLet => error_msg_tokens = "Unexpected EOF after 'let'",
                    TokenType::KeywordThis => error_msg_tokens = "Unexpected EOF after 'this'",
                    _ => error_msg_tokens = "Unexpected EOF"
                }

                let mut error_msg_ast: &str = "";
                match token.token_type {
                    TokenType::KeywordLet => error_msg_ast = "Variable declaration is empty!",
                    TokenType::KeywordThis => error_msg_ast = "'this' is empty!",
                    _ => error_msg_ast = "Invalid input!"
                }

                let mut node_init: Box<ASTNode> = Box::new(ASTNode::new(token, get_file_path()));

                let (node_tokens, new_i) = get_tokens_until(&i, &tokens, &[TokenType::OperatorSemicolon, TokenType::EOL], error_msg_tokens);
                i = new_i;

                node_init.children = generate_ast_from_tokens(node_tokens, error_msg_ast);

                expect_token(i, &tokens, TokenType::OperatorSemicolon, Rc::new(RefCell::new(node_init)), &current_parent_, "Missing token ';'")
            }

            if token.token_type == TokenType::KeywordFn || token.token_type == TokenType::KeywordIf || token.token_type == TokenType::KeywordElIf || token.token_type == TokenType::KeywordWhile || token.token_type == TokenType::KeywordClass {
                let mut error_msg_tokens: &str = "";
                match token.token_type {
                    TokenType::KeywordFn => error_msg_tokens = "Unexpected EOF after 'fn'",
                    TokenType::KeywordIf => error_msg_tokens = "Unexpected EOF after 'if'",
                    TokenType::KeywordElIf => error_msg_tokens = "Unexpected EOF after 'elif'",
                    TokenType::KeywordWhile => error_msg_tokens = "Unexpected EOF after 'while'",
                    TokenType::KeywordClass => error_msg_tokens = "Unexpected EOF after 'class'",
                    _ => error_msg_tokens = "Unexpected EOF"
                }

                let mut error_msg_ast: &str = "";
                match token.token_type {
                    TokenType::KeywordFn => error_msg_ast = "Function declaration is empty!",
                    TokenType::KeywordIf => error_msg_ast = "If condition is empty!",
                    TokenType::KeywordElIf => error_msg_ast = "Else if condition is empty!",
                    TokenType::KeywordWhile => error_msg_ast = "While condition is empty!",
                    TokenType::KeywordClass => error_msg_ast = "Class declaration is empty!",
                    _ => error_msg_ast = "Invalid input!"
                }

                let mut node_init: Box<ASTNode> = Box::new(ASTNode::new(token, get_file_path()));

                let (node_tokens, new_i) = get_tokens_until(&i, &tokens, &[TokenType::EOL], error_msg_tokens);
                i = new_i;

                node_init.children = generate_ast_from_tokens(node_tokens, error_msg_ast);

                check_last_child(&node_init, TokenType::PunctuationBraceOpen, "Missing token '{'", tokens[i].line, tokens[i].column);

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(node_init)));

                current_parent_ = generate_code_block(i, &current_parent_);

                continue;
            }

            if token.token_type == TokenType::KeywordFor {
                let mut for_init: Box<ASTNode> = Box::new(ASTNode::new(token, get_file_path()));

                let (condition_tokens, new_i) = get_tokens_until(&i, &tokens, &[TokenType::EOL], "Unexpected EOF after condition");
                i = new_i;

                for_init.children = generate_for_loop_condition_ast(condition_tokens, tokens[i].line);

                check_last_child(&for_init, TokenType::PunctuationBraceOpen, "Missing token '{'", tokens[i].line, tokens[i].column);

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(for_init)));

                current_parent_ = generate_code_block(i, &current_parent_);

                continue;
            }

            if token.token_type == TokenType::KeywordElse {
                let mut else_init:Box<ASTNode> = Box::new(ASTNode::new(token, get_file_path()));

                if i + 1 >= tokens.len() || tokens[i + 1].token_type != TokenType::PunctuationBraceOpen {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Missing token '{'",
                        tokens[i].line,
                        tokens[i].column,
                        get_file_path()
                    ).panic();
                }

                else_init.children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i + 1], get_file_path())))));
                i += 1;

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(else_init)));

                current_parent_ = generate_code_block(i, &current_parent_);

                continue;
            }

            if token.token_type == TokenType::KeywordBreak || token.token_type == TokenType::KeywordContinue {
                let node: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path()))));
                i += 1;

                expect_token(i, &tokens, TokenType::OperatorSemicolon, node, &current_parent_, "Missing token ';'");
            }

            if token.token_type == TokenType::KeywordConstruct {
                let mut construct_init: Box<ASTNode> = Box::new(ASTNode::new(token, get_file_path()));
                let mut construct_tokens: Vec<&Token> = Vec::new();

                let mut is_new_object: bool = false;

                let mut j: usize = i + 1;
                while j < tokens.len() && tokens[j].token_type != TokenType::EOL {
                    construct_tokens.push(&tokens[j]);
                    j += 1;

                    if j < tokens.len() && tokens[j].token_type == TokenType::OperatorSemicolon {
                        is_new_object = true;
                        break;
                    }
                }

                if j < tokens.len() {
                    i = j;
                } else {
                    let _err = Err::new(
                        ErrorType::Syntax,
                        "Unexpected EOF after 'construct'",
                        token.line,
                        token.column,
                        get_file_path()
                    ).panic();
                }

                for token in construct_tokens {
                    construct_init.children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path())))));
                }

                if !is_new_object {
                    check_last_child(&construct_init, TokenType::PunctuationBraceOpen, "Missing token '{'", tokens[i].line, tokens[i].column);

                    current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(construct_init)));

                    current_parent_ = generate_code_block(i, &current_parent_);
                } else {
                    check_last_child(&construct_init, TokenType::OperatorSemicolon, "Missing token ';'", tokens[i].line, tokens[i].column);

                    current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(construct_init)));
                }

                continue;
            }

            if token.token_type == TokenType::KeywordEnum || token.token_type == TokenType::KeywordStruct {
                let mut enum_init: Box<ASTNode> = Box::new(ASTNode::new(token, get_file_path()));

                let (enum_tokens, new_i) = get_tokens_until(&i, &tokens, &[TokenType::EOL], "Unexpected EOF after 'enum'");
                i = new_i;

                enum_init.children = generate_ast_from_tokens(enum_tokens, "Enum is empty!");

                check_last_child(&enum_init, TokenType::PunctuationBraceOpen, "Missing token '{'", tokens[i].line, tokens[i].column);

                current_parent_.borrow_mut().children.push(Rc::new(RefCell::new(enum_init)));

                current_parent_ = generate_code_block(i, &current_parent_);

                let (enum_items_tokens, new_i) = get_tokens_until(&i, &tokens, &[TokenType::PunctuationBraceClose], "Unexpected EOF after enum items");
                i = new_i - 1;

                let enum_items_nodes = generate_ast_from_tokens(enum_items_tokens, "Enum items are empty!");

                for node in enum_items_nodes {
                    current_parent_.borrow_mut().children.push(node);
                }
            }
        }

        if token.token_type == TokenType::Identifier{
            if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::OperatorAssign {
                let identifier_init: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path()))));

                let (identifier_tokens, new_i) = get_tokens_until(&i, &tokens, &[TokenType::OperatorSemicolon, TokenType::EOL], "Unexpected EOF after assignment");
                i = new_i;

                identifier_init.borrow_mut().children = generate_ast_from_tokens(identifier_tokens, "Variable assignment is empty!");

                expect_token(i, &tokens, TokenType::OperatorSemicolon, identifier_init, &current_parent_, "Missing token ';'");

                continue;
            }

            if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::OperatorIncrease || tokens[i + 1].token_type == TokenType::OperatorDecrease {
                let identifier_init: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path()))));

                identifier_init.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i + 1], get_file_path())))));

                i += 2;

                expect_token(i, &tokens, TokenType::OperatorSemicolon, identifier_init, &current_parent_, "Missing token ';'");

                continue;
            }

            if i + 1 < tokens.len() && tokens[i + 1].token_type == TokenType::PunctuationParenOpen {
                let (function_call, new_i) = generate_ast_function_call(&tokens, i);

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
            "Failed to unwrap root ast node",
            0,
            0,
            get_file_path()
        ).panic();

        unreachable!();
    }
}

/// Attempts to pop the current parent node, returning the parent of the current_parent.
///
/// # Arguments
/// * `root` - The root node of the AST
/// * `current_parent` - The current parent node to pop
///
/// # Panics
/// * If attempting to change scope at root level
/// * If unable to borrow current_parent
/// * If unable to find parent of current_parent
fn pop_current_parent(root: &Node, current_parent: &Node) -> Node {
    if Rc::ptr_eq(root, current_parent) {
        let _err = Err::new(
            ErrorType::Other,
            "You cannot change the scope on root level",
            0,
            0,
            get_file_path()
        ).panic();

        unreachable!();
    }

    // Check if current_parent is empty
    let current_borrow = current_parent.try_borrow();
    let _current_borrow = match current_borrow {
        Ok(val) => val,
        Err(_) => {
            let _err = Err::new(
                ErrorType::Other,
                "Failed to borrow current_parent",
                0,
                0,
                get_file_path()
            ).panic();

            unreachable!();
        }
    };

    if let Some(found) = find_parent_of_current_parent(&root, &current_parent) {
        found
    } else {
        let _err = Err::new(
            ErrorType::Other,
            "Failed to find parent of current_parent",
            0,
            0,
            get_file_path()
        ).panic();

        unreachable!();
    }
}

/// Recursively searches for the parent node of the given current_parent node.
///
/// # Arguments
/// * `current` - The current node to search in
/// * `current_parent` - The node whose parent we're looking for
///
/// # Returns
/// * `Option<Node>` - The parent node if found, None otherwise
fn find_parent_of_current_parent(current: &Node, current_parent: &Node) -> Option<Node> {
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

/// Checks tokens until a token of an allowed type is encountered.
///
/// # Arguments
/// * `i` - Current token index
/// * `tokens` - Vector of tokens to check
/// * `allowed_types` - Slice of allowed token types to stop at
/// * `error_msg` - Error message to display if end of tokens is reached
///
/// # Returns
/// * Tuple containing collected tokens and the new index position
///
/// # Panics
/// * If end of tokens is reached before finding an allowed token type
fn get_tokens_until<'a>(i: &usize, tokens: &'a Vec<Token>, allowed_types: &'a [TokenType], error_msg: &str) -> (Vec<&'a Token>, usize) {
    let mut output_tokens: Vec<&Token> = Vec::new();

    let mut j: usize = *i + 1;

    while j < tokens.len() && !allowed_types.contains(&tokens[j].token_type) {
        output_tokens.push(&tokens[j]);
        j += 1;
    }

    if j < tokens.len() {
        output_tokens.retain(|token| token.token_type != TokenType::EOL);

        (output_tokens, j)
    } else {
        let _err = Err::new(
            ErrorType::Syntax,
            error_msg,
            tokens[*i].line,
            tokens[*i].column,
            get_file_path()
        ).panic();

        unreachable!();
    }
}

/// Generates AST nodes for a for-loop condition.
///
/// # Arguments
/// * `tokens` - Vector of tokens representing the loop condition
/// * `line` - Line number for error reporting
///
/// # Returns
/// * Vector of nodes representing the loop condition AST
///
/// # Panics
/// * If the condition tokens are empty
fn generate_for_loop_condition_ast(tokens: Vec<&Token>, line: u32) -> Vec<Node> {
    let mut condition_ast: Vec<Node> = Vec::new();

    if tokens.is_empty() {
      let _err = Err::new(
          ErrorType::Syntax,
          "Missing condition",
          line, 
          0,
          get_file_path()
      ).panic();
    }

    let mut prev_token: &Token = tokens[0];

    for token in tokens {
        if prev_token.token_type == TokenType::Identifier && (token.token_type == TokenType::OperatorIncrease || token.token_type == TokenType::OperatorDecrease) {
            let node: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(prev_token, get_file_path()))));
            node.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path())))));
            condition_ast.push(node);
        } else {
            let node: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path()))));
            condition_ast.push(node);
        }

        prev_token = token;

    }

    condition_ast
}

/// Generates an AST node for a function call, including its arguments.
///
/// # Arguments
/// * `tokens` - Vector of all tokens
/// * `i` - Current token index
///
/// # Returns
/// * Tuple containing the optional function call node and the new index position
///
/// # Panics
/// * If semicolon is missing after the function call
fn generate_ast_function_call(tokens: &Vec<Token>, i: usize) -> (Option<Node>, usize){
    let function_call: Node = Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[i], get_file_path()))));
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
        function_call.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(tokens, get_file_path())))));
    }

    if j < tokens.len() && tokens[j].token_type == TokenType::OperatorSemicolon {
        function_call.borrow_mut().children.push(Rc::new(RefCell::new(Box::new(ASTNode::new(&tokens[j], get_file_path())))));
        (Some(function_call), j)
    } else {
        let _err = Err::new(
            ErrorType::Syntax,
            "Missing token ';'",
            tokens[i].line,
            tokens[i].column,
            get_file_path()
        ).panic();

        (None, j)
    }
}

/// Generates AST nodes from a vector of tokens.
///
/// # Arguments
/// * `tokens` - Vector of tokens to convert to AST nodes
/// * `error_msg` - Error message to display if tokens vector is empty
///
/// # Returns
/// * Vector of AST nodes
///
/// # Panics
/// * If the tokens vector is empty
fn generate_ast_from_tokens(tokens: Vec<&Token>, error_msg: &str) -> Vec<Node> {
    let mut ast: Vec<Node> = Vec::new();

    if tokens.is_empty() {
        let _err = Err::new(
            ErrorType::Syntax,
            error_msg,
            0,
            0,
            get_file_path()
        ).panic();

        return ast;
    }

    for token in tokens {
        ast.push(Rc::new(RefCell::new(Box::new(ASTNode::new(token, get_file_path())))));
    }

    ast
}