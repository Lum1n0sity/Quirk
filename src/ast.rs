use crate::lexer::Token;


#[derive(Debug, PartialEq, Clone)]
pub struct ASTNode {
    pub token_type: String,
    pub value: String
}

impl ASTNode {
    pub fn new(token: &Token) -> Self {
        ASTNode {
            token_type: format!("{:?}", token.token_type),
            value: token.value.clone()
        }
    }
}

pub fn generate_ast(tokens: Vec<Token>) -> Vec<ASTNode> {
    let mut ast_nodes = Vec::new();

    // TODO: Implement AST generation logic

    ast_nodes
}