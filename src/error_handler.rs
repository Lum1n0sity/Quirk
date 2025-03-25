pub struct Error {
    pub error_type: String,
    pub message: String,
    pub line: u32,
    pub column: u32
}

impl Error {
    pub fn new(error_type: String, message: String, line: u32, column: u32) -> Self {
        Error {
            error_type,
            message,
            line,
            column
        }
    }

    pub fn display_error(error: Error) -> () {
        panic!("{}: {} at line {} column {}", error.error_type, error.message, error.line, error.column);
    }
}