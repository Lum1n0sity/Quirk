use std::fmt::Formatter;

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

    pub fn panic(&self) {
        panic!("{}", self);
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {} at line {} column {}", self.error_type, self.message, self.line, self.column)
    }
}