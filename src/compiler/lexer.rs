use std::fmt::Display;

pub fn parse_tokens<'a>(text: &str) -> Vec<Token> {
    let token_parsers = [
        parse_number,
        parse_string,
        parse_operator,
        parse_parenthesis,
        parse_illegal,
    ];
    let mut lexer = LexerData { text, position: 0 };
    let mut tokens = vec![];

    while !lexer.eof() {
        skip_whitespace(&mut lexer);

        if lexer.eof() {
            break;
        }

        for parse_token in token_parsers {
            if let Some(token) = parse_token(&mut lexer) {
                tokens.push(token);
                break;
            }
        }
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        info: TokenInfo {
            length: 0,
            location: lexer.position,
        },
    });

    tokens
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub info: TokenInfo,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Illegal(char),
    Eof,
    Int(i64),
    String(Box<str>),
    Operator(Operator),
    LParen,
    RParen,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Illegal(c) => format!("'{}'", c),
            Self::Eof => "EOF".into(),
            Self::Int(i) => i.to_string(),
            Self::String(s) => format!(r#""{}""#, s),
            Self::Operator(op) => format!("{}", op),
            Self::LParen => "(".into(),
            Self::RParen => ")".into(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    More,
    MoreOrEqual,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::Less => "<",
            Self::LessOrEqual => "<=",
            Self::More => ">",
            Self::MoreOrEqual => ">=",
        };

        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct TokenInfo {
    pub length: usize,
    pub location: usize,
}

fn skip_whitespace(lexer: &mut LexerData) {
    while !lexer.eof() && lexer.current().is_whitespace() {
        lexer.pop();
    }
}

fn parse_number(lexer: &mut LexerData) -> Option<Token> {
    let mut number_as_str: String = "".into();
    let position = lexer.position;

    while !lexer.eof() && lexer.current().is_ascii_digit() {
        number_as_str.push(lexer.pop());
    }

    if !number_as_str.is_empty() {
        let number = number_as_str.parse::<i64>().unwrap();

        return Some(Token {
            kind: TokenKind::Int(number),
            info: TokenInfo {
                length: number_as_str.len(),
                location: position,
            },
        });
    }

    None
}

fn parse_string(lexer: &mut LexerData) -> Option<Token> {
    if lexer.eof() || lexer.current() != '"' {
        return None;
    }

    let position = lexer.position;
    let mut string = String::new();

    lexer.pop(); // pops opening "
    while !lexer.eof() && lexer.current() != '"' {
        string.push(lexer.pop());
    }
    lexer.pop(); // pops closing "

    let length = string.len() + 2;

    Some(Token {
        kind: TokenKind::String(string.into_boxed_str()),
        info: TokenInfo {
            length,
            location: position,
        },
    })
}

fn parse_operator(lexer: &mut LexerData) -> Option<Token> {
    let operators = [
        ("+", Operator::Plus),
        ("-", Operator::Minus),
        ("*", Operator::Star),
        ("/", Operator::Slash),
        ("==", Operator::Equal),
        ("!=", Operator::NotEqual),
        ("<", Operator::Less),
        ("<=", Operator::LessOrEqual),
        (">", Operator::More),
        (">=", Operator::More),
    ];

    let position = lexer.position;
    let mut operator = None;
    let mut length: usize = 0;

    for op in operators {
        let enough_chars = lexer.text.len() >= op.0.len();
        if !enough_chars {
            continue;
        }

        let bigger_then_previous = op.0.len() > length;
        let operators_are_equal = &lexer.text[..op.0.len()] == op.0;
        if operators_are_equal && bigger_then_previous {
            operator = Some(op.1);
            length = op.0.len()
        }
    }
    let operator = operator?;

    for _ in 0..length {
        lexer.pop();
    }

    Some(Token {
        kind: TokenKind::Operator(operator),
        info: TokenInfo {
            length,
            location: position,
        },
    })
}

fn parse_parenthesis(lexer: &mut LexerData) -> Option<Token> {
    let location = lexer.position;

    let kind = match lexer.current() {
        '(' => Some(TokenKind::LParen),
        ')' => Some(TokenKind::RParen),
        _ => None,
    }?;

    lexer.pop();

    Some(Token {
        kind,
        info: TokenInfo {
            length: 1,
            location,
        },
    })
}

fn parse_illegal(lexer: &mut LexerData) -> Option<Token> {
    let position = lexer.position;
    Some(Token {
        kind: TokenKind::Illegal(lexer.pop()),
        info: TokenInfo {
            length: 1,
            location: position,
        },
    })
}

struct LexerData<'a> {
    text: &'a str,
    position: usize,
}

impl<'a> LexerData<'a> {
    fn current(&self) -> char {
        self.text.chars().next().unwrap()
    }

    fn eof(&self) -> bool {
        self.text.is_empty()
    }

    fn pop(&mut self) -> char {
        let at = self.current();
        self.text = &self.text[1..];

        self.position += 1;

        at
    }
}

mod tests {
    #[cfg(test)]
    use super::*;

    #[test]
    fn test() {
        let text = r#"123- "abc" "#;
        let expected = vec![
            Token {
                kind: TokenKind::Int(123),
                info: TokenInfo {
                    length: 3,
                    location: 0,
                },
            },
            Token {
                kind: TokenKind::Operator(Operator::Minus),
                info: TokenInfo {
                    length: 1,
                    location: 3,
                },
            },
            Token {
                kind: TokenKind::String("abc".into()),
                info: TokenInfo {
                    length: 5,
                    location: 5,
                },
            },
            Token {
                kind: TokenKind::Eof,
                info: TokenInfo {
                    length: 0,
                    location: 11,
                },
            },
        ];

        let tokens = parse_tokens(text);

        assert_eq!(tokens, expected)
    }
}
