use std::fmt::Display;

pub fn parse_tokens(text: &str) -> Vec<Token> {
    let token_parsers = [
        parse_keyword_or_identifier,
        parse_number,
        parse_string,
        parse_operator,
        parse_punctuation,
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
        info: Info {
            length: 0,
            position: lexer.position,
        },
    });

    tokens
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub info: Info,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Illegal(char),
    Eof,
    Operator(Operator),
    Punctuation(Punctuation),
    Keyword(Keyword),
    Identifier(Box<str>),
    Bool(bool),
    Int(i64),
    String(Box<str>),
    LParen,
    RParen,
    LSquirly,
    RSquirly,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Illegal(c) => format!("{}", c),
            Self::Eof => "EOF".into(),
            Self::Int(i) => i.to_string(),
            Self::String(s) => format!(r#""{}""#, s),
            Self::Operator(op) => format!("{}", op),
            Self::Punctuation(x) => {
                let mut s = None;
                for p in PUNCTUATION {
                    if p.1 == *x {
                        s = Some(p.0);
                        break;
                    }
                }

                s.expect("Unexpected punctuation").into()
            }
            Self::Keyword(keyword) => format!("{}", keyword),
            Self::Identifier(iden) => {
                if iden.is_empty() {
                    "IDENTIFIER".to_string()
                } else {
                    iden.to_string()
                }
            }
            Self::Bool(b) => (if *b { "true" } else { "false" }).into(),
            Self::LParen => "(".into(),
            Self::RParen => ")".into(),
            Self::LSquirly => "{".into(),
            Self::RSquirly => "}".into(),
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
    Not,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    More,
    MoreOrEqual,
    Assignment,
    Arrow,
}

const PUNCTUATION: &[(char, Punctuation)] = &[
    (';', Punctuation::Semicolon),
    (':', Punctuation::Colon),
    (',', Punctuation::Comma),
];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Punctuation {
    Semicolon,
    Colon,
    Comma,
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
            Self::Assignment => "=",
            Self::Not => "!",
            Self::Arrow => "->",
        };

        write!(f, "{}", s)
    }
}

const KEYWORDS: &[(&str, Keyword)] = &[
    ("let", Keyword::Let),
    ("fn", Keyword::Fn),
    ("if", Keyword::If),
    ("else", Keyword::Else),
];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    Let,
    Fn,
    If,
    Else,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for keyword in KEYWORDS {
            if keyword.1 == *self {
                return write!(f, "{}", keyword.0);
            }
        }

        panic!("keyword not found")
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Info {
    pub length: usize,
    pub position: usize,
}

fn skip_whitespace(lexer: &mut LexerData) {
    while !lexer.eof() && lexer.current().is_whitespace() {
        lexer.pop();
    }
}

fn parse_keyword_or_identifier(lexer: &mut LexerData) -> Option<Token> {
    if !lexer.current().is_alphabetic() && lexer.current() != '_' {
        return None;
    }

    let mut text = "".to_string();
    let position = lexer.position;

    while !lexer.eof() && (lexer.current().is_alphanumeric() || lexer.current() == '_') {
        text.push(lexer.pop());
    }

    if text.is_empty() {
        return None;
    }

    let mut token_kind = None;

    for keyword in KEYWORDS {
        if text == "true" || text == "false" {
            token_kind = Some(TokenKind::Bool(text == "true"));
            break;
        }

        if keyword.0 == text {
            token_kind = Some(TokenKind::Keyword(keyword.1));
            break;
        }
    }

    let length = text.len();
    let token_kind = token_kind.unwrap_or(TokenKind::Identifier(text.into()));

    Some(Token {
        kind: token_kind,
        info: Info { length, position },
    })
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
            info: Info {
                length: number_as_str.len(),
                position,
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
        info: Info { length, position },
    })
}

fn parse_operator(lexer: &mut LexerData) -> Option<Token> {
    let operators = [
        ("!", Operator::Not),
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
        ("=", Operator::Assignment),
        ("->", Operator::Arrow),
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
        info: Info { length, position },
    })
}

fn parse_punctuation(lexer: &mut LexerData) -> Option<Token> {
    for c in PUNCTUATION {
        if c.0 == lexer.current() {
            lexer.pop();
            return Some(Token {
                kind: TokenKind::Punctuation(c.1),
                info: Info {
                    length: 1,
                    position: lexer.position,
                },
            });
        }
    }

    None
}

fn parse_parenthesis(lexer: &mut LexerData) -> Option<Token> {
    let location = lexer.position;

    let kind = match lexer.current() {
        '(' => Some(TokenKind::LParen),
        ')' => Some(TokenKind::RParen),
        '{' => Some(TokenKind::LSquirly),
        '}' => Some(TokenKind::RSquirly),
        _ => None,
    }?;

    lexer.pop();

    Some(Token {
        kind,
        info: Info {
            length: 1,
            position: location,
        },
    })
}

fn parse_illegal(lexer: &mut LexerData) -> Option<Token> {
    let position = lexer.position;
    Some(Token {
        kind: TokenKind::Illegal(lexer.pop()),
        info: Info {
            length: 1,
            position,
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
    fn text_nums_and_strs() {
        let text = r#"123- "abc" "#;
        let expected = vec![
            Token {
                kind: TokenKind::Int(123),
                info: Info {
                    length: 3,
                    position: 0,
                },
            },
            Token {
                kind: TokenKind::Operator(Operator::Minus),
                info: Info {
                    length: 1,
                    position: 3,
                },
            },
            Token {
                kind: TokenKind::String("abc".into()),
                info: Info {
                    length: 5,
                    position: 5,
                },
            },
            Token {
                kind: TokenKind::Eof,
                info: Info {
                    length: 0,
                    position: 11,
                },
            },
        ];

        let tokens = parse_tokens(text);

        assert_eq!(tokens, expected)
    }

    #[test]
    fn test_keywords() {
        let text = r#"let hello = 1"#;
        let expected = [
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("hello".into()),
            TokenKind::Operator(Operator::Assignment),
            TokenKind::Int(1),
            TokenKind::Eof,
        ];

        let tokens: Vec<TokenKind> = parse_tokens(text).iter().map(|x| x.kind.clone()).collect();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn text_identifiers() {
        let text = r#"let a _a a_a 1a a1"#;
        let expected = [
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("a".into()),
            TokenKind::Identifier("_a".into()),
            TokenKind::Identifier("a_a".into()),
            TokenKind::Int(1),
            TokenKind::Identifier("a".into()),
            TokenKind::Identifier("a1".into()),
            TokenKind::Eof,
        ];

        let tokens: Vec<TokenKind> = parse_tokens(text).iter().map(|x| x.kind.clone()).collect();

        assert_eq!(tokens, expected);
    }
}
