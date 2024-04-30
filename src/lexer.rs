//! lexer module for kaleidscope.
use std::iter::Peekable;
use std::str::Chars;

/// An enum represinting token.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// End of File
    Eof,
    /// "def"
    Def,
    /// "extern"
    Extern,
    /// Other identifiers, i.e. "x", "i", "name"...
    Identifier(String),
    /// Floating point numbers, i.e. "1", "1.2", "0.5", ".6"...
    Number(f64),
    /// Other charcters, i.e. "+", "-"...
    Other(char),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Eof => write!(f, "EOF"),
            Token::Def => write!(f, "def"),
            Token::Extern => write!(f, "extern"),
            Token::Identifier(id) => write!(f, "identifier: {}", id),
            Token::Number(num) => write!(f, "number literal: {}", num),
            Token::Other(ch) => write!(f, "{}", ch),
        }
    }
}

/// Error struct.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Error {
    msg: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

/// A token iterator.
#[derive(Debug)]
pub struct TokenIter<'a, 'b: 'a> {
    chars: &'a mut Peekable<Chars<'b>>,
}

impl<'a, 'b: 'a> TokenIter<'a, 'b> {
    /// Create a new TokenIter.
    pub fn new(chars: &'a mut Peekable<Chars<'b>>) -> Self {
        Self { chars }
    }
}

impl<'a, 'b: 'a> Iterator for TokenIter<'a, 'b> {
    type Item = Result<Token, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.peek() {
            Some(_) => Some(get_token(&mut self.chars)),
            None => None,
        }
    }
}

/// Get next token of given chars.
///
/// # Exemples
///
/// ```
/// use rustyscope::lexer::{Token, get_token};
/// let mut chars = "def x = 1.2".chars().peekable();
/// assert_eq!(get_token(&mut chars).unwrap(), Token::Def);
/// assert_eq!(get_token(&mut chars).unwrap(), Token::Identifier("x".to_string()));
/// assert_eq!(get_token(&mut chars).unwrap(), Token::Other('='));
/// assert_eq!(get_token(&mut chars).unwrap(), Token::Number(1.2));
/// ```
pub fn get_token(chars: &mut Peekable<Chars>) -> Result<Token, Error> {
    skip_while(chars, |c| c.is_whitespace());

    match chars.peek() {
        None => Ok(Token::Eof),
        Some(&c) => {
            if c.is_alphabetic() {
                identifier(chars)
            } else if c.is_numeric() || c == '.' {
                number(chars)
            } else if c == '#' {
                skip_while(chars, |&c| c != '\n' && c != '\r');
                get_token(chars)
            } else {
                chars.next();
                Ok(Token::Other(c))
            }
        }
    }
}

fn skip_while<I, P>(iter: &mut Peekable<I>, predicate: P)
where
    I: Iterator,
    P: Fn(&I::Item) -> bool,
{
    while let Some(x) = iter.peek() {
        if !predicate(x) {
            return;
        }
        iter.next();
    }
}

fn take_string_while<P>(chars: &mut Peekable<Chars>, predicate: P) -> String
where
    P: Fn(&char) -> bool,
{
    let mut ret = String::new();
    while let Some(c) = chars.peek() {
        if predicate(c) {
            ret.push(*c);
            chars.next();
        } else {
            break;
        }
    }

    ret
}

fn is_identifier_char(ch: &char) -> bool {
    match ch {
        '_' => true,
        _ => ch.is_alphanumeric(),
    }
}

fn identifier(chars: &mut Peekable<Chars>) -> Result<Token, Error> {
    match chars.peek() {
        None => Err(Error {
            msg: format!("Unexpected end of file"),
        }),
        Some(c) => {
            if !c.is_alphabetic() {
                Err(Error {
                    msg: format!("Expected alphabetic character"),
                })
            } else {
                let pred = is_identifier_char;
                let id = take_string_while(chars, pred);

                match id.as_str() {
                    "" => unreachable!(),
                    "def" => Ok(Token::Def),
                    "extern" => Ok(Token::Extern),
                    _ => Ok(Token::Identifier(id)),
                }
            }
        }
    }
}

fn get_number_string(chars: &mut Peekable<Chars>) -> String {
    return take_string_while(chars, |x| x.is_numeric());
}

fn fraction(chars: &mut Peekable<Chars>) -> String {
    match chars.peek() {
        Some('.') => {
            chars.next();
            ".".to_string() + &get_number_string(chars)
        }
        _ => "".to_string(),
    }
}

fn number(chars: &mut Peekable<Chars>) -> Result<Token, Error> {
    match chars.peek() {
        None => Err(Error {
            msg: format!("Expected number literal"),
        }),
        Some(_) => {
            let frac = fraction(chars);
            if !frac.is_empty() {
                Ok(Token::Number(frac.parse::<f64>().unwrap()))
            } else {
                let decimal = get_number_string(chars);
                let frac = fraction(chars);
                let num = decimal + &frac;
                if !num.is_empty() {
                    Ok(Token::Number(
                        num.parse::<f64>().expect("Failed to parse number literal"),
                    ))
                } else {
                    Err(Error {
                        msg: format!("Expected number literal"),
                    })
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_while() {
        let mut chars = "  123".chars().peekable();
        skip_while(&mut chars, |c| c.is_whitespace());
        let mut chars = "  123   456".chars().peekable();
        skip_while(&mut chars, |c| c.is_whitespace());
        assert_eq!(chars.collect::<String>(), "123   456");
        let mut chars = "#   asdf2\n unchi".chars().peekable();
        skip_while(&mut chars, |c| *c != '\n');
        assert_eq!(chars.collect::<String>(), "\n unchi");
    }

    #[test]
    fn test_identifier() {
        let mut chars = "hello12_3".chars().peekable();
        assert_eq!(
            identifier(&mut chars).unwrap(),
            Token::Identifier("hello12_3".to_string())
        );
        let mut chars = "hello12-3".chars().peekable();
        assert_eq!(
            identifier(&mut chars).unwrap(),
            Token::Identifier("hello12".to_string())
        );
        let mut chars = " hello12_3".chars().peekable();
        assert_eq!(
            identifier(&mut chars),
            Err(Error {
                msg: format!("Expected alphabetic character")
            })
        );
        let mut chars = "1hello12_3".chars().peekable();
        assert_eq!(
            identifier(&mut chars),
            Err(Error {
                msg: format!("Expected alphabetic character")
            })
        );
        let mut chars = "hello world guys".chars().peekable();
        assert_eq!(
            identifier(&mut chars).unwrap(),
            Token::Identifier("hello".to_string())
        );
        assert_eq!(chars.next().unwrap(), ' ');
        assert_eq!(
            identifier(&mut chars).unwrap(),
            Token::Identifier("world".to_string())
        );
        assert_eq!(chars.next().unwrap(), ' ');
        assert_eq!(
            identifier(&mut chars).unwrap(),
            Token::Identifier("guys".to_string())
        );
    }

    #[test]
    fn test_number() {
        let test = |str: &str, expected: f64| {
            let mut chars = str.chars().peekable();
            assert_eq!(number(&mut chars).unwrap(), Token::Number(expected));
        };

        let mut chars = "1.34.5".chars().peekable();
        assert_eq!(get_number_string(&mut chars), "1");
        assert_eq!(chars.next().unwrap(), '.');
        assert_eq!(chars.next().unwrap(), '3');
        test("1.0", 1.0);
        test("1.", 1.0);
        test("1", 1.0);
        test(".012", 0.012);
        test("1.34.5", 1.34);

        let test_false = |str: &str| {
            let mut chars = str.chars().peekable();
            assert_eq!(
                number(&mut chars),
                Err(Error {
                    msg: format!("Expected number literal")
                })
            );
        };

        test_false("abc");
        test_false("!;b");
        test_false(" 13");
    }

    #[test]
    fn test_get_token() {
        let test = |str: &str, exp: &[Token]| {
            let mut chars = str.chars().peekable();
            for expected in exp {
                assert_eq!(&get_token(&mut chars).unwrap(), expected);
            }
        };

        test(
            "def x = 1",
            &[
                Token::Def,
                Token::Identifier("x".to_string()),
                Token::Other('='),
                Token::Number(1.0),
            ],
        );
        test(
            "extern #x;\ndef defx=1.2y",
            &[
                Token::Extern,
                Token::Def,
                Token::Identifier("defx".to_string()),
                Token::Other('='),
                Token::Number(1.2),
                Token::Identifier("y".to_string()),
            ],
        );
    }
}
