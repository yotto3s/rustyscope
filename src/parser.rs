//! Parser implementation for kaleidoscope.

use std::collections::HashMap;
use std::iter::Peekable;

use crate::lexer::*;

/// Represents parser error.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Error {
    /// Error description.
    pub msg: String,
}

/// Binary operator precedence
fn binop_precedence(ch: char) -> Option<u32> {
    match ch {
        '<' => Some(10),
        '+' => Some(20),
        '-' => Some(20),
        '*' => Some(40),
        _ => None,
    }
}

fn binop_precedence_token(token: &Token) -> Option<(char, u32)> {
    if let Token::Other(op) = token {
        if let Some(prec) = binop_precedence(*op) {
            return Some((*op, prec));
        }
    }
    None
}

/// An enum representing ast node.
/// expr
///     ::= number
///     ::= variable
///     ::= binary_expr
///     ::= calling_expr
///     ::= '(' expr ')'
#[derive(Debug, Clone, PartialEq)]
pub enum ExprAST {
    /// Number literal.
    /// number ::= <floating point>
    Number(f64),
    /// Variable.
    /// variable ::= <identifier>
    Variable(String),
    /// Binary expression, i.e. `1 + 2` is BinaryExpr("+", Number(1), Number(2))
    /// binary_expr ::= expr <binary_op> expr
    BinaryExpr(String, Box<ExprAST>, Box<ExprAST>),
    /// Call expression, i.e. add(1, 2)
    /// call_expr ::= <identifier> '(' expression* ')'
    CallExpr(String, Vec<Box<ExprAST>>),
}

/// Represents prototype(declaration) i.e. `def add(x, y)`
#[derive(Debug, Clone, PartialEq)]
pub struct PrototypeAST(String, Vec<String>);
/// Represents function definition, i.e. `def add(x, y) = x + y`
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionAST(Box<PrototypeAST>, Box<ExprAST>);

type TokenIter<I> = Peekable<I>;

/// Parses a expression.
pub fn parse_expr<I>(tokens: &mut TokenIter<I>) -> Result<Box<ExprAST>, Error>
where
    I: Iterator<Item = Token>,
{
    let lhs = parse_primary(tokens)?;
    parse_binary_rhs(tokens, 0, lhs)
}

fn parse_binary_rhs<I>(
    tokens: &mut TokenIter<I>,
    precedence: u32,
    lhs: Box<ExprAST>,
) -> Result<Box<ExprAST>, Error>
where
    I: Iterator<Item = Token>,
{
    match tokens.peek() {
        Some(token) => {
            if let Some((binop, op_prec)) = binop_precedence_token(token) {
                if op_prec < precedence {
                    return Ok(lhs);
                }

                let _ = tokens.next();
                let mut rhs = parse_primary(tokens)?;
                if let Some(next_token) = tokens.peek() {
                    if let Some((_, next_prec)) = binop_precedence_token(next_token) {
                        if op_prec < next_prec {
                            rhs = parse_binary_rhs(tokens, op_prec + 1, rhs)?;
                        }
                    }
                }
                parse_binary_rhs(
                    tokens,
                    precedence,
                    Box::new(ExprAST::BinaryExpr(binop.to_string(), lhs, rhs)),
                )
            } else {
                Ok(lhs)
            }
        }
        None => Ok(lhs),
    }
}

fn parse_number_expr<I>(tokens: &mut TokenIter<I>) -> Result<Box<ExprAST>, Error>
where
    I: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token::Number(x)) => Ok(Box::new(ExprAST::Number(x))),
        Some(t) => Err(Error {
            msg: format!("Expected number, found {}", t),
        }),
        None => Err(Error {
            msg: format!("Expected number, found nothing"),
        }),
    }
}

fn parse_paren_expr<I>(tokens: &mut TokenIter<I>) -> Result<Box<ExprAST>, Error>
where
    I: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token::Other('(')) => {
            let e = parse_expr(tokens)?;
            match tokens.next() {
                Some(Token::Other(')')) => Ok(e),
                _ => Err(Error {
                    msg: format!("Expected ')'"),
                }),
            }
        }
        _ => Err(Error {
            msg: format!("Expected '('"),
        }),
    }
}

fn parse_expressions<I>(tokens: &mut TokenIter<I>) -> Result<Vec<Box<ExprAST>>, Error>
where
    I: Iterator<Item = Token>,
{
    let mut res = vec![parse_expr(tokens)?];
    while tokens.peek() == Some(&Token::Other(',')) {
        let _ = tokens.next();
        res.push(parse_expr(tokens)?);
    }
    Ok(res)
}

fn parse_identifier_expr<I>(tokens: &mut TokenIter<I>) -> Result<Box<ExprAST>, Error>
where
    I: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token::Identifier(id)) => match tokens.peek() {
            Some(Token::Other('(')) => {
                let _ = tokens.next();
                let expressions = if tokens.peek() != Some(&Token::Other(')')) {
                    parse_expressions(tokens)?
                } else {
                    Vec::new()
                };
                if tokens.next() == Some(Token::Other(')')) {
                    Ok(Box::new(ExprAST::CallExpr(id, expressions)))
                } else {
                    Err(Error {
                        msg: format!("Expected ')'"),
                    })
                }
            }
            _ => Ok(Box::new(ExprAST::Variable(id))),
        },
        _ => Err(Error {
            msg: format!("Expected identifier"),
        }),
    }
}

fn parse_primary<I>(tokens: &mut TokenIter<I>) -> Result<Box<ExprAST>, Error>
where
    I: Iterator<Item = Token>,
{
    if let Some(tok) = tokens.peek() {
        match tok {
            Token::Identifier(_) => parse_identifier_expr(tokens),
            Token::Number(_) => parse_number_expr(tokens),
            Token::Other('(') => parse_paren_expr(tokens),
            _ => Err(Error {
                msg: format!("Expected identifier, number or '('"),
            }),
        }
    } else {
        Err(Error {
            msg: format!("Expected identifier, number or '('"),
        })
    }
}

fn parse_prototype<I>(tokens: &mut TokenIter<I>) -> Result<Box<PrototypeAST>, Error>
where
    I: Iterator<Item = Token>,
{
    if let Some(Token::Identifier(name)) = tokens.next() {
        if tokens.next() != Some(Token::Other('(')) {
            return Err(Error {
                msg: format!("Expected '('"),
            });
        }
        let mut ids = Vec::new();
        loop {
            match tokens.next() {
                Some(Token::Identifier(id)) => ids.push(id),
                Some(Token::Other(')')) => break Ok(Box::new(PrototypeAST(name, ids))),
                _ => {
                    break Err(Error {
                        msg: format!("Expected identifier or ')'"),
                    })
                }
            }
        }
    } else {
        Err(Error {
            msg: format!("Expected identifier"),
        })
    }
}

fn parse_definition<I>(tokens: &mut TokenIter<I>) -> Result<Box<FunctionAST>, Error>
where
    I: Iterator<Item = Token>,
{
    if tokens.next() != Some(Token::Def) {
        Err(Error {
            msg: format!("Expected 'def'"),
        })
    } else {
        let proto = parse_prototype(tokens)?;
        let expr = parse_expr(tokens)?;
        Ok(Box::new(FunctionAST(proto, expr)))
    }
}

fn parse_extern<I>(tokens: &mut TokenIter<I>) -> Result<Box<PrototypeAST>, Error>
where
    I: Iterator<Item = Token>,
{
    if tokens.next() != Some(Token::Extern) {
        Err(Error {
            msg: format!("Expected 'extern'"),
        })
    } else {
        parse_prototype(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn test_parse_prototype() {
        let mut tokens = [Identifier("f".to_string()), Other('('), Other(')')]
            .into_iter()
            .peekable();
        let ast = parse_prototype(&mut tokens).unwrap();
        assert_eq!(ast.as_ref(), &PrototypeAST("f".to_string(), vec![]));

        let mut tokens = [
            Identifier("add".to_string()),
            Other('('),
            Identifier("x".to_string()),
            Identifier("y".to_string()),
            Other(')'),
        ]
        .into_iter()
        .peekable();
        let ast = parse_prototype(&mut tokens).unwrap();
        assert_eq!(
            ast.as_ref(),
            &PrototypeAST("add".to_string(), vec!["x".to_string(), "y".to_string()])
        );

        let mut tokens = [
            Identifier("add".to_string()),
            Identifier("x".to_string()),
            Identifier("y".to_string()),
            Other(')'),
        ]
        .into_iter()
        .peekable();
        let ast = parse_prototype(&mut tokens);
        assert_eq!(
            ast,
            Err(Error {
                msg: format!("Expected '('")
            })
        );

        let mut tokens = [
            Identifier("add".to_string()),
            Other('('),
            Identifier("x".to_string()),
            Identifier("y".to_string()),
        ]
        .into_iter()
        .peekable();
        let ast = parse_prototype(&mut tokens);
        assert_eq!(
            ast,
            Err(Error {
                msg: format!("Expected identifier or ')'")
            })
        );
    }
}
