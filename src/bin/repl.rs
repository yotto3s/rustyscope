use std::io::Write;
use std::iter::Peekable;

use rustyscope::*;

fn main() -> std::io::Result<()> {
    main_loop()?;
    Ok(())
}

fn main_loop() -> std::io::Result<()> {
    let stdin = std::io::stdin();
    'outer: loop {
        print!("ready> ");
        std::io::stdout().flush()?;

        let mut buffer = String::new();
        stdin.read_line(&mut buffer)?;
        let mut tokens = match lexer::TokenIter::new(&mut buffer.chars().peekable())
            .collect::<Result<Vec<lexer::Token>, lexer::Error>>()
        {
            Ok(iter) => iter.into_iter().peekable(),
            Err(e) => {
                println!("Error: {}", e);
                continue 'outer;
            }
        };

        'inner: while let Some(token) = tokens.peek() {
            let result = match token {
                lexer::Token::Eof => break 'inner,
                lexer::Token::Other(';') => {
                    tokens.next();
                    continue 'inner;
                }
                lexer::Token::Def => handle_definition(&mut tokens),
                lexer::Token::Extern => handle_extern(&mut tokens),
                lexer::Token::Other(':') => {
                    let _ = tokens.next();
                    match handle_repl_command(&mut tokens) {
                        Ok(true) => break 'outer,
                        Ok(false) => Ok(()),
                        Err(()) => break 'inner,
                    }
                }
                _ => handle_expression(&mut tokens),
            };

            if let Err(e) = result {
                match e {
                    parser::Error::WrongToken(_, lexer::Token::Eof) => {
                        let mut next_line = String::new();
                        print!("...");
                        std::io::stdout().flush()?;
                        stdin.read_line(&mut next_line)?;
                        buffer += &next_line;
                        tokens = match lexer::TokenIter::new(&mut buffer.chars().peekable())
                            .collect::<Result<Vec<lexer::Token>, lexer::Error>>()
                        {
                            Ok(iter) => iter.into_iter().peekable(),
                            Err(e) => {
                                println!("Error: {}", e);
                                continue 'outer;
                            }
                        };
                        continue 'inner;
                    }
                    _ => {
                        println!("Error: {}", e);
                        break 'inner;
                    }
                }
            }
        }
    }
    Ok(())
}

fn handle_repl_command<I>(tokens: &mut Peekable<I>) -> Result<bool, ()>
where
    I: Iterator<Item = lexer::Token>,
{
    match tokens.next() {
        Some(lexer::Token::Identifier(id)) => {
            if id == "quit" {
                Ok(true)
            } else {
                println!("Unknown repl command {}", id);
                Err(())
            }
        }
        _ => Err(()),
    }
}

fn handle_definition<I>(tokens: &mut Peekable<I>) -> Result<(), parser::Error>
where
    I: Iterator<Item = lexer::Token>,
{
    let _ = parser::parse_definition(tokens)?;
    println!("parsed a definition");
    Ok(())
}

fn handle_extern<I>(tokens: &mut Peekable<I>) -> Result<(), parser::Error>
where
    I: Iterator<Item = lexer::Token>,
{
    let _ = parser::parse_extern(tokens)?;
    println!("parsed an extern");
    Ok(())
}

fn handle_expression<I>(tokens: &mut Peekable<I>) -> Result<(), parser::Error>
where
    I: Iterator<Item = lexer::Token>,
{
    let _ = parser::parse_expr(tokens)?;
    println!("parsed a top level expression");
    Ok(())
}
