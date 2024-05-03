use std::io::Write;
use std::iter::Peekable;

use rustyscope::*;

fn main() -> std::io::Result<()> {
    let context = inkwell::context::Context::create();
    let module = context.create_module("top_module");
    let builder = context.create_builder();
    let mut compiler = compiler::Compiler::new(&context, &module, &builder);
    main_loop(&mut compiler)?;
    Ok(())
}

fn main_loop(compiler: &mut compiler::Compiler) -> std::io::Result<()> {
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
                lexer::Token::Def => handle_definition(&mut tokens, compiler),
                lexer::Token::Extern => handle_extern(&mut tokens, compiler),
                lexer::Token::Other(':') => {
                    let _ = tokens.next();
                    match handle_repl_command(&mut tokens) {
                        Ok(true) => break 'outer,
                        Ok(false) => Ok(()),
                        Err(()) => break 'inner,
                    }
                }
                _ => handle_expression(&mut tokens, compiler),
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

fn handle_definition<I>(
    tokens: &mut Peekable<I>,
    compiler: &mut compiler::Compiler,
) -> Result<(), parser::Error>
where
    I: Iterator<Item = lexer::Token>,
{
    let ast = parser::parse_definition(tokens)?;
    match compiler.compile_function(&ast) {
        Ok(fnir) => println!("parsed a definition:{}", fnir.to_string()),
        Err(e) => println!("error: {}", e),
    }
    Ok(())
}

fn handle_extern<I>(
    tokens: &mut Peekable<I>,
    compiler: &mut compiler::Compiler,
) -> Result<(), parser::Error>
where
    I: Iterator<Item = lexer::Token>,
{
    let ast = parser::parse_extern(tokens)?;
    match compiler.compile_prototype(&ast) {
        Ok(ir) => println!("parsed a prototype:{}", ir.to_string()),
        Err(e) => println!("error: {}", e),
    }
    Ok(())
}

fn handle_expression<I>(
    tokens: &mut Peekable<I>,
    compiler: &mut compiler::Compiler,
) -> Result<(), parser::Error>
where
    I: Iterator<Item = lexer::Token>,
{
    let ast = parser::parse_top_level_expr(tokens)?;
    match compiler.compile_function(&ast) {
        Ok(ir) => println!("parsed a prototype:{}", ir.to_string()),
        Err(e) => println!("error: {}", e),
    }
    Ok(())
}
