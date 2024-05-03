//! Convert AST into llvm code.
use std::collections::HashMap;

use crate::*;

/// Represents an error during compiling.
#[derive(Debug, Clone, PartialEq)]
pub struct CompileError {
    msg: &'static str,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

/// Compiles AST into llvm binary code.
pub struct Compiler<'a, 'ctx> {
    context: &'ctx inkwell::context::Context,
    module: &'a inkwell::module::Module<'ctx>,
    builder: &'a inkwell::builder::Builder<'ctx>,
    variables: HashMap<String, inkwell::values::PointerValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Creates and inits a compiler.
    pub fn new(
        context: &'ctx inkwell::context::Context,
        module: &'a inkwell::module::Module<'ctx>,
        builder: &'a inkwell::builder::Builder<'ctx>,
    ) -> Self {
        let variables = HashMap::new();
        Self {
            context,
            module,
            builder,
            variables,
        }
    }
    /// Compiles a expression.
    pub fn compile_expr(
        &self,
        expr: &parser::ExprAST,
    ) -> Result<inkwell::values::FloatValue<'ctx>, CompileError> {
        match expr {
            parser::ExprAST::Number(x) => Ok(self.context.f64_type().const_float(*x)),
            parser::ExprAST::Variable(v) => match self.variables.get(v) {
                Some(&ptr) => Ok(self
                    .builder
                    .build_load(self.context.f64_type(), ptr, v)
                    .expect("build load")
                    .into_float_value()),
                None => Err(CompileError {
                    msg: "Unknown variable",
                }),
            },
            parser::ExprAST::BinaryExpr(op, lhs, rhs) => {
                let lval = self.compile_expr(lhs)?;
                let rval = self.compile_expr(rhs)?;
                match op.as_str() {
                    "+" => Ok(self.builder.build_float_add(lval, rval, "addtmp").unwrap()),
                    "-" => Ok(self.builder.build_float_sub(lval, rval, "subtmp").unwrap()),
                    "*" => Ok(self.builder.build_float_mul(lval, rval, "multmp").unwrap()),
                    "/" => Ok(self.builder.build_float_div(lval, rval, "divtmp").unwrap()),
                    ">" => Ok({
                        let cmp = self
                            .builder
                            .build_float_compare(inkwell::FloatPredicate::UGT, lval, rval, "cmptmp")
                            .unwrap();
                        self.builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")
                            .unwrap()
                    }),
                    "<" => Ok({
                        let cmp = self
                            .builder
                            .build_float_compare(inkwell::FloatPredicate::ULT, lval, rval, "cmptmp")
                            .unwrap();
                        self.builder
                            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")
                            .unwrap()
                    }),
                    _ => Err(CompileError {
                        msg: "Unknown binary operator",
                    }),
                }
            }
            parser::ExprAST::CallExpr(name, args) => {
                let func = match self.module.get_function(name.as_str()) {
                    Some(f) => Ok(f),
                    None => Err(CompileError {
                        msg: "Unknown function",
                    }),
                }?;

                if func.get_type().count_param_types() as usize != args.len() {
                    return Err(CompileError {
                        msg: "Mismatched number of arguments",
                    });
                }

                let compiled_args = args
                    .iter()
                    .map(|x| self.compile_expr(x))
                    .map(|x| x.map(|inner| inner.into()))
                    .collect::<Result<Vec<inkwell::values::BasicMetadataValueEnum>, CompileError>>(
                    )?;

                match self
                    .builder
                    .build_call(func, &compiled_args, "tmpcall")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                {
                    Some(value) => Ok(value.into_float_value()),
                    None => Err(CompileError {
                        msg: "Invalid call produced",
                    }),
                }
            }
        }
    }

    /// Compiles a prototype.
    pub fn compile_prototype(
        &self,
        proto: &parser::PrototypeAST,
    ) -> Result<inkwell::values::FunctionValue<'ctx>, CompileError> {
        let parser::PrototypeAST(name, args) = proto;
        let f64_type = self.context.f64_type();
        let arg_types = std::iter::repeat(f64_type)
            .take(args.len())
            .map(|f| f.into())
            .collect::<Vec<inkwell::types::BasicMetadataTypeEnum>>();

        let fn_type = f64_type.fn_type(arg_types.as_slice(), false);
        let fn_val = self.module.add_function(name.as_str(), fn_type, None);

        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(args[i].as_str());
        }

        Ok(fn_val)
    }
    /// Compiles a expression.
    pub fn compile_function(
        &mut self,
        fun_ast: &parser::FunctionAST,
    ) -> Result<inkwell::values::FunctionValue<'ctx>, CompileError> {
        let parser::FunctionAST(proto, expr) = fun_ast;
        let parser::PrototypeAST(name, args) = proto.as_ref();

        let fun = match self.module.get_function(name.as_str()) {
            Some(fun) => Ok(fun),
            None => self.compile_prototype(proto),
        }?;

        // if !fun.is_undef() {
        // return Err(CompileError{msg: "Function cannot be redefined."});
        // }

        let block = self.context.append_basic_block(fun, "entry");
        self.builder.position_at_end(block);
        self.variables.clear();
        for (i, arg) in fun.get_param_iter().enumerate() {
            let arg_name = &args[i];
            let alloca = self
                .builder
                .build_alloca(self.context.f64_type(), arg_name)
                .unwrap();
            self.builder.build_store(alloca, arg).unwrap();
            self.variables.insert(args[i].clone(), alloca);
        }

        let body = match self.compile_expr(expr.as_ref()) {
            Ok(b) => Ok(b),
            Err(e) => {
                unsafe {
                    fun.delete();
                }
                Err(e)
            }
        }?;
        self.builder.build_return(Some(&body)).unwrap();
        if fun.verify(true) {
            Ok(fun)
        } else {
            unsafe {
                fun.delete();
            }
            Err(CompileError {
                msg: "Invalid generated function.",
            })
        }
    }
}
