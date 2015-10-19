#![feature(iter_arith)]
#![feature(plugin)]
#![plugin(oak)]

extern crate oak_runtime;
use std::io::{stdin, stdout, Write};
use oak_runtime::*;

grammar! haskeel {
    use std::str::FromStr;

    #[derive(Debug)]
    pub struct Binding(pub String, pub PExpr);

    pub type PExpr = Box<Expression>;
    #[derive(Debug)]
    pub enum Expression {
        Literal(u64),
        Variable(String),
        LetIn(Binding, PExpr),
        Apply {
            func: Function,
            params: Vec<Expression>,
        },
    }

    #[derive(Debug)]
    pub enum Function {
        Add,
        Min,
        Mul,
        Div,
        Exp,
        Neg,
    }

    program = spacing expression

    expression = term (term_op term)* > fold_left
    term = expo (factor_op expo)* > fold_left
    expo = (factor expo_op)* factor > fold_right

    literal   = int spacing > to_number
    variable  = ident spacing
    let_in    = let_kw binding in_kw expression

    factor = literal           > lit
           / variable          > var
           / let_in            > let_in
           / un_op factor      > un_op
           / lparen expression rparen

    term_op   = add_op > add
              / min_op > min
    factor_op = mul_op > mul
              / div_op > div
    expo_op   = exp_op > exp
    un_op     = min_op > neg

    binding = variable bind_op expression

    add_op  = "+" spacing
    min_op  = "-" spacing
    exp_op  = "^" spacing
    mul_op  = "*" spacing
    div_op  = "/" spacing
    bind_op = "=" spacing
    lparen  = "(" spacing
    rparen  = ")" spacing

    int = ["0-9"]+ > to_string
    ident = !keyword ["_a-zA-Z"] ["a-zA-Z0-9_"]* > collapse

    keyword = let_kw
            / in_kw
    let_kw = "let" spacing
    in_kw = ";" spacing
    spacing = [" \n\r\t"]* -> ()

    fn collapse(raw_first: char, raw_rest: Vec<char>) -> String {
        vec![raw_first].into_iter().chain(raw_rest.into_iter()).collect()
    }

    fn to_string(raw: Vec<char>) -> String { raw.into_iter().collect() }
    fn to_number(i: String) -> u64 { u64::from_str(&i).unwrap() }

    fn bin_expr(first: Expression, (bin, second): (Function, Expression)) -> Expression {
        Expression::Apply { func: bin, params: vec![first, second] }
    }

    fn fold_left(head: Expression, rest: Vec<(Function, Expression)>) -> Expression {
        rest.into_iter().fold(head, bin_expr)
    }

    fn fold_right(front: Vec<(Expression, Function)>, last: Expression) -> Expression {
        front.into_iter().rev().fold(last, |a, (b, op)| bin_expr(a, (op, b)))
    }

    fn var(name: String) -> Expression { Expression::Variable(name) }
    fn lit(i: u64) -> Expression { Expression::Literal(i) }
    fn let_in(bind: (String, Expression), expr: Expression) -> Expression { Expression::LetIn(Binding(bind.0, Box::new(bind.1)), Box::new(expr)) }
    fn un_op(fun: Function, expr: Expression) -> Expression {
        Expression::Apply { func: fun, params: vec![expr] }
    }

    fn add() -> Function { Function::Add }
    fn min() -> Function { Function::Min }
    fn mul() -> Function { Function::Mul }
    fn div() -> Function { Function::Div }
    fn exp() -> Function { Function::Exp }
    fn neg() -> Function { Function::Neg }
}

fn eval(program: haskeel::Expression) -> Result<i64, String> {
    let mut cntxt: Vec<(String, i64)> = vec![];
    execute(program, &mut cntxt)
}

fn execute(program: haskeel::Expression, context: &mut Vec<(String, i64)>) -> Result<i64, String> {
    use haskeel::Expression::*;
    match program {
        Literal(i) => Ok(i as i64),
        Variable(var) =>
            // rev so shadowed variable names work correctly
            if let Some(tup) = context.iter().rev().find(|tup| tup.0 == var) {
                Ok(tup.1)
            } else {
                Err(format!("The variable {} does not exist.", var))
            },
        LetIn(bind, expr) =>
        {
            let haskeel::Binding(name, bexp) = bind;
            let eval_bind = try!(execute(*bexp, context));
            context.push(
                (name, eval_bind)
            );
            let out = execute(*expr, context);
            context.pop();
            out
        },
        Apply {
            func: f,
            params: par,
        } => Ok(apply(
            f,
            try!(
                par.into_iter().map(|e|
                    execute(e, context)
                ).settle()
            )
        )),
    }
}

fn apply(func: haskeel::Function, params: Vec<i64>) -> i64 {
    use haskeel::Function::*;
    let mut params_iter = params.into_iter();
    match func {
        Add => params_iter.sum(),
        Min =>
            if let Some(n) = params_iter.next() {
                n - params_iter.sum::<i64>()
            } else {
                0
            },
        Mul => params_iter.product(),
        Div =>
            if let Some(n) = params_iter.next() {
                n / params_iter.product::<i64>()
            } else {
                0
            },
        Exp =>
            if let Some(n) = params_iter.next() {
                params_iter.map(|i| i as u32).fold(n, i64::pow)
            } else {
                0
            },
        Neg =>
            if let Some(n) = params_iter.next() {
                -n
            } else {
                0
            },
    }
}

trait Settle<O, E> {
    fn settle(self) -> Result<O, E>;
}

impl<T, A, B> Settle<Vec<A>, B> for T where T: Iterator<Item=Result<A, B>> {
    fn settle(self) -> Result<Vec<A>, B> {
        let mut out = vec![];

        for i in self {
            match i {
                Ok(o)  => out.push(o),
                Err(e) => return Err(e),
            }
        }

        Ok(out)
    }
}

fn print(msg: &'static str) {
    print!("{}", msg);
    stdout().flush();
}

fn main() {
    let mut prog = "".to_owned();
    let std = stdin();

    print("=> ");
    'rw: loop {
        std.read_line(&mut prog).unwrap();

        match haskeel::parse_program(prog.stream()).into_result() {
            Ok((a, _)) =>
                match eval(a.data) {
                    Ok(out) => println!("(evaluates to {})", out),
                    Err(e)  => println!("Error: {}", e),
                }
            ,
            Err(e) => { println!("{}", e); print(".. "); continue 'rw; },
        }

        prog = "".to_owned();
        print("=> ");
    }
}
