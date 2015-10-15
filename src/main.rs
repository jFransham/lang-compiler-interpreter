#![feature(plugin)]
#![plugin(oak)]

extern crate oak_runtime;
use oak_runtime::*;

grammar! test {
    use std::collections::HashMap;
    use std::str::FromStr;

    program = (spacing expression)*

    expression = var > parse_expr_var
               / lit > parse_expr_lit
               / fcall > parse_expr_fcall
               / ifthen > parse_expr_ifthen
               / letin > parse_expr_letin
               / block > parse_expr_block
               / record  > parse_expr_record
               / tuple   > parse_expr_tuple
               / bracketed > parse_expr

    // Expressions
    ifthen = kw_if ("|" ifdef)+ (elsedef)? > combine_ifdef
    ifdef = expression kw_then expression
    elsedef = kw_else expression > parse_else
    var = identifier
    lit = number  > parse_value
        / boolean > parse_value
        / string  > parse_value
        / func    > parse_value
    fcall = ":" expression (expression)*
    block = expression (";" expression)* > combine
    letin = kw_let bindingdef+ kw_in expression
    bindingdef = pattern eq expression
    bracketed = lparen expression rparen

    pattern = identifier

    // Literals
    num_or_none = "_" > no_char
                / individual_num > some_char
    num_part = individual_num (num_or_none+ individual_num)? > parse_n_part
    number = num_part > parse_int
           // / num_part? "." num_part > parse_float
    boolean = kw_true > parse_true
            / kw_false > parse_false
    string = string_delim string_inner string_delim
           / string_single_delim string_single_inner string_single_delim
    string_inner        = .* > to_string
    string_single_inner = .* > to_string
    tuple = ltupleparen expression ("," expression)* rtupleparen > combine
    func = pattern* fbody expression > parse_value_func
    record = lrecordparen bindingdef* rrecordparen

    spacing = [" \n\t"]* -> ()
    kw_if = "if"
    kw_else = "else"
    kw_then = "then"
    kw_true = "true"
    kw_false = "false"
    kw_let = "let"
    kw_in = "in"
    eq = "="
    string_delim = "\""
    string_single_delim = "'"
    fbody = "=>"
    identifier = (["a-zA-Z"] ["_a-zA-Z0-9"]*) > combine_to_string
    individual_num = ["0-9"]
    lparen = "("
    rparen = ")"
    ltupleparen = "["
    rtupleparen = "]"
    lrecordparen = "{"
    rrecordparen = "}"

    fn no_char() -> Option<char> { None }
    fn some_char(c: char) -> Option<char> { Some(c) }
    fn parse_true() -> bool { true }
    fn parse_false() -> bool { false }

    fn combine(first: Expression, mut second: Vec<Expression>) -> Vec<Expression> {
        second.insert(0, first);
        second
    }

    fn combine_ifdef(mut first: Vec<IfDef>, second: Option<IfDef>) -> Vec<IfDef> {
        if let Some(s) = second { first.insert(0, s); }
        first
    }

    fn combine_to_string(first: char, mut rest: Vec<char>) -> String {
        rest.insert(0, first);
        rest.into_iter().collect()
    }

    fn parse_else(e: Expression) -> (Expression, Expression) {
        (Expression::Literal(Value::Bool(true)), e)
    }

    fn parse_n_part(a: char, b: Option<(Vec<Option<char>>, char)>) -> String {
        match b {
            None => a.to_string(),
            Some((s_b, c)) => {
                let mut out = vec![a];
                for o_char in s_b.into_iter() {
                    if let Some(chr) = o_char { out.push(chr); }
                }
                out.push(c);
                out.into_iter().collect()
            }
        }
    }

    fn escaped_quote() -> Vec<char> { vec!['"'] }
    fn escaped_single_quote() -> Vec<char> { vec!['\''] }

    fn to_string(raw: Vec<char>) -> String {
        raw.into_iter().collect()
    }

    /*fn parse_num_part<T: IntoIter<char>>(raw: T) -> usize {
        usize::from_str(&raw.into_iter().collect())
    }*/

    fn parse_int(i: String) -> i64 { i64::from_str(&i).unwrap() }
    fn parse_float(a: Option<usize>, b: usize) -> f64 {
        use std::num::*;
        let bf = b as f64;
        (a.unwrap_or_default() as f64) + (
                bf /
                f64::powi(10.0, bf.log(10.0).floor() as i32 + 1)
            )
    }

    fn parse_value<T: Into<Value>>(inner: T) -> Value {
        inner.into()
    }

    fn parse_expr_var(v: String) -> Expression { Expression::Variable(v) }
    fn parse_expr_lit(v: Value) -> Expression { Expression::Literal(v) }
    fn parse_expr_fcall(v0: Expression, v1: Vec<Expression>) -> Expression { Expression::FCall(Box::new(v0), v1) }
    fn parse_expr_ifthen(v: Vec<IfDef>) -> Expression { Expression::If(v) }
    fn parse_expr_letin(v0: Vec<BindingDef>, v1: Expression) -> Expression { Expression::LetIn(v0, Box::new(v1)) }
    fn parse_expr_block(v: Vec<Expression>) -> Expression { Expression::Block(v) }
    fn parse_expr_record(v: Vec<(String, Expression)>) -> Expression { Expression::Record(v) }
    fn parse_expr_tuple(v: Vec<Expression>) -> Expression { Expression::Tuple(v) }
    fn parse_expr(v: Expression) -> Expression { v }

    fn parse_value_func(v0: Vec<Pattern>, v1: Expression) -> Func { (v0, Box::new(v1)) }

    pub type PExpr = Box<Expression>;
    pub enum Expression {
        Variable(String),
        Literal(Value),
        FCall(PExpr, Vec<Expression>),
        If(Vec<IfDef>),
        LetIn(Vec<BindingDef>, PExpr),
        Block(Vec<Expression>),
        Record(Vec<(String, Expression)>),
        Tuple(Vec<Expression>),
    }

    pub type BindingDef = (Pattern, Expression);
    pub type IfDef = (Expression, Expression);
    pub type Func = (Vec<Pattern>, PExpr);
    pub type Pattern = String;

    pub enum Value {
        Number(i64),
        Bool(bool),
        String(String),
        Function(Func),
        Tuple(Vec<Value>),
        Record(HashMap<String, Value>),
        Error(String),
    }

    impl Into<Value> for i64 {
        fn into(self) -> Value { Value::Number(self) }
    }

    impl Into<Value> for String {
        fn into(self) -> Value { Value::String(self) }
    }

    impl Into<Value> for Func {
        fn into(self) -> Value { Value::Function(self) }
    }

    impl Into<Value> for bool {
        fn into(self) -> Value { Value::Bool(self) }
    }
}

fn main() {
    let prog = r##"
        let a = 0
        in a
    "##;

    match test::parse_program(prog.stream()).into_result() {
        Ok((_, _)) => println!("Success!"), 
        Err(e) => println!("Failed parsing, reason: {}", e)
    }
}
