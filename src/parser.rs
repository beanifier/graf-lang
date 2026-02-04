/// A pointer
#[derive(Debug, Clone, Copy)]
pub enum Pointer {
    Regular(char),
    New,
    InChar,
    OutChar,
    SysVal,
    SysCall,
    Null,
}

/// A pointer expression
#[derive(Debug, Clone, Copy)]
pub enum PointerExpression {
    Exact(Pointer),
    NextMax(Pointer),
    NextMin(Pointer),
    NextRand(Pointer),
    NextSpecific(Pointer, i32),
}

/// A weight expression
#[derive(Debug, Clone, Copy)]
pub enum WeightExpression {
    WeightOf(PointerExpression),
    WeightOfInc(PointerExpression),
    WeightOfDec(PointerExpression),
}

/// A statement
#[derive(Debug, Clone, Copy)]
pub enum Statement {
    SetWeight(PointerExpression, WeightExpression),
    ConnectNodes(PointerExpression, PointerExpression),
    DisconnectNodes(PointerExpression, PointerExpression),
    SetPointer(Pointer, PointerExpression),
    Label,
    Jump(i32),
    JumpIfEqual(i32, PointerExpression, PointerExpression),
    JumpIfNotEqual(i32, PointerExpression, PointerExpression),
    Debug,
}

peg::parser! {
    pub grammar graf() for str {
        rule comment() = quiet!{"[" [^']']* "]"}

        rule ws() = quiet!{[' '|'\n'|'\t'|'\r']*}

        rule ws_comment() = quiet!{(comment()/[' '|'\n'|'\t'|'\r'])*}

        rule pointer() -> Pointer
            = "_" { Pointer::Null }
            / "!" { Pointer::New }
            / "(" { Pointer::InChar }
            / ")" { Pointer::OutChar }
            / "{" { Pointer::SysVal }
            / "}" { Pointer::SysCall }
            / c:['a'..='z'] { Pointer::Regular(c) }

        rule num_i32() -> i32
            = n:$("-"? ['0'..='9']+) {? n.parse().or(Err("i32")) }

        rule pointer_expression() -> PointerExpression
            = p:pointer() "*" n:num_i32() { PointerExpression::NextSpecific(p, n) }
            / p:pointer() "+" { PointerExpression::NextMax(p) }
            / p:pointer() "-" { PointerExpression::NextMin(p) }
            / p:pointer() "$" { PointerExpression::NextRand(p) }
            / p:pointer() { PointerExpression::Exact(p) }

        rule weight_expression() -> WeightExpression
            = "^" ws() p:pointer_expression() { WeightExpression::WeightOfInc(p) }
            / "V" ws() p:pointer_expression() { WeightExpression::WeightOfDec(p) }
            / p:pointer_expression() { WeightExpression::WeightOf(p) }

        rule statement() -> Statement
            = "<" { Statement::Label }
            / "#" {Statement::Debug}
            / a:pointer_expression() ws() "%" ws() b:weight_expression() { Statement::SetWeight(a, b) }
            / a:pointer_expression() ws() ">" ws() b:pointer_expression() { Statement::ConnectNodes(a, b) }
            / a:pointer_expression() ws() "/" ws() b:pointer_expression() { Statement::DisconnectNodes(a, b) }
            / a:pointer() ws() "=" ws() b:pointer_expression() { Statement::SetPointer(a, b) }
            / "@" ws() n:num_i32() { Statement::Jump(n) }
            / "?" ws() a:pointer_expression() ws() ":" ws() b:pointer_expression() ws() "@" ws() n:num_i32() { Statement::JumpIfEqual(n, a, b) }
            / "?" ws() a:pointer_expression() ws() ";" ws() b:pointer_expression() ws() "@" ws() n:num_i32() { Statement::JumpIfNotEqual(n, a, b) }

        /// Parses a string into a list of statements
        pub rule program() -> Vec<Statement>
            = ws_comment() a:(a:statement() ws_comment() {a})* { a }
    }
}
