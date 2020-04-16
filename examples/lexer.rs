#![crate_type="bin"]
extern crate redfa;

use std::io::BufRead;

use redfa::*;
use redfa::dfa::Normalize;
use redfa::dfa::Dfa;

fn main() {
    let dfa_ident = make_dfa("[a-zA-Z][a-zA-Z]*", Token::Ident).unwrap();
    let _dfa_num   = make_dfa("[1-9][0-9]*", Token::Num).unwrap();
    let _dfa_comma = make_dfa(",", Token::Comma).unwrap();

    // TODO find a way to combine the DFA's
    let dfa_comb = dfa_ident;

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        
        tokenize(&dfa_comb, &line);
    }
}

#[derive(Debug, Clone, Copy)]
enum Token {
    Ident, Num, Comma
}

fn make_dfa(regex_str: &str, token: Token) -> Option<Dfa<char, Option<Token>>> {
    let regex = regex_str.trim().parse::<Regex<char>>().ok()?;
    let regex = regex.normalize();
    let (dfa, _mapping) = Dfa::from_derivatives(vec![regex, Regex::Null]);
    let dfa = dfa.map(|reg| reg.nullable());
    
    Some(dfa.minimize().map(|x| if *x { Some(token) } else { None }))
}

fn tokenize(_dfa: &Dfa<char, Option<Token>>, _input: &str) {
    // TODO tokenize input and print out token stream and spans
}
