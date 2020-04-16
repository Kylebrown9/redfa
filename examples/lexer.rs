#![crate_type="bin"]
extern crate redfa;

use std::io::BufRead;

use redfa::*;
use redfa::dfa::Normalize;
use redfa::dfa::Dfa;

fn main() {
    let dfa_ident = make_dfa("[a-zA-Z][_a-zA-Z]*", Token::Ident).unwrap();
    let dfa_num   = make_dfa("[1-9][0-9]*", Token::Num).unwrap();
    let dfa_comma = make_dfa(",", Token::Comma).unwrap();

    let dfa_comb = dfa_ident;
    let dfa_comb = dfa_comb.product(dfa_num, overlay);
    let dfa_comb = dfa_comb.product(dfa_comma, overlay);
    let dfa_comb = dfa_comb.minimize().map(|v| *v);

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        
        tokenize(&dfa_comb, &line);
    }
}

fn overlay<V>(l: &Option<V>, r: &Option<V>) -> Option<V>
    where
        V: Copy {

    match (*l, *r) {
        (Some(lv), None) => Some(lv),
        (_, Some(rv)) => Some(rv),
        (_, _) => None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

fn tokenize(dfa: &Dfa<char, Option<Token>>, input: &str) {
    let mut current_idx;
    let mut beginning = 0;
    let mut last_found: Option<(Token, usize)> = None;

    loop {
        current_idx = 0;
        // println!("Beginning Lexing at {}", beginning);

        let mut iterator = input[beginning..].char_indices().peekable();

        while let Some((i, c)) = iterator.next() {
            let last_state = &dfa.states[current_idx];

            current_idx = if let Some(idx) = last_state.by_char.get(&c) {
                *idx as usize
            } else {
                last_state.default as usize
            };
            
            let current_state = &dfa.states[current_idx];

            // println!("Input Index: {}, State Index: {}", i + beginning, current_idx);

            if let Some(t) = current_state.value {
                // println!("Candidate Match {:?} for [{}, {})", t, beginning, i + 1);
                last_found = Some((t, i + beginning + 1));
            }
    
            if current_state.trapped(current_idx as u32) || iterator.peek().is_none() {
                if let Some((found_token, found_end)) = last_found {
                    println!("Found {:?} for [{}, {}) = {}", found_token, beginning, found_end, &input[beginning..found_end]);
                    beginning = found_end;
                    last_found = None;
                    break;
                } else {
                    println!("Lexing Failed!");
                    return;
                }
            }
        }

        if input.len() == beginning {
            println!("Lexing Done!");
            break;
        }
    }   
}
