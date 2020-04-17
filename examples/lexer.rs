#![crate_type="bin"]
extern crate redfa;

use std::io::BufRead;
use std::fmt::Debug;

use redfa::*;
use redfa::dfa::Normalize;
use redfa::dfa::Dfa;

fn main() {
    let lexer = Lexer::new(vec![
        ("[a-zA-Z][_a-zA-Z]*", Token::Ident),
        ("[1-9][0-9]*", Token::Num),
        (",", Token::Comma)
    ]).unwrap();

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        lexer.tokenize(&line);
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Token {
    Ident, Num, Comma
}

struct Lexer<T> {
    dfa: Dfa<char, Option<T>>
}

impl<T> Lexer<T> {
    fn new<I>(iterable: I) -> Option<Self>
    where
        T: Copy + Ord,
        I: IntoIterator<Item=(&'static str, T)> {

        let mut iterator = iterable.into_iter();

        let (first_r, first_t) = iterator.next()?;
        let mut dfa = Lexer::make_dfa(first_r, first_t)?;

        while let Some((regex, token)) = iterator.next() {
            let new_dfa = Lexer::make_dfa(regex, token)?;
            dfa = dfa.product(new_dfa, Lexer::<T>::overlay).minimize().map(|v| *v);
        }

        Some(Lexer { dfa })
    }
    
    fn make_dfa(regex_str: &str, token: T) -> Option<Dfa<char, Option<T>>>
        where
            T: Copy {

        let regex = regex_str.trim().parse::<Regex<char>>().ok()?;
        let regex = regex.normalize();
        let (dfa, _mapping) = Dfa::from_derivatives(vec![regex, Regex::Null]);
        let dfa = dfa.map(|reg| reg.nullable());
        
        Some(dfa.minimize().map(|x| if *x { Some(token) } else { None }))
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

    fn tokenize(&self, input: &str)
        where
            T: Copy + Debug {

        let mut current_idx;
        let mut beginning = 0;
        let mut last_found: Option<(T, usize)> = None;
    
        loop {
            current_idx = 0;
            // println!("Beginning Lexing at {}", beginning);
    
            let mut iterator = input[beginning..].char_indices().peekable();
    
            while let Some((i, c)) = iterator.next() {
                let last_state = &self.dfa.states[current_idx];
    
                current_idx = if let Some(idx) = last_state.by_char.get(&c) {
                    *idx as usize
                } else {
                    last_state.default as usize
                };
                
                let current_state = &self.dfa.states[current_idx];
    
                if let Some(t) = &current_state.value {
                    last_found = Some((*t, i + beginning + 1));
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
}


