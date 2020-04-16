use Regex;

use crate::iter_set::{ Set, union };

/// The set of some object's derivatives with respect to an alphabet `T`.
/// T - the type for symbols in the alphabet
/// R - the type of results
#[derive(Debug, Clone)]
pub struct Derivatives<T, R> {
    /// Holds a set of pairs `(chars, derivative)`, meaning that the derivative
    /// with respect to any element of `chars` is `derivative`.
    pub d: Vec<Derivative<T, R>>,
    /// The derivative with respect to any character not listed in `d`.
    pub rest: R,
}

#[derive(Debug, Clone)]
pub struct Derivative<T, R> {
    pub chars: Vec<T>,
    pub res: R
}

impl<T, R> Derivatives<T, R> {
    pub fn map<F: FnMut(R) -> R>(self, mut f: F) -> Derivatives<T, R> {
        Derivatives {
            d: self.d.into_iter().map(|der| Derivative { chars: der.chars, res: f(der.res) }).collect(),
            rest: f(self.rest),
        }
    }
}

/// A trait for types which can be differentiated with respect to an alphabet
/// `T`.
pub trait Differentiable<T>: Sized {
    fn derivative(&self) -> Derivatives<T, Self>;
}

fn combine<T, R, S, F>(v: &[Derivatives<T, R>], mut f: F) -> Derivatives<T, S>
    where
        T:  Ord + Clone,
        F: FnMut(&[&R]) -> S {

    let mut result = (Vec::new(), None);
    let mut regexes = Vec::new();

    go(v, &mut f, Set::Not(Vec::new()), &mut regexes, &mut result);
    
    Derivatives {
        d: result.0,
        rest: result.1.unwrap(),
    }
}

fn go<'a, T, R, S, F>(
    v: &'a [Derivatives<T, R>],
    f: &mut F,
    what: Set<T>,
    current: &mut Vec<&'a R>,
    out: &mut (Vec<Derivative<T, S>>, Option<S>)
) 
    where
        T: Ord + Clone,
        F: FnMut(&[&R]) -> S {

    if let Set::Just(ref v) = what {
        if v.len() == 0 {
            // prune
            return;
        }
    }
    if v.len() == 0 {
        let reg = f(&current);
        match what {
            Set::Just(c) => out.0.push(Derivative { chars: c, res: reg }),
            Set::Not(_) => {
                assert!(out.1.is_none());
                out.1 = Some(reg);
            }
        }
        return;
    }
    let (first, rest) = v.split_at(1);
    let first = &first[0];
    let mut all_chars = Vec::new();
    for &Derivative { ref chars, res: ref reg } in first.d.iter() {
        all_chars = union(all_chars.into_iter(), chars.iter().cloned()).collect();
        let inter = what.inter(&chars);
        current.push(reg);
        go(rest, f, inter, current, out);
        current.pop();
    }

    let inter = what.subtract(&all_chars);
    current.push(&first.rest);
    go(rest, f, inter, current, out);
    current.pop();
}

impl<T: Ord + Clone> Differentiable<T> for Regex<T> {
    fn derivative(&self) -> Derivatives<T, Regex<T>> {
        match *self {
            Regex::Null => Derivatives { d: Vec::new(), rest: Regex::Null },
            Regex::Empty => Derivatives { d: Vec::new(), rest: Regex::Null },
            Regex::Except(ref cs) => {
                if cs.len() == 0 {
                    Derivatives { d: Vec::new(), rest: Regex::Empty }
                } else {
                    Derivatives { d: vec![Derivative { chars: cs.clone(), res: Regex::Null }], rest: Regex::Empty }
                }
            }
            Regex::Alt(ref cs, ref xs) => {
                let mut ds = Vec::with_capacity(if cs.len() > 0 { 1 } else { 0 } + xs.len());
                if cs.len() > 0 {
                    ds.push(Derivatives { d: vec![Derivative { chars: cs.clone(), res: Regex::Empty }], rest: Regex::Null });
                }
                ds.extend(xs.iter().map(Differentiable::derivative));
                combine(&ds, |regexes| Regex::Alt(Vec::new(), regexes.iter().map(|r| (*r).clone()).collect()))
            }
            Regex::And(ref xs) => {
                let ds: Vec<_> = xs.iter().map(Differentiable::derivative).collect();
                combine(&ds, |regexes| Regex::And(regexes.iter().map(|r| (*r).clone()).collect()))
            }
            Regex::Not(ref x) => x.derivative().map(|r| Regex::Not(Box::new(r))),
            Regex::Cat(ref xs) => {
                let mut ds = Vec::new();
                for i in 0..xs.len() {
                    ds.push(xs[i].derivative().map(|r| {
                        let mut v = vec![r];
                        v.extend(xs[i+1..].iter().cloned());
                        Regex::Cat(v)
                    }));
                    if !xs[i].nullable() {
                        break;
                    }
                }
                combine(&ds, |regexes| Regex::Alt(Vec::new(), regexes.iter().map(|r| (*r).clone()).collect()))
            }
            Regex::Kleene(ref x) => x.derivative().map(|r| Regex::Cat(vec![r, Regex::Kleene(x.clone())])),
        }
    }
}

// Derivatives of "regular vectors", as described in "Regular-expression derivatives reexamined" by Owens et al.
impl<T, R> Differentiable<T> for Vec<R>
    where
        T: Ord + Clone,
        R: Differentiable<T> + Clone {

    fn derivative(&self) -> Derivatives<T, Vec<R>> {
        let v: Vec<Derivatives<T, R>> = self.iter().map(Differentiable::derivative).collect();
        combine(&*v, |xs: &[&R]| xs.iter().map(|&x| x.clone()).collect())
    }
}
