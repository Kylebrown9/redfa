use std::iter::Peekable;
use std::cmp::Ordering;

/// A set represented as an ordered collection
/// of elements with no duplicates.
/// In the `Just` case, these are the elements in the set.
/// In the `Not` case, these are the elements not in the set.
pub enum Set<T> {
    /// A Set represented by the elements it contains
    Just(Vec<T>),
    /// A Set represented using its complement
    Not(Vec<T>),
}

impl<T> Set<T>
    where
        T: Ord + Clone {

    /// The set intersection operator
    pub fn inter(&self, b: &[T]) -> Set<T> {
        match *self {
            Set::Just(ref a) => {
                Set::Just(inter(
                    a.iter().cloned(),
                    b.iter().cloned()
                ).collect())
            }
            Set::Not(ref a) => {
                Set::Just(subtract(
                    b.iter().cloned(),
                    a.iter().cloned()
                ).collect())
            }
        }
    }

    /// The set subtraction operator
    pub fn subtract(&self, b: &[T]) -> Set<T> {
        match *self {
            Set::Just(ref a) => {
                Set::Just(subtract(
                    a.iter().cloned(),
                    b.iter().cloned()
                ).collect())
            }
            Set::Not(ref a) => {
                Set::Not(union(
                    a.iter().cloned(),
                    b.iter().cloned()
                ).collect())
            }
        }
    }
}

/// Compute the union of two iter_set iterators as an iterator.
pub fn union<T, It1, It2>(a: It1, b: It2) -> Union<T, It1, It2>
    where
        T: Ord,
        It1: Iterator<Item=T>,
        It2: Iterator<Item=T> {

    Union { a: a.peekable(), b: b.peekable() }
}

/// A struct holding two iterators of ordered non-duplicated items.
/// When iterated over it produces the ordered non-duplicated values that occur in either set.
pub struct Union<T, It1, It2>
    where
        T: Ord,
        It1: Iterator<Item=T>,
        It2: Iterator<Item=T> {

    a : Peekable<It1>,
    b : Peekable<It2>,
}

impl<T, It1, It2> Iterator for Union<T, It1, It2>
    where
        T: Ord,
        It1: Iterator<Item=T>,
        It2: Iterator<Item=T> {

    type Item = T;

    fn next(&mut self) -> Option<T> {
        let order = match (self.a.peek(),  self.b.peek()) {
            (Some(av), Some(bv)) => av.cmp(bv),
            (Some(_), None) => Ordering::Less,
            (None, _) => Ordering::Greater,
        };
        match order {
            Ordering::Less => {
                self.a.next()
            }
            Ordering::Greater => {
                self.b.next()
            }
            Ordering::Equal => {
                self.a.next();
                self.b.next()
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (a1, a2) = self.a.size_hint();
        let (b1, b2) = self.b.size_hint();

        let lower = a1 + b1;
        let upper = if let (Some(a2), Some(b2)) = (a2, b2) {
            Some(a2 + b2)
        } else {
            None
        };

        (lower, upper)
    }
}

/// Compute the intersection of two iter_set iterators as an iterator.
pub fn inter<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>>(a: It1, b: It2) -> Inter<T, It1, It2> {
    Inter { a: a.peekable(), b: b.peekable() }
}

/// A struct holding two iterators of ordered non-duplicated items.
/// When iterated over it produces the ordered non-duplicated values that occur in both sets.
pub struct Inter<T, It1, It2>
    where
        T: Ord,
        It1: Iterator<Item=T>,
        It2: Iterator<Item=T> {

    a : Peekable<It1>,
    b : Peekable<It2>,
}

impl<T, It1, It2> Iterator for Inter<T, It1, It2>
    where
        T: Ord,
        It1: Iterator<Item=T>,
        It2: Iterator<Item=T> {

    type Item = T;

    fn next(&mut self) -> Option<T> {
        loop {
            match if let (Some(av), Some(bv)) = (self.a.peek(), self.b.peek()) {
                av.cmp(bv)
            } else {
                return None
            } {
                Ordering::Less => {
                    self.a.next();
                }
                Ordering::Greater => {
                    self.b.next();
                }
                Ordering::Equal => {
                    self.a.next();
                    return self.b.next();
                }
            }
        }
    }
}

/// Compute the subtraction of the right-hand iter_set iterator
/// from the left-hand one as an iterator.
pub fn subtract<T, It1, It2>(a: It1, b: It2) -> Subtract<T, It1, It2>
    where
        T: Ord,
        It1: Iterator<Item=T>,
        It2: Iterator<Item=T> {

    Subtract { a: a.peekable(), b: b.peekable() }
}

/// A struct holding two iterators of ordered non-duplicated items.
/// When iterated over it produces the ordered non-duplicated values
/// that occur in the left set, but not the right one.
pub struct Subtract<T, It1, It2>
    where
        T: Ord,
        It1: Iterator<Item=T>,
        It2: Iterator<Item=T> {

    a : Peekable<It1>,
    b : Peekable<It2>,
}

impl<T, It1, It2> Iterator for Subtract<T, It1, It2>
    where
        T: Ord,
        It1: Iterator<Item=T>,
        It2: Iterator<Item=T> {

    type Item = T;

    fn next(&mut self) -> Option<T> {
        loop {
            match match (self.a.peek(), self.b.peek()) {
                (Some(av), Some(bv)) => av.cmp(bv),
                (_, None) => Ordering::Less,
                (None, _) => return None,
            } {
                Ordering::Less => {
                    return self.a.next();
                }
                Ordering::Greater => {
                    self.b.next();
                }
                Ordering::Equal => {
                    self.a.next();
                    self.b.next();
                }
            }
        }
    }
}

