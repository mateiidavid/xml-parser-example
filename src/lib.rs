use std::error::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
/// Element represents an XML element
pub struct Element {
    /// Name of the element
    name: String,
    /// Tuple representing KV for an attribute
    attributes: Vec<(String, String)>,
    /// Nested elements
    children: Vec<Element>,
}

/* "Parsing is the process of deriving a structure from a stream of data"
 * in this sense, a parser "teases" out that structure.
 * At a fundamental level, a parser may just a single fn that accepts an input
 * and returns either the parsed structure or an error.
 * Fn(Input) --> Result<(Input, Output), Error>
 */

// We are using &str since it's a pointer to a part of a string this allows us
// to chop input as we process; could use bytez too
// type ParseFn = Fn(&str) -> Result<(&str, Element), &str>;

fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}
