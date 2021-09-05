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
type ParseFn = fn(&str) -> Result<(&str, ()), &str>;
/// match_literal is a function that builds a parser to find a static string of
/// any lengrth in a given input stream. It returns a Unit if the 'needle' has
/// been found successfully, along with the rest of the input (minus our
/// 'needle').
fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| match input.strip_prefix(expected) {
        // Here we get the len because...never presume about the length of the unicode
        // monster.
        Some(stripped_in) => Ok((stripped_in, ())),
        _ => Err(input),
    }
}
// Before, we used a function to parse a string. We passed in the needle and the
// haystack (or just the haystack in the case of finding the letter 'a') and use
// it. Here, we're passing in our expected string and return a closure. Our
// parser became a higher order function, it takes in a param and returns a fn.
// I suppose since the input is not declared in the func signature, it'll be
// captured in the returned closure.

/// match_literal will let us parse <, >, even </ or /> but to parse the Element
/// name, we need a few more changes to how we do things. We can't rely on
/// simple string comparisons however we could use regex (which is slgihtly to
/// heavy).
/// Instead, we can rely on the rules and assumptions we made for this parser:
/// an element name identifier may have one alphabetical character, followed by
/// zxero or more of either an alphabetical character, a number or a dash.
fn identifier(input: &str) -> Result<(&str, String), &str> {
    let mut matched = String::new();
    let mut chars = input.chars();

    // Make sure first char is alphabetic
    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

// ==== TESTS =====
#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello, Joe!");
    assert_eq!(Ok(("", ())), parse_joe("Hello, Joe!"));
    assert_eq!(
        Ok(((" Hello, Jack!"), ())),
        parse_joe("Hello, Joe! Hello, Jack!")
    );
    assert_eq!(Err("Hello, bromandude"), parse_joe("Hello, bromandude"));
    let input = "Dear all, welcome to this fine evening. I am here to tell you that you need to do something...for further directions, read on. I have no idea what I'm saying. Hello, Joe! Ya alright m8";
    //assert_eq!(Ok((" Ya alright m8", ())), parse_joe(input));
}
