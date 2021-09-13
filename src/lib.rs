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

// Lifetime added here because type declaration requires it, Rust compiler
// should be able to infer it a lot of times tho.
type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

// We can implement the trait for any function that matches the signature of a
// parser
trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

/* "Parsing is the process of deriving a structure from a stream of data"
 * in this sense, a parser "teases" out that structure.
 * At a fundamental level, a parser may just a single fn that accepts an
 * input and returns either the parsed structure or an error.
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
    // what we do here is basically replace regex with our own code; that is, go
    // char by char n make sure it's valid.
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

// Parser combinator: takes in two parsers as input and returns a parser that
// uses the two parsers passed to it in order. In our case, it will parse
// literals and identifiers (the order being up to us but ofc has to be
// according to spec).

/// pair combines two parser functions, each with its own result. The function
/// takes in two parsers that each return a Result with a tuple of the remaining
/// input and a result, and returns a function that will apply the parsers to an
/// input string in order. e.g first we would match a literal, return the result
/// and the rest of the input, on the input we then apply another parser to find
/// an identifier and at the end return whatever's left of the initial input
/// string as well as the two results from the parsers.
fn pair<'a, F1, F2, R1, R2>(parser1: F1, parser2: F2) -> impl Parser<'a, (R1, R2)>
where
    F1: Parser<'a, R1>,
    F2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next, result)| {
            parser2
                .parse(next)
                .map(|(final_input, second_result)| (final_input, (result, second_result)))
        })
    }
}
fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
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

#[test]
fn identifier_parser() {
    // Test we alphanumeric + dash
    assert_eq!(Ok((" ", "input-123".to_string())), identifier("input-123 "));

    // Test entirely identifier
    assert_eq!(
        Ok(("", "i-am-identifier".to_string())),
        identifier("i-am-identifier")
    );

    // Test err
    assert_eq!(Err("<invalidinput>"), identifier("<invalidinput>"));
}

#[test]
fn pair_combinator() {
    let parse_bracket = match_literal("[");

    let input = "[my-name-Jeff";
    let no_remaining_in = pair(parse_bracket, identifier);
    assert_eq!(
        Ok(("", ((), "my-name-Jeff".to_string()))),
        no_remaining_in(input)
    );

    let tag_opener = pair(match_literal("<"), identifier);
    let tag_good_input = "<Element1/>";
    assert_eq!(
        Ok(("/>", ((), "Element1".to_string()))),
        tag_opener(tag_good_input),
    );

    let tag_bad_input = "Element1/>";
    assert_eq!(Err("Element1/>"), tag_opener(tag_bad_input));
    let tag_bad_input = "<1Element/>";
    assert_eq!(Err("1Element/>"), tag_opener(tag_bad_input));
}
