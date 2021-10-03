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

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }
    //TODO: zero_or_more, pair, one_or_more. Leave L-R as is
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

/// BoxedParser represents a boxed parser function; it is boxed because we want
/// to provide a way for compiler to know size of object (in this case a ptr)
/// ahead of time. This will allow us to reduce types in our parsers...I think.
struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
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
fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.strip_prefix(expected) {
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
fn identifier(input: &str) -> ParseResult<String> {
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

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

// Combines one or more parsers, useful for whitespaces
fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        // First, parse the first element, if it's a success, push it into the
        // vec.
        if let Ok((next_input, item)) = parser.parse(input) {
            input = next_input;
            result.push(item);
        } else {
            return Err(input);
        }

        // Second, keep parsing until we bump into an error
        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, F, A>(parser: P, pred_fn: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next, val)) = parser.parse(input) {
            if pred_fn(&val) {
                return Ok((next, val));
            }
        }
        Err(input)
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

/// The start of an XML element. If we see "<" and an identifier, followed by
/// zero or more attributes, then we know we started an XML attribute.
fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}

/// Use element_start combinator to parse an Element. What happens is, we first
/// pair up element_start and literal /> that signals an Element ends. We take
/// the output of element_start through left() combinator. At the end, we're
/// left with the actual output from pair(identifier, attributes) which will
/// give us the name of the element and its attributes. Only thing left is to
/// map this output to an actual element.
fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

/// A combinator that will try to parsers in order: if first succeeds, we return
/// result. If first fails, we try second on the same input before returning an
/// error.
fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input),
    }
}

/// Either a parent or a single element.
fn element<'a>() -> impl Parser<'a, Element> {
    either(single_element(), open_element())
}

/// Is a parent element closed? We expect </ and the name of the element
/// inside.
fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(match_literal("</"), left(identifier, match_literal(">")))
        .pred(move |name| name == &expected_name)
}

/// Gives us a parent element, children and all
fn parent_element<'a>() -> impl Parser<'a, Element> {
    pair(
        open_element(),
        left(zero_or_more(element()), close_element()),
    )
}

// ==== TESTS =====
#[test]
fn literal_parser() {
    let parse_joe = match_literal("Hello, Joe!");
    assert_eq!(Ok(("", ())), parse_joe.parse("Hello, Joe!"));
    assert_eq!(
        Ok(((" Hello, Jack!"), ())),
        parse_joe.parse("Hello, Joe! Hello, Jack!")
    );
    assert_eq!(
        Err("Hello, bromandude"),
        parse_joe.parse("Hello, bromandude")
    );
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
fn right_combinator() {
    let tag_opener = right(match_literal("<"), identifier);
    let tag_good_input = "<Element1/>";
    assert_eq!(
        Ok(("/>", "Element1".to_string())),
        tag_opener.parse(tag_good_input),
    );

    let tag_bad_input = "Element1/>";
    assert_eq!(Err("Element1/>"), tag_opener.parse(tag_bad_input));
    let tag_bad_input = "<1Element/>";
    assert_eq!(Err("1Element/>"), tag_opener.parse(tag_bad_input));
}

#[test]
fn one_or_more_combinator() {
    let parser = one_or_more(match_literal("foo"));
    assert_eq!(Ok(("", vec![(), ()])), parser.parse("foofoo"));
    assert_eq!(Ok(("bar", vec![(), ()])), parser.parse("foofoobar"));
    assert_eq!(Err("barfoo"), parser.parse("barfoo"));
}

#[test]
fn zero_or_more_combinator() {
    let parser = zero_or_more(match_literal("foo"));
    assert_eq!(Ok(("", vec![(), ()])), parser.parse("foofoo"));
    assert_eq!(Ok(("bar", vec![(), ()])), parser.parse("foofoobar"));
    assert_eq!(Ok(("barfoo", vec![])), parser.parse("barfoo"));
}

#[test]
fn is_whitespace_pred() {
    let parser = pred(any_char, |c| *c == 'o');
    assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
    assert_eq!(Err("nmg"), parser.parse("nmg"));
}

#[test]
fn quoted_strings() {
    let parser = quoted_string();
    assert_eq!(Ok(("", "abc".to_string())), parser.parse("\"abc\""));
    assert_eq!(Err(""), parser.parse("\""));
}

#[test]
fn zero_or_more_attributes() {
    let parser = attributes();
    assert_eq!(
        Ok(("", vec![("Key".to_string(), "Value".to_string())])),
        parser.parse("  Key=\"Value\"")
    );
    assert_eq!(
        Ok((
            "",
            vec![
                ("k1".to_string(), "v1".to_string()),
                ("k2".to_string(), "v2".to_string())
            ]
        )),
        parser.parse("  k1=\"v1\" k2=\"v2\"")
    );
}

#[test]
fn single_element_parser() {
    assert_eq!(
        Ok((
            "",
            Element {
                name: "div".to_string(),
                attributes: vec![("class".to_string(), "float".to_string())],
                children: vec![],
            }
        )),
        single_element().parse("<div class=\"float\"/>")
    );
}
