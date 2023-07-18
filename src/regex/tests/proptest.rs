use proptest::{collection, option::of, prelude::*};

use crate::prelude::W;

use super::{
    ast::{ExprKind, GroupedLiteralKind, LiteralKind},
    parser::Parser,
    tokenizer::{ClassKind, QuantifierKind, QuantifierRangeKind},
};

fn is_special_regex(c: char) -> bool {
    matches!(
        c,
        '[' | ']' | '(' | ')' | '^' | '|' | '-' | '*' | '+' | '?' | '{' | '.'
    )
}

impl ToString for QuantifierRangeKind {
    fn to_string(&self) -> String {
        use QuantifierRangeKind::*;
        match *self {
            Max(m) => format!("{{{}}}", m),
            Min(m) => format!("{{{},}}", m),
            Range(min, max) => format!("{{{},{}}}", min, max),
        }
    }
}

impl ToString for QuantifierKind {
    fn to_string(&self) -> String {
        use QuantifierKind::*;
        match *self {
            Asterisk => "*".to_string(),
            Plus => "+".to_string(),
            QuestionMark => "?".to_string(),
            Range(range_kind) => range_kind.to_string(),
        }
    }
}

impl ToString for ClassKind {
    fn to_string(&self) -> String {
        use ClassKind::*;
        match *self {
            Wildcard => ".",
            Word => r"\w",
            Whitespace => r"\s",
            Digit => r"\d",
            NonWord => r"\W",
            NonDigit => r"\D",
            NonWhitespace => r"\S",
        }
        .to_string()
    }
}

impl ToString for GroupedLiteralKind {
    fn to_string(&self) -> String {
        use GroupedLiteralKind::*;
        match *self {
            Match(m) => char_to_string(m),
            Class(class_kind) => class_kind.to_string(),
            Range(begin, end) => format!("{}-{}", char_to_string(begin), char_to_string(end)),
        }
    }
}

impl ToString for LiteralKind {
    fn to_string(&self) -> String {
        use LiteralKind::*;
        match self {
            Match(m) => char_to_string(*m),
            Class(class_kind) => class_kind.to_string(),
            Group { negated, literals } => format!(
                "[{}{}]",
                match negated {
                    true => "^",
                    false => "",
                },
                literals
                    .0
                    .iter()
                    .map(|l| l.to_string())
                    .collect::<Vec<_>>()
                    .join("")
            ),
        }
    }
}

impl ToString for ExprKind {
    fn to_string(&self) -> String {
        use ExprKind::*;
        match self {
            Concat(expressions) => expressions
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(""),
            Empty => "".to_string(),
            Alt(lhs, rhs) => format!("{}|{}", lhs.to_string(), rhs.to_string()),
            Lit(literal_kind, quantifier) => format!(
                "{}{}",
                literal_kind.to_string(),
                match quantifier {
                    Some(quantifier) => quantifier.to_string(),
                    None => "".to_string(),
                }
            ),
            Group(expr_kind, quantifier) => format!(
                "({}){}",
                expr_kind.to_string(),
                match quantifier {
                    Some(quantfifier) => quantfifier.to_string(),
                    None => "".to_string(),
                }
            ),
        }
    }
}

fn char_to_string(c: char) -> String {
    match c {
        '\\' => r"\\\\".to_string(),
        c => c.to_string(),
    }
}

fn arb_grouped_literal_kind() -> impl Strategy<Value = GroupedLiteralKind> {
    prop_oneof![
        any::<char>()
            .prop_filter("Non-special regex characters", |c| !is_special_regex(*c))
            .prop_map(GroupedLiteralKind::Match),
        any::<ClassKind>().prop_map(GroupedLiteralKind::Class),
        (any::<char>(), any::<char>())
            .prop_filter(
                "Non-special regex characters",
                |(begin, end)| !(is_special_regex(*begin) || is_special_regex(*end))
            )
            .prop_map(|(begin, end)| GroupedLiteralKind::Range(begin, end))
    ]
}

fn arb_literal_kind() -> impl Strategy<Value = LiteralKind> {
    prop_oneof![
        any::<char>()
            .prop_filter("Non-special regex characters", |c| !is_special_regex(*c))
            .prop_map(LiteralKind::Match),
        any::<ClassKind>().prop_map(LiteralKind::Class),
        (
            any::<bool>(),
            collection::vec(arb_grouped_literal_kind(), 0..=6)
        )
            .prop_map(|(negated, literals)| LiteralKind::Group {
                negated,
                literals: W(literals)
            })
    ]
}

fn arb_expr_kind() -> impl Strategy<Value = ExprKind> {
    // Creating the empty regex is not allowed. It can only be at the root of
    // the AST, which is not supported by proptest.
    let leaf = prop_oneof![(arb_literal_kind(), of(any::<QuantifierKind>()))
        .prop_map(|(literal_kind, quantifier)| ExprKind::Lit(literal_kind, quantifier))];

    leaf.prop_recursive(3, 32, 3, |inner| {
        prop_oneof![
            collection::vec(inner.clone(), 10).prop_map(ExprKind::Concat),
            (inner.clone(), inner.clone())
                .prop_map(|(lhs, rhs)| ExprKind::Alt(Box::new(lhs), Box::new(rhs))),
            (inner, of(any::<QuantifierKind>())).prop_map(|(expr_kind, quantifier)| {
                ExprKind::Group(Box::new(expr_kind), quantifier)
            })
        ]
    })
}

proptest! {
    // In release builds, 10000 tests takes about 2.5 seconds without coverage.
    #![proptest_config(ProptestConfig::with_cases(10000))]

    #[test]
    #[ignore = "proptests should be run explicitly"]
    fn all_valid_regex(r in arb_expr_kind().prop_map(|e| e.to_string())) {
        println!("{}", r);
        Parser::new(&r).parse().unwrap();
    }
}
