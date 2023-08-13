use super::{
    ast::{ExprKind, GroupedLiteralKind, LiteralKind},
    parser::Parser,
    tokenizer::{ClassKind, QuantifierKind, QuantifierRangeKind},
};
use crate::prelude::W;
use proptest::{collection, option::of, prelude::*};

fn is_special_regex(c: char) -> bool {
    matches!(
        c,
        '[' | ']' | '(' | ')' | '^' | '|' | '-' | '*' | '+' | '?' | '{' | '}' | '.'
    )
}

fn char_to_string(c: char) -> String {
    if is_special_regex(c) {
        format!(r"\{}", c)
    } else {
        match c {
            '\\' => r"\\".to_string(),
            '\0' => r"\0".to_string(),
            c => c.to_string(),
        }
    }
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
            Range(begin, end) => format!(r"{}-{}", char_to_string(begin), char_to_string(end)),
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

// # Regex grammar
//
// ```ebnf
//      expression ::= sub_expression (VERTICAL expression)?;
//      sub_expression ::= sub_exrpession_item+;
//      sub_expression_item ::= match | group;
//      group ::= LEFT_PAREN expresion RIGHT_PAREN QUANTFIER?;
//      match ::= match_item QUANTIFIER?;
//      match_item ::= CHARACTER_CLASS | CHARACTER | character_group;
//      character_group ::= LEFT_BRACKET CARRET? character_group_item+ RIGHT_BRACKET;
//      character_group_item ::= CHARACTER_CLASS | character_range | CHARACTER;
// ```
// # Ast
//
// Represenation of the AST using the grammar rules.
//
// enum ExprKind {
//     /// Concatenation of regular expressions.
//     Concat(Vec<(Lit, Group)>),
//     /// An empty regex expresion.
//     Empty,
//     /// An alternative expression (e.g., `<expression> | <expression>`).
//     Alt(Box<Concat>, Box<Concat>),
//     /// A literal (e.g., `a`, `[^ca]`, `[a-z]`, `[0-1]*`).
//     Lit(LiteralKind, Option<tokenizer::QuantifierKind>),
//     /// A grouped expression (e.g., `([a-z] | foo)`, `(ab[ac]){3,}`).
//     Group(Box<ExprKind>, Option<tokenizer::QuantifierKind>),
// }

fn arb_character_group_item() -> impl Strategy<Value = GroupedLiteralKind> {
    prop_oneof![
        any::<char>().prop_map(GroupedLiteralKind::Match),
        any::<ClassKind>().prop_map(GroupedLiteralKind::Class),
        (any::<char>(), any::<char>())
            .prop_map(|(begin, end)| GroupedLiteralKind::Range(begin, end))
    ]
}

fn arb_character_group() -> impl Strategy<Value = LiteralKind> {
    (
        any::<bool>(),
        collection::vec(arb_character_group_item(), 0..=6),
    )
        .prop_map(|(negated, literals)| LiteralKind::Group {
            negated,
            literals: W(literals),
        })
}

fn arb_match_item() -> impl Strategy<Value = LiteralKind> {
    prop_oneof![
        any::<char>().prop_map(LiteralKind::Match),
        any::<ClassKind>().prop_map(LiteralKind::Class),
        arb_character_group()
    ]
}

fn arb_match() -> impl Strategy<Value = ExprKind> {
    prop_oneof![(arb_match_item(), of(any::<QuantifierKind>()))
        .prop_map(|(literal_kind, quantifier)| ExprKind::Lit(literal_kind, quantifier))]
}

fn arb_group(inner: impl Strategy<Value = ExprKind>) -> impl Strategy<Value = ExprKind> {
    prop_oneof![(inner, of(any::<QuantifierKind>()))
        .prop_map(|(expr_kind, quantifier)| ExprKind::Group(Box::new(expr_kind), quantifier))]
}

fn arb_sub_expression_item(
    inner: impl Strategy<Value = ExprKind>,
) -> impl Strategy<Value = ExprKind> {
    prop_oneof![arb_match(), arb_group(inner)]
}

fn arb_sub_expression(inner: impl Strategy<Value = ExprKind>) -> impl Strategy<Value = ExprKind> {
    prop_oneof![
        collection::vec(arb_sub_expression_item(inner), 1..=10).prop_map(|exprs| {
            let mut iter = exprs.iter();
            let (first, second) = (iter.next(), iter.next());

            match (first, second) {
                (Some(_), Some(_)) => ExprKind::Concat(exprs),
                (Some(first), None) => first.clone(),
                _ => unreachable!("there should always be one expression"),
            }
        })
    ]
}

fn arb_expression() -> impl Strategy<Value = ExprKind> {
    let leaf = prop_oneof![arb_match()];

    leaf.prop_recursive(30, 500, 15, |inner| {
        prop_oneof![(
            arb_sub_expression(inner.clone()),
            of(arb_sub_expression(inner))
        )
            .prop_map(|(lhs, rhs)| match rhs {
                Some(rhs) => ExprKind::Alt(Box::new(lhs), Box::new(rhs)),
                None => lhs,
            })]
    })
}

proptest! {
    // On my main computer, with a release build, 10000 runs takes about 3.3 seconds.
    #![proptest_config(ProptestConfig::with_cases(10000))]

    #[test]
    #[ignore = "proptests should be run explicitly"]
    fn all_valid_regex(r in arb_expression()) {
        let regex = r.to_string();
        Parser::new(&regex).parse().expect(&regex);

        #[cfg(debug_assertions)]
        eprintln!("{:?}", &r);
    }
}
