//! [`SyntaxChecker`] for AST-based syntax errors. //
//!
//! This checker is not responsible for traversing the AST itself. Instead, its
//! [`SourceOrderVisitor::enter_node`] method should be called on every node by
//! a parent `Visitor`.

use ruff_python_ast::{
    visitor::source_order::{SourceOrderVisitor, TraversalSignal},
    AnyNodeRef, StmtMatch,
};
use ruff_text_size::TextRange;

pub struct SyntaxChecker {
    /// The target Python version for detecting backwards-incompatible syntax
    /// changes.
    target_version: PythonVersion,
    /// The cumulative set of syntax errors found when visiting the source AST.
    errors: Vec<SyntaxError>,
}

impl SyntaxChecker {
    pub fn new(target_version: PythonVersion) -> Self {
        Self {
            target_version,
            errors: Vec::new(),
        }
    }

    pub fn finish(self) -> impl Iterator<Item = SyntaxError> {
        self.errors.into_iter()
    }
}

/// Representation of a Python version.
///
/// Based on the flexible implementation in the `red_knot_python_semantic` crate for easier
/// interoperability.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PythonVersion {
    pub major: u8,
    pub minor: u8,
}

impl PythonVersion {
    pub const PY39: PythonVersion = PythonVersion { major: 3, minor: 9 };
    pub const PY310: PythonVersion = PythonVersion {
        major: 3,
        minor: 10,
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub range: TextRange,
    pub target_version: PythonVersion,
}

impl SyntaxError {
    pub fn message(&self) -> String {
        match self.kind {
            SyntaxErrorKind::MatchBeforePy310 => format!(
                "Cannot use `match` statement on Python {major}.{minor} (syntax was new in Python 3.10)",
                major = self.target_version.major,
                minor = self.target_version.minor,
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxErrorKind {
    MatchBeforePy310,
}

impl SyntaxErrorKind {
    pub const fn as_str(self) -> &'static str {
        match self {
            SyntaxErrorKind::MatchBeforePy310 => "match-before-python-310",
        }
    }
}

impl<'a> SourceOrderVisitor<'a> for SyntaxChecker {
    fn enter_node(&mut self, node: AnyNodeRef<'a>) -> TraversalSignal {
        match node {
            AnyNodeRef::StmtMatch(StmtMatch { range, .. }) => {
                if self.target_version < PythonVersion::PY310 {
                    self.errors.push(SyntaxError {
                        kind: SyntaxErrorKind::MatchBeforePy310,
                        range: *range,
                        target_version: self.target_version,
                    });
                }
            }
            AnyNodeRef::ModModule(_)
            | AnyNodeRef::ModExpression(_)
            | AnyNodeRef::StmtFunctionDef(_)
            | AnyNodeRef::StmtClassDef(_)
            | AnyNodeRef::StmtReturn(_)
            | AnyNodeRef::StmtDelete(_)
            | AnyNodeRef::StmtTypeAlias(_)
            | AnyNodeRef::StmtAssign(_)
            | AnyNodeRef::StmtAugAssign(_)
            | AnyNodeRef::StmtAnnAssign(_)
            | AnyNodeRef::StmtFor(_)
            | AnyNodeRef::StmtWhile(_)
            | AnyNodeRef::StmtIf(_)
            | AnyNodeRef::StmtWith(_)
            | AnyNodeRef::StmtRaise(_)
            | AnyNodeRef::StmtTry(_)
            | AnyNodeRef::StmtAssert(_)
            | AnyNodeRef::StmtImport(_)
            | AnyNodeRef::StmtImportFrom(_)
            | AnyNodeRef::StmtGlobal(_)
            | AnyNodeRef::StmtNonlocal(_)
            | AnyNodeRef::StmtExpr(_)
            | AnyNodeRef::StmtPass(_)
            | AnyNodeRef::StmtBreak(_)
            | AnyNodeRef::StmtContinue(_)
            | AnyNodeRef::StmtIpyEscapeCommand(_)
            | AnyNodeRef::ExprBoolOp(_)
            | AnyNodeRef::ExprNamed(_)
            | AnyNodeRef::ExprBinOp(_)
            | AnyNodeRef::ExprUnaryOp(_)
            | AnyNodeRef::ExprLambda(_)
            | AnyNodeRef::ExprIf(_)
            | AnyNodeRef::ExprDict(_)
            | AnyNodeRef::ExprSet(_)
            | AnyNodeRef::ExprListComp(_)
            | AnyNodeRef::ExprSetComp(_)
            | AnyNodeRef::ExprDictComp(_)
            | AnyNodeRef::ExprGenerator(_)
            | AnyNodeRef::ExprAwait(_)
            | AnyNodeRef::ExprYield(_)
            | AnyNodeRef::ExprYieldFrom(_)
            | AnyNodeRef::ExprCompare(_)
            | AnyNodeRef::ExprCall(_)
            | AnyNodeRef::ExprFString(_)
            | AnyNodeRef::ExprStringLiteral(_)
            | AnyNodeRef::ExprBytesLiteral(_)
            | AnyNodeRef::ExprNumberLiteral(_)
            | AnyNodeRef::ExprBooleanLiteral(_)
            | AnyNodeRef::ExprNoneLiteral(_)
            | AnyNodeRef::ExprEllipsisLiteral(_)
            | AnyNodeRef::ExprAttribute(_)
            | AnyNodeRef::ExprSubscript(_)
            | AnyNodeRef::ExprStarred(_)
            | AnyNodeRef::ExprName(_)
            | AnyNodeRef::ExprList(_)
            | AnyNodeRef::ExprTuple(_)
            | AnyNodeRef::ExprSlice(_)
            | AnyNodeRef::ExprIpyEscapeCommand(_)
            | AnyNodeRef::ExceptHandlerExceptHandler(_)
            | AnyNodeRef::FStringExpressionElement(_)
            | AnyNodeRef::FStringLiteralElement(_)
            | AnyNodeRef::PatternMatchValue(_)
            | AnyNodeRef::PatternMatchSingleton(_)
            | AnyNodeRef::PatternMatchSequence(_)
            | AnyNodeRef::PatternMatchMapping(_)
            | AnyNodeRef::PatternMatchClass(_)
            | AnyNodeRef::PatternMatchStar(_)
            | AnyNodeRef::PatternMatchAs(_)
            | AnyNodeRef::PatternMatchOr(_)
            | AnyNodeRef::TypeParamTypeVar(_)
            | AnyNodeRef::TypeParamTypeVarTuple(_)
            | AnyNodeRef::TypeParamParamSpec(_)
            | AnyNodeRef::FStringFormatSpec(_)
            | AnyNodeRef::PatternArguments(_)
            | AnyNodeRef::PatternKeyword(_)
            | AnyNodeRef::Comprehension(_)
            | AnyNodeRef::Arguments(_)
            | AnyNodeRef::Parameters(_)
            | AnyNodeRef::Parameter(_)
            | AnyNodeRef::ParameterWithDefault(_)
            | AnyNodeRef::Keyword(_)
            | AnyNodeRef::Alias(_)
            | AnyNodeRef::WithItem(_)
            | AnyNodeRef::MatchCase(_)
            | AnyNodeRef::Decorator(_)
            | AnyNodeRef::ElifElseClause(_)
            | AnyNodeRef::TypeParams(_)
            | AnyNodeRef::FString(_)
            | AnyNodeRef::StringLiteral(_)
            | AnyNodeRef::BytesLiteral(_)
            | AnyNodeRef::Identifier(_) => {}
        }
        TraversalSignal::Skip
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use insta::assert_debug_snapshot;
    use ruff_python_ast::{visitor::source_order::SourceOrderVisitor, PySourceType};
    use ruff_python_trivia::textwrap::dedent;

    use crate::{PythonVersion, SyntaxChecker, SyntaxError};

    /// Run [`check_syntax`] on a snippet of Python code.
    fn test_snippet(contents: &str, target_version: PythonVersion) -> Vec<SyntaxError> {
        let path = Path::new("<filename>");
        let source_type = PySourceType::from(path);
        let parsed = ruff_python_parser::parse_unchecked_source(&dedent(contents), source_type);
        let mut checker = SyntaxChecker::new(target_version);
        checker.visit_body(parsed.suite());
        checker.errors
    }

    #[test]
    fn match_before_py310() {
        assert_debug_snapshot!(test_snippet(
            r#"
match var:
    case 1:
        print("it's one")
"#,
            PythonVersion::PY39,
        ), @r"
        [
            SyntaxError {
                kind: MatchBeforePy310,
                range: 1..49,
                target_version: PythonVersion {
                    major: 3,
                    minor: 9,
                },
            },
        ]
        ");
    }

    #[test]
    fn match_on_py310() {
        assert_debug_snapshot!(test_snippet(
            r#"
match var:
    case 1:
        print("it's one")
"#,
            PythonVersion::PY310,
        ), @"[]");
    }
}
