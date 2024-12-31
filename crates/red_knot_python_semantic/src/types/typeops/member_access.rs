use super::{TypeOperationError, TypeOperationResult, UniversalError};
use crate::db::Db;
use crate::types::diagnostic::{POSSIBLY_UNBOUND_ATTRIBUTE, UNRESOLVED_ATTRIBUTE};
use crate::types::{InferContext, Type};
use ruff_python_ast as ast;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum MemberAccessErrorKind<'db> {
    PossiblyUndefinedMember { type_when_defined: Type<'db> },
    UndefinedMember,
}

impl<'db> MemberAccessErrorKind<'db> {
    const fn is_undefined_member(self) -> bool {
        matches!(self, Self::UndefinedMember)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MemberAccessError<'db> {
    pub(crate) member: &'db str,
    pub(crate) accessed_on: Type<'db>,
    pub(crate) kind: MemberAccessErrorKind<'db>,
}

impl<'db> MemberAccessError<'db> {
    pub(crate) fn undefined(member: &'db str, accessed_on: Type<'db>) -> Self {
        Self {
            member,
            accessed_on,
            kind: MemberAccessErrorKind::UndefinedMember,
        }
    }

    pub(crate) fn possibly_undefined(
        member: &'db str,
        accessed_on: Type<'db>,
        type_when_defined: Type<'db>,
    ) -> Self {
        Self {
            member,
            accessed_on,
            kind: MemberAccessErrorKind::PossiblyUndefinedMember { type_when_defined },
        }
    }
}

impl<'db> TypeOperationError<'db> for MemberAccessError<'db> {
    type AST = ast::AnyNodeRef<'db>;

    fn into_type(self, context: &InferContext, attribute_node: &ast::AnyNodeRef) -> Type<'db> {
        match self.kind {
            MemberAccessErrorKind::PossiblyUndefinedMember { type_when_defined } => {
                context.report_lint(
                    &POSSIBLY_UNBOUND_ATTRIBUTE,
                    *attribute_node,
                    format_args!(
                        "Attribute `{}` is possibly unbound on object of type `{}`",
                        self.member,
                        self.accessed_on.display(context.db()),
                    ),
                );
                type_when_defined
            }
            MemberAccessErrorKind::UndefinedMember => {
                context.report_lint(
                    &UNRESOLVED_ATTRIBUTE,
                    *attribute_node,
                    format_args!(
                        "Object of type `{}` has no attribute `{}`",
                        self.member,
                        self.accessed_on.display(context.db()),
                    ),
                );
                Type::Unknown
            }
        }
    }

    fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, fallback: F) -> TypeOperationResult<'db, Self>
    where
        for<'a> &'a E2: Into<UniversalError<'db>>,
        F: FnOnce() -> TypeOperationResult<'db, E2>,
    {
        let self_as_universal_error = UniversalError::from(&self);
        let MemberAccessError {
            member,
            accessed_on,
            kind,
        } = self;
        self_as_universal_error
            .or_fall_back_to(db, || fallback().map_err(|err| (&err).into()))
            .map_err(|err| match err {
                UniversalError::DefiniteError => MemberAccessError {
                    member,
                    accessed_on,
                    kind,
                },
                UniversalError::PossibleError { fallback } => MemberAccessError {
                    member,
                    accessed_on,
                    kind: MemberAccessErrorKind::PossiblyUndefinedMember {
                        type_when_defined: fallback,
                    },
                },
            })
    }
}

impl<'db> From<&MemberAccessError<'db>> for UniversalError<'db> {
    fn from(err: &MemberAccessError<'db>) -> Self {
        match err.kind {
            MemberAccessErrorKind::PossiblyUndefinedMember { type_when_defined } => {
                UniversalError::PossibleError {
                    fallback: type_when_defined,
                }
            }
            MemberAccessErrorKind::UndefinedMember => UniversalError::DefiniteError,
        }
    }
}

pub(crate) type MemberAccessResult<'db> = TypeOperationResult<'db, MemberAccessError<'db>>;
