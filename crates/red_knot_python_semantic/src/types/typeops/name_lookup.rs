use crate::db::Db;
use crate::types::{Type, InferContext};
use crate::types::diagnostic::{report_possibly_unresolved_reference, report_unresolved_reference};
use super::{TypeOperationResult, TypeOperationError, UniversalError};
use ruff_python_ast as ast;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum NameLookupError<'db> {
    PossiblyUndefinedName { type_when_defined: Type<'db> },
    UndefinedName,
}

impl<'db> TypeOperationError<'db> for NameLookupError<'db> {
    type AST = ast::ExprName;

    fn into_type(self, context: &InferContext, node: &ast::ExprName) -> Type<'db> {
        match self {
            NameLookupError::PossiblyUndefinedName { type_when_defined } => {
                report_possibly_unresolved_reference(context, node);
                type_when_defined
            }
            NameLookupError::UndefinedName => {
                report_unresolved_reference(context, node);
                Type::Unknown
            }
        }
    }

    fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, fallback: F) -> TypeOperationResult<'db, Self>
    where
        for<'a> &'a E2: Into<UniversalError<'db>>,
        F: FnOnce() -> TypeOperationResult<'db, E2>,
    {
        UniversalError::from(&self)
            .or_fall_back_to(db, || fallback().map_err(|err| (&err).into()))
            .map_err(|err| match err {
                UniversalError::DefiniteError => NameLookupError::UndefinedName,
                UniversalError::PossibleError { fallback } => {
                    NameLookupError::PossiblyUndefinedName {
                        type_when_defined: fallback,
                    }
                }
            })
    }
}

impl<'db> From<&NameLookupError<'db>> for UniversalError<'db> {
    fn from(err: &NameLookupError<'db>) -> Self {
        match err {
            NameLookupError::PossiblyUndefinedName { type_when_defined } => {
                UniversalError::PossibleError {
                    fallback: *type_when_defined,
                }
            }
            NameLookupError::UndefinedName => UniversalError::DefiniteError,
        }
    }
}

pub(crate) type NameLookupResult<'db> = TypeOperationResult<'db, NameLookupError<'db>>;

impl<'db> NameLookupResult<'db> {
    pub(crate) fn possibly_unbound(type_when_defined: Type<'db>) -> Self {
        Self::Err(NameLookupError::PossiblyUndefinedName { type_when_defined })
    }

    pub(crate) fn unbound() -> Self {
        Self::Err(NameLookupError::UndefinedName)
    }
}
