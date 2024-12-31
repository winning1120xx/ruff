use crate::types::Type;
use super::{TypeOperationResult, UniversalError};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum ImplicitDunderCallErrorReason<'db> {
    DunderMethodNotPresent,
    DunderMethodPresentButNotCallable { dunder_method_type: Type<'db> },
    CallableDunderMethodCalledWithIncorrectArguments,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ImplicitDunderCallError<'db> {
    DefiniteError {
        reason: ImplicitDunderCallErrorReason<'db>,
    },
    PossibleError {
        reasons: smallvec::SmallVec<[(Type<'db>, ImplicitDunderCallErrorReason<'db>); 1]>,
        fallback_return_type: Type<'db>,
    },
}

impl<'db> From<&ImplicitDunderCallError<'db>> for UniversalError<'db> {
    fn from(err: &ImplicitDunderCallError<'db>) -> Self {
        match err {
            ImplicitDunderCallError::DefiniteError { .. } => UniversalError::DefiniteError,
            ImplicitDunderCallError::PossibleError {
                fallback_return_type,
                ..
            } => UniversalError::PossibleError {
                fallback: *fallback_return_type,
            },
        }
    }
}

pub(crate) type ImplicitDunderCallResult<'db> =
    TypeOperationResult<'db, ImplicitDunderCallError<'db>>;
