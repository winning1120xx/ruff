use crate::db::Db;
use super::{InferContext, Type, UnionType};
pub(crate) use name_lookup::{NameLookupError, NameLookupResult};
pub(crate) use member_access::{MemberAccessError, MemberAccessErrorKind, MemberAccessResult};
pub(crate) use explicit_call::{ExplicitCallError, ExplicitCallResult};
pub(crate) use implicit_dunder_call::{ImplicitDunderCallError, ImplicitDunderCallResult};

mod explicit_call;
mod implicit_dunder_call;
mod member_access;
mod name_lookup;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeOperationResult<'db, E> {
    Ok(Type<'db>),
    Err(E),
}

impl<'db, E> TypeOperationResult<'db, E> {
    pub(crate) fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> TypeOperationResult<'db, E2> {
        match self {
            Self::Ok(ty) => TypeOperationResult::Ok(ty),
            Self::Err(error) => TypeOperationResult::Err(f(error)),
        }
    }

    pub(crate) const fn is_ok(&self) -> bool {
        matches!(self, Self::Ok(_))
    }

    pub(crate) const fn is_err(&self) -> bool {
        matches!(self, Self::Err(_))
    }
}

impl<'db, E> TypeOperationResult<'db, E>
where
    E: TypeOperationError<'db>,
{
    pub(crate) fn unwrap_with_diagnostic(
        self,
        context: &InferContext,
        node: &'db E::AST,
    ) -> Type<'db> {
        match self {
            Self::Ok(ty) => ty,
            Self::Err(error) => error.into_type(context, node),
        }
    }

    pub(crate) fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, op: F) -> Self
    where
        for<'a> &'a E2: Into<UniversalError<'db>>,
        F: FnOnce() -> TypeOperationResult<'db, E2>,
    {
        match self {
            Self::Ok(ty) => Self::Ok(ty),
            Self::Err(error) => error.or_fall_back_to(db, op),
        }
    }
}

impl<'db, E> TypeOperationResult<'db, E>
where
    for<'a> &'a E: Into<UniversalError<'db>>,
{
    pub(crate) fn unwrap_or_unknown(self) -> Type<'db> {
        match self.map_err(|err| (&err).into()) {
            TypeOperationResult::Ok(ty) => ty,
            TypeOperationResult::Err(UniversalError::PossibleError { fallback }) => fallback,
            TypeOperationResult::Err(UniversalError::DefiniteError) => Type::Unknown,
        }
    }
}

pub(crate) trait TypeOperationError<'db>: Sized {
    type AST;

    fn into_type(self, context: &InferContext, node: &'db Self::AST) -> Type<'db>;

    fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, fallback: F) -> TypeOperationResult<'db, Self>
    where
        for<'a> &'a E2: Into<UniversalError<'db>>,
        F: FnOnce() -> TypeOperationResult<'db, E2>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UniversalError<'db> {
    DefiniteError,
    PossibleError { fallback: Type<'db> },
}

impl<'db> UniversalError<'db> {
    fn or_fall_back_to(
        self,
        db: &'db dyn Db,
        fallback: impl FnOnce() -> TypeOperationResult<'db, Self>,
    ) -> TypeOperationResult<'db, Self> {
        let fallback = fallback();
        match (self, &fallback) {
            (Self::DefiniteError, _) => fallback,
            (_, TypeOperationResult::Err(Self::DefiniteError)) => TypeOperationResult::Err(self),
            (
                Self::PossibleError {
                    fallback: self_type,
                },
                TypeOperationResult::Ok(fallback),
            ) => TypeOperationResult::Ok(UnionType::from_elements(db, [self_type, *fallback])),
            (
                Self::PossibleError {
                    fallback: self_type,
                },
                TypeOperationResult::Err(Self::PossibleError { fallback }),
            ) => TypeOperationResult::Err(Self::PossibleError {
                fallback: UnionType::from_elements(db, [self_type, *fallback]),
            }),
        }
    }
}
