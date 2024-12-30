use ruff_db::diagnostic::{DiagnosticId, Severity};
use ruff_python_ast as ast;

use crate::db::Db;

use super::{
    diagnostic::{
        report_possibly_unresolved_reference, report_unresolved_reference, CALL_NON_CALLABLE,
        CALL_POSSIBLY_NON_CALLABLE, POSSIBLY_UNBOUND_ATTRIBUTE, UNRESOLVED_ATTRIBUTE,
    },
    FunctionType, InferContext, Type, UnionType,
};

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

    pub(crate) fn or_fall_back_to<E2: TypeOperationError<'db>>(
        self,
        db: &'db dyn Db,
        op: impl FnOnce() -> TypeOperationResult<'db, E2>,
    ) -> Self {
        match self {
            Self::Ok(ty) => Self::Ok(ty),
            Self::Err(error) => error.or_fall_back_to(db, op),
        }
    }

    pub(crate) fn unwrap_or_unknown(self) -> Type<'db> {
        match self.map_err(|err| err.as_universal_error()) {
            TypeOperationResult::Ok(ty) => ty,
            TypeOperationResult::Err(UniversalError::PossibleError { fallback }) => fallback,
            TypeOperationResult::Err(UniversalError::DefiniteError) => Type::Unknown,
        }
    }
}

pub(crate) trait TypeOperationError<'db>: Sized {
    type AST;
    fn into_type(self, context: &InferContext, node: &'db Self::AST) -> Type<'db>;
    fn as_universal_error(&self) -> UniversalError<'db>;

    fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, fallback: F) -> TypeOperationResult<'db, Self>
    where
        E2: TypeOperationError<'db>,
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

    fn as_universal_error(&self) -> UniversalError<'db> {
        match self {
            NameLookupError::PossiblyUndefinedName { type_when_defined } => {
                UniversalError::PossibleError {
                    fallback: *type_when_defined,
                }
            }
            NameLookupError::UndefinedName => UniversalError::DefiniteError,
        }
    }

    fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, fallback: F) -> TypeOperationResult<'db, Self>
    where
        E2: TypeOperationError<'db>,
        F: FnOnce() -> TypeOperationResult<'db, E2>,
    {
        self.as_universal_error()
            .or_fall_back_to(db, || fallback().map_err(|err| err.as_universal_error()))
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

pub(crate) type NameLookupResult<'db> = TypeOperationResult<'db, NameLookupError<'db>>;

impl<'db> NameLookupResult<'db> {
    pub(crate) fn possibly_unbound(type_when_defined: Type<'db>) -> Self {
        Self::Err(NameLookupError::PossiblyUndefinedName { type_when_defined })
    }

    pub(crate) fn unbound() -> Self {
        Self::Err(NameLookupError::UndefinedName)
    }
}

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

    fn as_universal_error(&self) -> UniversalError<'db> {
        match &self.kind {
            MemberAccessErrorKind::PossiblyUndefinedMember { type_when_defined } => {
                UniversalError::PossibleError {
                    fallback: *type_when_defined,
                }
            }
            MemberAccessErrorKind::UndefinedMember => UniversalError::DefiniteError,
        }
    }

    fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, fallback: F) -> TypeOperationResult<'db, Self>
    where
        E2: TypeOperationError<'db>,
        F: FnOnce() -> TypeOperationResult<'db, E2>,
    {
        let self_as_universal_error = self.as_universal_error();
        let MemberAccessError {
            member,
            accessed_on,
            kind,
        } = self;
        self_as_universal_error
            .or_fall_back_to(db, || fallback().map_err(|err| err.as_universal_error()))
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

pub(crate) type MemberAccessResult<'db> = TypeOperationResult<'db, MemberAccessError<'db>>;

/// Is the type not callable because it does not have a `__call__` method,
/// or because its `__call__` method is not callable?
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum CallErrorReasonKind {
    NoDunderCallMethod,
    DunderCallMethodNotCallable,
}

impl CallErrorReasonKind {
    const fn as_str(self) -> &'static str {
        match self {
            Self::NoDunderCallMethod => "objects of that type have no `__call__` method",
            Self::DunderCallMethodNotCallable => {
                "the `__call__` attribute on objects of that type is not callable"
            }
        }
    }
}

/// For a single element in a union that was called, why was it not callable?
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallErrorReason<'db> {
    /// The element in the union that was not callable
    pub(crate) ty: Type<'db>,

    /// The reason why it was not callable
    pub(crate) kind: CallErrorReasonKind,
}

/// Enumeration of functions that are special-cased:
/// each of these results in a custom diagnostic being emitted when it is called.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(super) enum SpecialCasedFunction<'db> {
    RevealType { revealed_type: Type<'db> },
}

/// Does this error represent the fact that the type is:
/// (a) Not callable;
/// (b) Possibly not callable; or,
/// (c) A special case that requires special handling?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum CallDiagnosticKind<'db> {
    NotCallable,
    PossiblyNotCallable {
        return_type: Type<'db>,
    },
    SpecialCase {
        case: SpecialCasedFunction<'db>,
        return_type: Type<'db>,
    },
}

impl<'db> CallDiagnosticKind<'db> {
    const fn is_not_callable(self) -> bool {
        matches!(self, Self::NotCallable)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct CallDiagnostic<'db> {
    /// Does this error represent the fact that the type is:
    /// (a) Not callable;
    /// (b) Possibly not callable; or,
    /// (c) A special case that requires special handling?
    pub(super) kind: CallDiagnosticKind<'db>,

    /// What type was called that caused the error?
    pub(super) called_type: Type<'db>,

    /// Assuming that the error represents that the type is not callable,
    /// what are the reasons for that?
    ///
    /// If the called type is a union, this will contain multiple reasons;
    /// in the common case, however, there will be only one reason.
    pub(super) not_callable_reasons: smallvec::SmallVec<[CallErrorReason<'db>; 1]>,
}

impl<'db> TypeOperationError<'db> for CallDiagnostic<'db> {
    type AST = ast::AnyNodeRef<'db>;

    fn into_type(self, context: &InferContext, call_node: &ast::AnyNodeRef) -> Type<'db> {
        match self.kind {
            CallDiagnosticKind::NotCallable => {
                context.report_lint(
                    &CALL_NON_CALLABLE,
                    *call_node,
                    format_args!(
                        "Object of type {} is not callable because {}",
                        self.called_type.display(context.db()),
                        self.not_callable_reasons[0].kind.as_str(),
                    ),
                );
                Type::Unknown
            }
            CallDiagnosticKind::PossiblyNotCallable { return_type } => {
                for CallErrorReason { ty, kind } in self.not_callable_reasons {
                    context.report_lint(
                        &CALL_POSSIBLY_NON_CALLABLE,
                        *call_node,
                        format_args!(
                            "Object is possibly not callable due to to union element `{}` because {}",
                            ty.display(context.db()),
                            kind.as_str(),
                        ),
                    );
                }
                return_type
            }
            CallDiagnosticKind::SpecialCase { case, return_type } => {
                match case {
                    SpecialCasedFunction::RevealType { revealed_type } => context
                        .report_diagnostic(
                            *call_node,
                            DiagnosticId::RevealedType,
                            Severity::Info,
                            format_args!(
                                "Revealed type is `{}`",
                                revealed_type.display(context.db())
                            ),
                        ),
                }
                return_type
            }
        }
    }

    fn as_universal_error(&self) -> UniversalError<'db> {
        match &self.kind {
            CallDiagnosticKind::NotCallable { .. } => UniversalError::DefiniteError,
            CallDiagnosticKind::PossiblyNotCallable { return_type } => {
                UniversalError::PossibleError {
                    fallback: *return_type,
                }
            }
            CallDiagnosticKind::SpecialCase { return_type, .. } => UniversalError::PossibleError {
                fallback: *return_type,
            },
        }
    }

    fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, fallback: F) -> TypeOperationResult<'db, Self>
    where
        E2: TypeOperationError<'db>,
        F: FnOnce() -> TypeOperationResult<'db, E2>,
    {
        let self_as_universal_error = self.as_universal_error();
        let CallDiagnostic {
            kind,
            called_type,
            not_callable_reasons,
        } = self;
        self_as_universal_error
            .or_fall_back_to(db, || fallback().map_err(|err| err.as_universal_error()))
            .map_err(|err| match err {
                UniversalError::DefiniteError => CallDiagnostic {
                    kind,
                    called_type,
                    not_callable_reasons,
                },
                UniversalError::PossibleError { .. } => CallDiagnostic {
                    kind,
                    called_type,
                    not_callable_reasons,
                },
            })
    }
}

pub(crate) type CallResult<'db> = TypeOperationResult<'db, CallDiagnostic<'db>>;

impl<'db> CallResult<'db> {
    pub(crate) fn revealed(
        db: &'db dyn Db,
        function_ty: FunctionType<'db>,
        args: &[Type<'db>],
    ) -> Self {
        Self::Err(CallDiagnostic {
            kind: CallDiagnosticKind::SpecialCase {
                case: SpecialCasedFunction::RevealType {
                    revealed_type: args.first().copied().unwrap_or(Type::Unknown),
                },
                return_type: function_ty.signature(db).return_ty,
            },
            called_type: function_ty.into(),
            not_callable_reasons: smallvec::SmallVec::default(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ImplicitDunderCallError<'db> {
    /// The dunder method that was implicitly called.
    pub(crate) dunder: &'db str,

    /// The type on which an operation was performed that led to the dunder method
    /// being implicitly called.
    ///
    /// Note that this type is not necessarily the type that the dunder method was called on:
    /// the dunder method is looked up on the meta-type of the type that this operation was
    /// performed on.
    pub(crate) operation_on: Type<'db>,

    /// Any errors that came about as a result of accessing the dunder method on this type
    pub(crate) attribute_access_errors: Option<MemberAccessError<'db>>,

    /// Any errors that came about as a result of calling the accessed dunder method on this type
    pub(crate) call_errors: Option<CallDiagnostic<'db>>,

    pub(crate) special_case_diagnostics: smallvec::SmallVec<[SpecialCasedFunction<'db>; 1]>,

    /// The type to use as the evaluated result of this implicit dunder call
    pub(crate) fallback_return_ty: Type<'db>,
}

impl<'db> TypeOperationError<'db> for ImplicitDunderCallError<'db> {
    type AST = ast::AnyNodeRef<'db>;

    fn into_type(self, context: &InferContext, call_node: &'db ast::AnyNodeRef) -> Type<'db> {
        if let Some(attribute_access_error) = self.attribute_access_error {
            attribute_access_error.into_type(context, call_node);
        }

        if let Some(call_error) = self.call_error {
            call_error.into_type(context, call_node);
        }

        self.fallback_return_ty
    }

    fn as_universal_error(&self) -> UniversalError<'db> {
        if self
            .attribute_access_error
            .as_ref()
            .is_some_and(|attribute_error| attribute_error.kind.is_undefined_member())
        {
            return UniversalError::DefiniteError;
        }

        if self
            .call_error
            .as_ref()
            .is_some_and(|call_error| call_error.kind.is_not_callable())
        {
            return UniversalError::DefiniteError;
        }

        UniversalError::PossibleError {
            fallback: self.fallback_return_ty,
        }
    }

    fn or_fall_back_to<E2, F>(self, db: &'db dyn Db, fallback: F) -> TypeOperationResult<'db, Self>
    where
        E2: TypeOperationError<'db>,
        F: FnOnce() -> TypeOperationResult<'db, E2>,
    {
        let self_as_universal_error = self.as_universal_error();
        let ImplicitDunderCallError {
            dunder,
            operation_on,
            attribute_access_error,
            call_error,
            fallback_return_ty,
        } = self;
        self_as_universal_error
            .or_fall_back_to(db, || fallback().map_err(|err| err.as_universal_error()))
            .map_err(|err| match err {
                UniversalError::DefiniteError => ImplicitDunderCallError {
                    dunder,
                    operation_on,
                    attribute_access_error,
                    call_error,
                    fallback_return_ty,
                },
                UniversalError::PossibleError { fallback } => ImplicitDunderCallError {
                    dunder,
                    operation_on,
                    attribute_access_error,
                    call_error,
                    fallback_return_ty: fallback,
                },
            })
    }
}

pub(crate) type ImplicitDunderCallResult<'db> = TypeOperationResult<'db, ImplicitDunderCallError<'db>>;
