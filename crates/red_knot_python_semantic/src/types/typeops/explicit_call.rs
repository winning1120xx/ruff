use super::{TypeOperationError, TypeOperationResult, UniversalError};
use crate::types::diagnostic::{CALL_NON_CALLABLE, CALL_POSSIBLY_NON_CALLABLE};
use crate::types::{InferContext, Type};
use ruff_db::diagnostic::{DiagnosticId, Severity};
use ruff_python_ast as ast;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum NotCallableReason<'db> {
    NoDunderCallMethod,
    DunderCallMethodNotCallable { dunder_call_type: Type<'db> },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum ExplicitCallErrorReason<'db> {
    NotCallable { reason: NotCallableReason<'db> },
    CalledWithIncorrectArguments,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum SpecialCasedFunction<'db> {
    RevealType { revealed: Type<'db> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExplicitCallErrorKind<'db> {
    DefiniteError {
        reason: ExplicitCallErrorReason<'db>,
    },
    PossibleError {
        error_reasons: smallvec::SmallVec<[(Type<'db>, ExplicitCallErrorReason<'db>); 1]>,
        function: Option<SpecialCasedFunction<'db>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExplicitCallError<'db> {
    pub(crate) callee: Type<'db>,
    pub(crate) kind: ExplicitCallErrorKind<'db>,
    pub(crate) fallback_return_type: Type<'db>,
}

impl<'db> TypeOperationError<'db> for ExplicitCallError<'db> {
    type AST = ast::ExprCall;

    fn into_type(self, context: &InferContext, call_node: &ast::ExprCall) -> Type<'db> {
        let db = context.db();
        let ExplicitCallError {
            callee,
            kind,
            fallback_return_type,
        } = self;

        match kind {
            ExplicitCallErrorKind::DefiniteError { reason } => {
                match reason {
                    ExplicitCallErrorReason::NotCallable {
                        reason,
                    } => match reason {
                        NotCallableReason::NoDunderCallMethod => context.report_lint(
                            &CALL_NON_CALLABLE,
                            call_node.into(),
                            format_args!(
                                "Object of type {} is not callable as it has no `__call__` method",
                                callee.display(db)
                            ),
                        ),
                        NotCallableReason::DunderCallMethodNotCallable {dunder_call_type} => context.report_lint(
                            &CALL_NON_CALLABLE,
                            call_node.into(),
                            format_args!(
                                "Object of type {callee_type} is not callable as its `__call__` method has type {dunder_call_type}, which is not callable",
                                callee_type = callee.display(db),
                                dunder_call_type = dunder_call_type.display(db),
                            ),
                        ),
                    },
                    ExplicitCallErrorReason::CalledWithIncorrectArguments => {
                        todo!("Diagnostics for incorrect arguments")
                    }
                }
            }
            ExplicitCallErrorKind::PossibleError {
                error_reasons,
                function,
            } => {
                for (ty, reason) in error_reasons {
                    match reason {
                        ExplicitCallErrorReason::NotCallable {
                            reason,
                        } => match reason {
                            NotCallableReason::NoDunderCallMethod => context.report_lint(
                                &CALL_POSSIBLY_NON_CALLABLE,
                                call_node.into(),
                                format_args!(
                                    "Object of type {callee_type} may not be callable as objects of type {ty} have no `__call__` method",
                                    callee_type = callee.display(db),
                                    ty = ty.display(db),
                                ),
                            ),
                            NotCallableReason::DunderCallMethodNotCallable { dunder_call_type } => context.report_lint(
                                &CALL_POSSIBLY_NON_CALLABLE,
                                call_node.into(),format_args!(
                                    "Object of type {callee} may not be callable as the `__call__` method for objects of type {ty} has type {dunder_call_ty}, which is not callable",
                                    callee = callee.display(db),
                                    ty = ty.display(db),
                                    dunder_call_ty = dunder_call_type.display(db)
                                ),
                            ),
                        }
                        ExplicitCallErrorReason::CalledWithIncorrectArguments => {
                            todo!("Diagnostics for incorrect arguments")
                        }
                    }
                }
                if let Some(special_cased_function) = function {
                    match special_cased_function {
                        SpecialCasedFunction::RevealType { revealed } => {
                            context.report_diagnostic(
                                call_node.into(),
                                DiagnosticId::RevealedType,
                                Severity::Info,
                                format_args!(
                                    "Revealed type is `{}`",
                                    revealed.display(context.db())
                                ),
                            );
                        }
                    }
                }
            }
        }

        fallback_return_type
    }

    fn or_fall_back_to<E2, F>(
        self,
        db: &'db dyn crate::Db,
        fallback: F,
    ) -> TypeOperationResult<'db, Self>
    where
        for<'a> &'a E2: Into<UniversalError<'db>>,
        F: FnOnce() -> super::TypeOperationResult<'db, E2>,
    {
        let self_as_universal_error = UniversalError::from(&self);
        let ExplicitCallError {
            callee,
            kind,
            fallback_return_type,
        } = self;
        self_as_universal_error
            .or_fall_back_to(db, || fallback().map_err(|err| (&err).into()))
            .map_err(|err| match err {
                UniversalError::DefiniteError => ExplicitCallError {
                    callee,
                    kind,
                    fallback_return_type,
                },
                UniversalError::PossibleError { fallback } => ExplicitCallError {
                    callee,
                    kind,
                    fallback_return_type: fallback,
                },
            })
    }
}

impl<'db> From<&ExplicitCallError<'db>> for UniversalError<'db> {
    fn from(err: &ExplicitCallError<'db>) -> Self {
        match &err.kind {
            ExplicitCallErrorKind::DefiniteError { .. } => UniversalError::DefiniteError,
            ExplicitCallErrorKind::PossibleError { .. } => UniversalError::PossibleError {
                fallback: err.fallback_return_type,
            },
        }
    }
}

pub(crate) type ExplicitCallResult<'db> = TypeOperationResult<'db, ExplicitCallError<'db>>;
