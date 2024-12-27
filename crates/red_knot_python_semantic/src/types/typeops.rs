use std::ops::{Deref, DerefMut};

use ruff_python_ast as ast;

use super::{
    diagnostic::{
        report_possibly_unresolved_reference, report_unresolved_reference, CALL_NON_CALLABLE,
        POSSIBLY_UNBOUND_ATTRIBUTE, UNRESOLVED_ATTRIBUTE,
    },
    InferContext, Type,
};

#[derive(Debug, Copy, Clone, PartialEq)]
struct TypeOperationResult<'db, E, AST>(Result<Type<'db>, TypeOperationError<'db, E, AST>>);

impl<'db, E, AST> TypeOperationResult<'db, E, AST>
where
    E: TypeOperationErrorInner<'db, AST>,
{
    fn as_result(&self) -> Result<Type<'db>, &TypeOperationError<'db, E, AST>> {
        self.0.as_ref().map(Type::clone)
    }

    fn unwrap_with_diagnostic(self, context: &mut InferContext) -> Type<'db> {
        match self.0 {
            Ok(ty) => ty,
            Err(error) => error.into_type(context),
        }
    }
}

impl<'db> TypeOperationResult<'db, NameLookupError<'db>, ast::ExprName> {
    fn member(
        self,
        context: &mut InferContext,
        member: &str,
    ) -> TypeOperationResult<'db, MemberAccessError<'db>, ast::AnyNodeRef<'db>> {
        match self.0 {
            Ok(ty) => ty.member(context.db(), member),
            Err(error) => error.into_type(context).member(context.db(), member),
        }
    }

    fn call(
        self,
        context: &mut InferContext,
        call_args: &[Type<'db>],
    ) -> TypeOperationResult<'db, CallError<'db>, ast::AnyNodeRef<'db>> {
        match self.0 {
            Ok(ty) => ty.call(context.db(), call_args),
            Err(error) => error.into_type(context).call(context.db(), call_args),
        }
    }
}

impl<'db, E, AST> Deref for TypeOperationResult<'db, E, AST> {
    type Target = Result<Type<'db>, TypeOperationError<'db, E, AST>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<E, AST> DerefMut for TypeOperationResult<'_, E, AST> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct TypeOperationError<'db, E, AST> {
    node: &'db AST,
    inner: E,
}

impl<'db, E, AST> TypeOperationError<'db, E, AST>
where
    E: TypeOperationErrorInner<'db, AST>,
{
    fn into_type(self, context: &mut InferContext) -> Type<'db> {
        self.inner.into_type(context, self.node)
    }
}

impl<E, AST> std::fmt::Display for TypeOperationError<'_, E, AST>
where
    E: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl<E, AST> std::error::Error for TypeOperationError<'_, E, AST>
where
    E: std::fmt::Display + std::fmt::Debug,
    AST: std::fmt::Debug,
{
}

trait TypeOperationErrorInner<'db, AST> {
    fn into_type(self, context: &mut InferContext, node: &'db AST) -> Type<'db>;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum NameLookupError<'db> {
    PossiblyUndefinedName { type_when_defined: Type<'db> },
    UndefinedName,
}

impl<'db> TypeOperationErrorInner<'db, ast::ExprName> for NameLookupError<'db> {
    fn into_type(self, context: &mut InferContext, node: &ast::ExprName) -> Type<'db> {
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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum MemberAccessErrorKind<'db> {
    PossiblyDefinedMember { type_when_defined: Type<'db> },
    UndefinedMember,
}

struct MemberAccessError<'db> {
    member: String,
    accessed_on: Type<'db>,
    kind: MemberAccessErrorKind<'db>,
}

impl<'db> TypeOperationErrorInner<'db, ast::AnyNodeRef<'db>> for MemberAccessError<'db> {
    fn into_type(self, context: &mut InferContext, attribute_node: &ast::AnyNodeRef) -> Type<'db> {
        match self.kind {
            MemberAccessErrorKind::PossiblyDefinedMember { type_when_defined } => {
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
}

/// 
enum CallErrorKind<'db> {
    NotCallable,
    PossiblyNotCallable {
        not_callable_union_element: Type<'db>,
        return_type: Type<'db>,
    },
}

struct CallError<'db> {
    kind: CallErrorKind<'db>,
    called_type: Type<'db>,
}

impl<'db> TypeOperationErrorInner<'db, ast::AnyNodeRef<'db>> for CallError<'db> {
    fn into_type(self, context: &mut InferContext, call_node: &ast::AnyNodeRef) -> Type<'db> {
        match self.kind {
            CallErrorKind::NotCallable => {
                context.report_lint(
                    &CALL_NON_CALLABLE,
                    *call_node,
                    format_args!(
                        "Object of type {} is not callable",
                        self.called_type.display(context.db())
                    ),
                );
                Type::Unknown
            }
            CallError::PossiblyNotCallable { type_when_callable } => {
                context.report_lint(
                    &POSSIBLY_UNBOUND_ATTRIBUTE,
                    *call_node,
                    format_args!(
                        "Object is possibly callable with type `{}`",
                        type_when_callable.display(context.db()),
                    ),
                );
                type_when_callable
            }
        }
    }
}
