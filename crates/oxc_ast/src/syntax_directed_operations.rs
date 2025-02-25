//! [ECMA262 Syntax-Directed Operations](https://tc39.es/ecma262/#sec-syntax-directed-operations)

use oxc_span::Span;

#[allow(clippy::wildcard_imports)]
use crate::ast::*;

/// [`BoundName`](https://tc39.es/ecma262/#sec-static-semantics-boundnames)
pub trait BoundName {
    fn bound_name<F: FnMut(&BindingIdentifier)>(&self, f: &mut F);
}

pub trait BoundNames {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F);
}

impl<'a> BoundNames for BindingPattern<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        match &self.kind {
            BindingPatternKind::BindingIdentifier(ident) => ident.bound_names(f),
            BindingPatternKind::ArrayPattern(array) => array.bound_names(f),
            BindingPatternKind::ObjectPattern(object) => object.bound_names(f),
            BindingPatternKind::AssignmentPattern(assignment) => assignment.bound_names(f),
        }
    }
}

impl BoundNames for BindingIdentifier {
    fn bound_names<F: FnMut(&Self)>(&self, f: &mut F) {
        f(self);
    }
}

impl<'a> BoundNames for ArrayPattern<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        for elem in self.elements.iter().flatten() {
            elem.bound_names(f);
        }
        if let Some(rest) = &self.rest {
            rest.bound_names(f);
        }
    }
}

impl<'a> BoundNames for ObjectPattern<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        for p in &self.properties {
            p.value.bound_names(f);
        }
        if let Some(rest) = &self.rest {
            rest.bound_names(f);
        }
    }
}

impl<'a> BoundNames for AssignmentPattern<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        self.left.bound_names(f);
    }
}

impl<'a> BoundNames for RestElement<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        self.argument.bound_names(f);
    }
}

impl<'a> BoundNames for FormalParameters<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        for item in &self.items {
            item.bound_names(f);
        }
        if let Some(rest) = &self.rest {
            rest.bound_names(f);
        }
    }
}

impl<'a> BoundNames for Declaration<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        match self {
            Declaration::VariableDeclaration(decl) => decl.bound_names(f),
            Declaration::FunctionDeclaration(func) => func.bound_names(f),
            Declaration::ClassDeclaration(decl) => decl.bound_names(f),
            _ => {}
        }
    }
}

impl<'a> BoundNames for VariableDeclaration<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        for declarator in &self.declarations {
            declarator.id.bound_names(f);
        }
    }
}

impl<'a> BoundName for Function<'a> {
    fn bound_name<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        if let Some(ident) = &self.id {
            f(ident);
        }
    }
}

impl<'a> BoundNames for Function<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        self.bound_name(f);
    }
}

impl<'a> BoundName for Class<'a> {
    fn bound_name<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        if let Some(ident) = &self.id {
            f(ident);
        }
    }
}

impl<'a> BoundNames for Class<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        self.bound_name(f);
    }
}

impl<'a> BoundNames for FormalParameter<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        self.pattern.bound_names(f);
    }
}

impl<'a> BoundNames for ModuleDeclaration<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        if let ModuleDeclaration::ImportDeclaration(decl) = &self {
            decl.bound_names(f);
        }
    }
}

impl<'a> BoundNames for ImportDeclaration<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        self.specifiers.iter().for_each(|specifier| match specifier {
            ImportDeclarationSpecifier::ImportSpecifier(specifier) => f(&specifier.local),
            ImportDeclarationSpecifier::ImportDefaultSpecifier(specifier) => f(&specifier.local),
            ImportDeclarationSpecifier::ImportNamespaceSpecifier(specifier) => f(&specifier.local),
        });
    }
}

impl<'a> BoundNames for ExportNamedDeclaration<'a> {
    fn bound_names<F: FnMut(&BindingIdentifier)>(&self, f: &mut F) {
        if let Some(decl) = &self.declaration {
            decl.bound_names(f);
        }
    }
}

/// [`IsSimpleParameterList`](https://tc39.es/ecma262/#sec-static-semantics-issimpleparameterlist)
pub trait IsSimpleParameterList {
    fn is_simple_parameter_list(&self) -> bool;
}

impl<'a> IsSimpleParameterList for FormalParameters<'a> {
    fn is_simple_parameter_list(&self) -> bool {
        self.items.iter().all(|pat| pat.pattern.kind.is_binding_identifier()) && self.rest.is_none()
    }
}

/// [`PropName`](https://tc39.es/ecma262/#sec-static-semantics-propname)
pub trait PropName {
    fn prop_name(&self) -> Option<(&str, Span)>;
}

impl<'a> PropName for ObjectPropertyKind<'a> {
    fn prop_name(&self) -> Option<(&str, Span)> {
        match self {
            ObjectPropertyKind::ObjectProperty(prop) => prop.prop_name(),
            ObjectPropertyKind::SpreadProperty(_) => None,
        }
    }
}

impl<'a> PropName for ObjectProperty<'a> {
    fn prop_name(&self) -> Option<(&str, Span)> {
        if self.kind != PropertyKind::Init || self.method || self.shorthand || self.computed {
            return None;
        }
        self.key.prop_name()
    }
}

impl<'a> PropName for PropertyKey<'a> {
    fn prop_name(&self) -> Option<(&str, Span)> {
        match self {
            PropertyKey::Identifier(ident) => Some((&ident.name, ident.span)),
            PropertyKey::PrivateIdentifier(_) => None,
            PropertyKey::Expression(expr) => match &expr {
                Expression::Identifier(ident) => Some((&ident.name, ident.span)),
                Expression::StringLiteral(lit) => Some((&lit.value, lit.span)),
                _ => None,
            },
        }
    }
}

impl<'a> PropName for ClassElement<'a> {
    fn prop_name(&self) -> Option<(&str, Span)> {
        match self {
            ClassElement::MethodDefinition(def) => def.prop_name(),
            ClassElement::TSAbstractMethodDefinition(def) => def.method_definition.prop_name(),
            ClassElement::PropertyDefinition(def) => def.prop_name(),
            ClassElement::TSAbstractPropertyDefinition(def) => def.property_definition.prop_name(),
            _ => None,
        }
    }
}

impl<'a> PropName for MethodDefinition<'a> {
    fn prop_name(&self) -> Option<(&str, Span)> {
        if self.computed {
            return None;
        }
        self.key.prop_name()
    }
}

impl<'a> PropName for PropertyDefinition<'a> {
    fn prop_name(&self) -> Option<(&str, Span)> {
        if self.computed {
            return None;
        }
        self.key.prop_name()
    }
}

/// [`PrivateBoundIdentifiers`](https://tc39.es/ecma262/#sec-static-semantics-privateboundidentifiers)
pub trait PrivateBoundIdentifiers {
    fn private_bound_identifiers(&self) -> Option<PrivateIdentifier>;
}

impl<'a> PrivateBoundIdentifiers for ClassElement<'a> {
    fn private_bound_identifiers(&self) -> Option<PrivateIdentifier> {
        match self {
            ClassElement::StaticBlock(_) | ClassElement::TSIndexSignature(_) => None,
            ClassElement::MethodDefinition(def) => def.private_bound_identifiers(),
            ClassElement::PropertyDefinition(def) => def.private_bound_identifiers(),
            ClassElement::AccessorProperty(def) => def.private_bound_identifiers(),
            ClassElement::TSAbstractMethodDefinition(def) => {
                def.method_definition.private_bound_identifiers()
            }
            ClassElement::TSAbstractPropertyDefinition(def) => {
                def.property_definition.private_bound_identifiers()
            }
        }
    }
}

impl<'a> PrivateBoundIdentifiers for MethodDefinition<'a> {
    fn private_bound_identifiers(&self) -> Option<PrivateIdentifier> {
        self.value.body.as_ref()?;
        if let PropertyKey::PrivateIdentifier(ident) = &self.key {
            return Some((*ident).clone());
        }
        None
    }
}

impl<'a> PrivateBoundIdentifiers for PropertyDefinition<'a> {
    fn private_bound_identifiers(&self) -> Option<PrivateIdentifier> {
        if let PropertyKey::PrivateIdentifier(ident) = &self.key {
            return Some((*ident).clone());
        }
        None
    }
}

impl<'a> PrivateBoundIdentifiers for AccessorProperty<'a> {
    fn private_bound_identifiers(&self) -> Option<PrivateIdentifier> {
        if let PropertyKey::PrivateIdentifier(ident) = &self.key {
            return Some((*ident).clone());
        }
        None
    }
}
