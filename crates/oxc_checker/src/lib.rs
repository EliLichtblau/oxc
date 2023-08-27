use std::{cell::RefCell, rc::Rc, any::Any};
use oxc_ast::{ast::{Program, Statement, Expression, Declaration, VariableDeclaration, ObjectExpression, TSType, VariableDeclarator, TSTypeAliasDeclaration, TSTypeReference}, AstKind};
use oxc_diagnostics::DiagnosticService;
use oxc_semantic::{Semantic, AstNode, SymbolId, SymbolFlags};
use oxc_span::Span;
use oxc_index::IndexVec;


use rustc_hash::FxHashMap;
use types::{ObjectType, Assignable};

use crate::types::ToString;
mod types;
mod errors;
type Error = errors::CheckerError;
type ErrorKind = errors::ErrorKind;

type TypeId = types::TypeId;
pub struct Checker<'a> {
    semantic: Semantic<'a>,
    diagnostics: RefCell<Vec<Error>>,
    
    symbol_id_to_type: IndexVec<SymbolId, TypeId>,
    /*
     * TypeID exists for the following reason:
     * Take: const foo = {obj: 3}
     * How should this be stored: Intuitively its <string, Type>
     * But this is very expensive so ideally its
     * <Atom, SymbolId>, but 3 isn't a symbol but it does have a type
     * so we need 1 more abstraction
     * <Atom, TypeId>
     */
    types: IndexVec<TypeId, Type>,


    // built in types 
    any_type: TypeId,
    string_type: TypeId,
    number_type: TypeId,
    error_type: TypeId
    

}



type Type = types::Type;
type PrimitiveType = types::PrimitiveType;
// type AnyType = Type::PrimitiveType(PrimitiveType::Any);
// type NumberType = Type::PrimitiveType(PrimitiveType::Number);
// type StringType = Type::PrimitiveType(PrimitiveType::Stirng);


impl<'a> Checker<'a> {

    pub fn new(semantic: Semantic<'a>) -> Self {
        let mut types: IndexVec<TypeId, Type> = vec![].into();
        let mut symbol_id_to_type: IndexVec<SymbolId, TypeId> = vec![].into();

        assert!(semantic.symbols().spans.len() == semantic.symbols().names.len());
        // First add all symbols with Any Type
        for _ in 0..semantic.symbols().spans.len() {
            symbol_id_to_type.push(0.into());
        }
        // Add built symbols
        let any_type = types.push(Type::PrimitiveType(PrimitiveType::Any));
        let string_type = types.push(Type::PrimitiveType(PrimitiveType::String));
        let number_type = types.push(Type::PrimitiveType(PrimitiveType::Number));
        let error_type = types.push(Type::PrimitiveType(PrimitiveType::Error));


        Self {semantic, diagnostics: vec![].into(), symbol_id_to_type: symbol_id_to_type, types: types,
            any_type,
            string_type,
            number_type,
            error_type
        }
    }
    

    fn zero_span(&self) -> Span {
        return Span {start: 0, end: 0}
    }
    

    pub fn check(&mut self, program: &Program) {
        //println!("Checking!");
        for stmt in program.body.iter() {
            self.check_statement(stmt);
        };
        
        for diag in self.diagnostics.borrow().iter() {
            println!("{:?}", diag.kind)
        } 

    }

    fn is_symbol_checked(&self, symbol_id: SymbolId) -> bool {
        self.semantic.symbols().get_flag(symbol_id).is_checked()
    }
    fn set_symbol_checked(&mut self, symbol_id: SymbolId) {
        self.semantic.symbols_mut().union_flag(symbol_id, SymbolFlags::Checked);
    }

    fn check_statement(&mut self, statement: &Statement) -> TypeId {
        //println!("Check statement");
        match statement {
            Statement::BlockStatement(_) => todo!(),
            Statement::BreakStatement(_) => todo!(),
            Statement::ContinueStatement(_) => todo!(),
            Statement::DebuggerStatement(_) => todo!(),
            Statement::DoWhileStatement(_) => todo!(),
            Statement::EmptyStatement(_) => todo!(),
            Statement::ExpressionStatement(expr) => self.check_expression(&expr.expression),
            Statement::ForInStatement(_) => todo!(),
            Statement::ForOfStatement(_) => todo!(),
            Statement::ForStatement(_) => todo!(),
            Statement::IfStatement(_) => todo!(),
            Statement::LabeledStatement(_) => todo!(),
            Statement::ReturnStatement(ret) => if let Some(expr) = &ret.argument {self.check_expression(expr)} else {self.any_type},
            Statement::SwitchStatement(_) => todo!(),
            Statement::ThrowStatement(_) => todo!(),
            Statement::TryStatement(_) => todo!(),
            Statement::WhileStatement(_) => todo!(),
            Statement::WithStatement(_) => todo!(),
            Statement::ModuleDeclaration(_) => todo!(),
            Statement::Declaration(decl) => self.check_declaration(decl),
        }
    }
    fn check_declaration(&mut self, declaration: &Declaration) -> TypeId {
        //println!("Check declaration");
        match declaration {
            Declaration::VariableDeclaration(decl) => self.check_variable_declaration(decl),
            Declaration::FunctionDeclaration(_) => todo!(),
            Declaration::ClassDeclaration(_) => todo!(),
            Declaration::TSTypeAliasDeclaration(type_alias) => self.check_type_alias(type_alias),
            Declaration::TSInterfaceDeclaration(_) => todo!(),
            Declaration::TSEnumDeclaration(_) => todo!(),
            Declaration::TSModuleDeclaration(_) => todo!(),
            Declaration::TSImportEqualsDeclaration(_) => todo!(),
        }
    }

    fn check_variable_declaration(&mut self, variable_declaration: &VariableDeclaration) -> TypeId {
        assert!(variable_declaration.declarations.len()==1);
        let declarator = variable_declaration.declarations.get(0).unwrap();
        return self.check_variable_declarator(declarator);
    }   

    fn check_type_alias(&mut self, type_alias: &TSTypeAliasDeclaration) -> TypeId {
        return self.check_type(&type_alias.type_annotation)
    }


    fn check_variable_declarator(&mut self, declarator: &VariableDeclarator) -> TypeId {
        let initializer_type = match &declarator.init {
            Some(expr) => self.check_expression(expr),
            None => self.any_type,
        };
        let type_ann = &declarator.id.type_annotation;
        match type_ann {
            Some(type_ann) => {
                let type_ann_type =  self.check_type(&type_ann.type_annotation);
                if !self.is_assignable_to(initializer_type, type_ann_type) {
                    //{initializer_type.to_string(), type_ann_type.to_string(), self.zero_span()}
                    self.diagnostics.borrow_mut().push(Error {
                        kind: ErrorKind::Type0IsNotAssignableToType1(errors::Type0IsNotAssignableToType1 {
                            lhs: initializer_type, 
                            rhs: type_ann_type, 
                            span: self.zero_span()
                        }
                        )
                    })
                }
                return type_ann_type
            },
            None => initializer_type,
        }
        
    }



    fn check_expression(&mut self, expr: &Expression) -> TypeId {
        match expr {
            Expression::BooleanLiteral(_) => todo!(),
            Expression::NullLiteral(_) => todo!(),
            Expression::NumberLiteral(_) => self.number_type,
            Expression::BigintLiteral(_) => todo!(),
            Expression::RegExpLiteral(_) => todo!(),
            Expression::StringLiteral(_) => self.string_type,
            Expression::TemplateLiteral(_) => todo!(),
            Expression::Identifier(identifier) => {
                match identifier.reference_id.get() {
                    Some(ref_id) => {
                        let reference: &oxc_semantic::Reference = self.semantic.symbols().get_reference(ref_id);
                        if let Some(symbol_id) = reference.symbol_id() {
                            if self.is_symbol_checked(symbol_id) {
                                return self.symbol_id_to_type[symbol_id]
                            }
                            return self.get_value_type_of_symbol(symbol_id);
                        } 
                        println!("Error could not resolve {:?}", identifier);
                        self.any_type                   
                    }
                    None => {
                        self.any_type
                    }
                }
                
            },
            Expression::MetaProperty(_) => todo!(),
            Expression::Super(_) => todo!(),
            Expression::ArrayExpression(_) => todo!(),
            Expression::ArrowExpression(_) => todo!(),
            Expression::AssignmentExpression(_) => todo!(),
            Expression::AwaitExpression(_) => todo!(),
            Expression::BinaryExpression(_) => todo!(),
            Expression::CallExpression(_) => todo!(),
            Expression::ChainExpression(_) => todo!(),
            Expression::ClassExpression(_) => todo!(),
            Expression::ConditionalExpression(_) => todo!(),
            Expression::FunctionExpression(_) => todo!(),
            Expression::ImportExpression(_) => todo!(),
            Expression::LogicalExpression(_) => todo!(),
            Expression::MemberExpression(_) => todo!(),
            Expression::NewExpression(_) => todo!(),
            Expression::ObjectExpression(_) => todo!(),
            Expression::ParenthesizedExpression(_) => todo!(),
            Expression::SequenceExpression(_) => todo!(),
            Expression::TaggedTemplateExpression(_) => todo!(),
            Expression::ThisExpression(_) => todo!(),
            Expression::UnaryExpression(_) => todo!(),
            Expression::UpdateExpression(_) => todo!(),
            Expression::YieldExpression(_) => todo!(),
            Expression::PrivateInExpression(_) => todo!(),
            Expression::JSXElement(_) => todo!(),
            Expression::JSXFragment(_) => todo!(),
            Expression::TSAsExpression(_) => todo!(),
            Expression::TSSatisfiesExpression(_) => todo!(),
            Expression::TSTypeAssertion(_) => todo!(),
            Expression::TSNonNullExpression(_) => todo!(),
            Expression::TSInstantiationExpression(_) => todo!(),
        }
    }


    fn get_value_type_of_symbol(&mut self, symbol_id:  SymbolId) -> TypeId {
        let declaration_id = self.semantic.symbols().get_declaration(symbol_id);
        let declaration = self.semantic.nodes().get_node(declaration_id).kind();
        // Todo aliasing
        let type_id = match declaration { 
            AstKind::VariableDeclaration(decl) => self.check_variable_declaration(decl),
            AstKind::VariableDeclarator(decl)=>self.check_variable_declarator(decl),
            AstKind::TSTypeAliasDeclaration(decl) => self.check_type_alias(decl),
           _ => {
                println!("{:?}", declaration);
                todo!()
           }
        };
        self.symbol_id_to_type[symbol_id] = type_id;
        self.set_symbol_checked(symbol_id);
        type_id
    }


    fn check_object(&mut self, object: &ObjectExpression) -> Type {
        let mut object_type = ObjectType {
            members: FxHashMap::default()
        };
        for property in &object.properties {
            match property {
                oxc_ast::ast::ObjectPropertyKind::ObjectProperty(prop) => {
                    let prop_name = match &prop.key {
                        oxc_ast::ast::PropertyKey::Identifier(id) => id.name.clone(),
                        oxc_ast::ast::PropertyKey::PrivateIdentifier(id) => id.name.clone(),
                        oxc_ast::ast::PropertyKey::Expression(_) => todo!(),
                    };
                    let prop_type = self.check_expression(&prop.value);
                    object_type.members.insert(prop_name, prop_type);
                },
                oxc_ast::ast::ObjectPropertyKind::SpreadProperty(_) => todo!(),
            }
        };
        Type::ObjectType(object_type)

    }

    fn is_assignable_to(&self, lhs: TypeId, rhs: TypeId) -> bool {
        return self.types[lhs].is_assignable_to(&self.types[rhs], &self.types)
    }
    

    // fn check_object(&self, object: &ObjectExpression) -> Type {
    //     let object_type = Type::ObjectType(ObjectType {
    //         members: FxHashMap::default()
    //     });
    //     for property in &object.properties {
    //         match property {
    //             oxc_ast::ast::ObjectPropertyKind::ObjectProperty(prop) => {
    //                 match &prop.key {
    //                     oxc_ast::ast::PropertyKey::Identifier(id) => {
    //                         // What do I do here
    //                         self.semantic.scopes().get_binding(, name)
    //                     },
    //                     oxc_ast::ast::PropertyKey::PrivateIdentifier(_) => todo!(),
    //                     oxc_ast::ast::PropertyKey::Expression(_) => todo!(),
    //                 };
    //             },
    //             oxc_ast::ast::ObjectPropertyKind::SpreadProperty(_) => todo!(),
    //         };
    //     }
    //     object_type
    // }

    fn check_type_reference(&mut self, type_reference: &TSTypeReference) -> TypeId {
        match &type_reference.type_name {
            oxc_ast::ast::TSTypeName::IdentifierReference(id) => {
                let Some(reference_id) = id.reference_id.get() else {todo!() };
                let reference = self.semantic.symbols().get_reference(reference_id);
                match reference.symbol_id() {
                    Some(symbol_id) => self.get_value_type_of_symbol(symbol_id),
                    None => todo!()
                }
            },
            oxc_ast::ast::TSTypeName::QualifiedName(_) => todo!(),
        }
    }

    fn check_type(&mut self, type_node: &TSType) -> TypeId {
            match type_node {
                TSType::TSAnyKeyword(_) => self.any_type,
                TSType::TSBigIntKeyword(_) => todo!(),
                TSType::TSBooleanKeyword(_) => todo!(),
                TSType::TSNeverKeyword(_) => todo!(),
                TSType::TSNullKeyword(_) => todo!(),
                TSType::TSNumberKeyword(_) => self.number_type,
                TSType::TSObjectKeyword(_) => todo!(),
                TSType::TSStringKeyword(_) => self.string_type,
                TSType::TSSymbolKeyword(_) => todo!(),
                TSType::TSThisKeyword(_) => todo!(),
                TSType::TSUndefinedKeyword(_) => todo!(),
                TSType::TSUnknownKeyword(_) => todo!(),
                TSType::TSVoidKeyword(_) => todo!(),
                TSType::TSArrayType(_) => todo!(),
                TSType::TSConditionalType(_) => todo!(),
                TSType::TSConstructorType(_) => todo!(),
                TSType::TSFunctionType(_) => todo!(),
                TSType::TSImportType(_) => todo!(),
                TSType::TSIndexedAccessType(_) => todo!(),
                TSType::TSInferType(_) => todo!(),
                TSType::TSIntersectionType(_) => todo!(),
                TSType::TSLiteralType(_) => todo!(),
                TSType::TSMappedType(_) => todo!(),
                TSType::TSQualifiedName(_) => todo!(),
                TSType::TSTemplateLiteralType(_) => todo!(),
                TSType::TSTupleType(_) => todo!(),
                TSType::TSTypeLiteral(_) => todo!(),
                TSType::TSTypeOperatorType(_) => todo!(),
                TSType::TSTypePredicate(_) => todo!(),
                TSType::TSTypeQuery(_) => todo!(),
                TSType::TSTypeReference(type_reference) => self.check_type_reference(&type_reference),
                TSType::TSUnionType(_) => todo!(),
                TSType::JSDocNullableType(_) => todo!(),
                TSType::JSDocUnknownType(_) => todo!(),
            }
    }
    // TODO: A meaningful implementation of this helper
    // fn get_symbol_at_location(&self, node: &AstNode) -> Option<SymbolId>{
    //     self.semantic.scopes().get_binding(node.scope_id(), name)
    // }

}
#[cfg(test)]
mod tests {
    use oxc_allocator::Allocator;
    use oxc_ast::{ast::VariableDeclarationKind, AstKind};
    use oxc_semantic::SemanticBuilder;
    use oxc_span::{Atom, SourceType, Language, ModuleKind, LanguageVariant};
    use oxc_parser::Parser;

    use super::*;

    // fn get_checker<'s, 'a: 's>(
    //     allocator: &'a Allocator,
    //     source: &'a str,
    //     source_type: SourceType
    // ) -> Checker<'a> {
    //     let parse: oxc_parser::ParserReturn<'_> = Parser::new(allocator, source, source_type).parse();
    //     assert!(parse.errors.is_empty());
    //     let program = allocator.alloc(parse.program);
    //     let semantic: oxc_semantic::SemanticBuilderReturn<'_> = SemanticBuilder::new(source, source_type).build(program);
    //     assert!(semantic.errors.is_empty());
    //     Checker { semantic: semantic.semantic, diagnostics: vec![].into() }
    // }

    fn check_source(source: &str) {
        let alloc = Allocator::default();
        let source_type = SourceType::default().with_typescript(true);
        let parse: oxc_parser::ParserReturn<'_> = Parser::new(&alloc, source, source_type).parse();
        let program = alloc.alloc(parse.program);
        let semantic = SemanticBuilder::new(source, source_type).build(program);
        assert!(semantic.errors.is_empty());
        let mut checker = Checker::new(semantic.semantic);
        checker.check(program)
    }

    // #[test]
    // fn interest() {
    //     let alloc = Allocator::default();
    //     let checker = get_checker(&alloc, "let foo = 1; let bar = 2", SourceType::default());
    //     checker.tmp()
    // }
    #[test]
    fn simple() {
        check_source("type alias = string; const s: alias = 1")
    }
}