use rustc_hash::FxHashMap;
use oxc_semantic::SymbolId;
use oxc_span::Atom;
use oxc_index::{define_index_type, IndexVec};

define_index_type! {
    pub struct TypeId = u32;
}


pub trait Assignable {
    fn is_assignable_to(&self, other: &Self, types: &IndexVec<TypeId, Type>) -> bool;
}

pub trait ToString {
    fn to_string(&self) -> String;
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum PrimitiveType {
    String,
    Number,
    Error,
    Any
}


pub enum LiteralType {
    Number(f64),
    String(String),
    Boolean(bool)
}

impl Assignable for PrimitiveType {
    fn is_assignable_to(&self, other: &Self, types: &IndexVec<TypeId, Type>) -> bool {
        match (self, other) {
            (PrimitiveType::Any, _) => true,
            (_, PrimitiveType::Any) => true,
            (PrimitiveType::Error, _) => true,
            (_, PrimitiveType::Error) => true,
            (a, b)=> a==b
        }
    }
}


impl ToString for PrimitiveType {
    fn to_string(&self) -> String {
        match self {
            PrimitiveType::String => "string".to_owned(),
            PrimitiveType::Number => "number".to_owned(),
            PrimitiveType::Error => "error".to_owned(),
            PrimitiveType::Any => "any".to_owned(),
        }
    }
}


#[derive(Clone, Debug)]
pub struct ObjectType {
    pub(crate) members: FxHashMap<Atom, TypeId>
}

impl Assignable for ObjectType {
    fn is_assignable_to(&self, other: &Self, types: &IndexVec<TypeId, Type>) -> bool {
        for key in other.members.keys() {
            let source_key_value = self.members.get_key_value(key);
            match source_key_value {
                Some((_, type_id)) => {
                    let t: Type = types[type_id].into();
                },
                None => {
                    return false
                }
            };
        };

        true
    }
}


#[derive(Clone, Debug)]
pub struct TypeVariable {
    pub(crate) name: Atom
}

#[derive(Clone, Debug)]
pub enum Type {
    PrimitiveType(PrimitiveType),
    ObjectType(ObjectType),
    TypeVariable(TypeVariable)
}

impl Assignable for Type {
    fn is_assignable_to(&self, other: &Self, types: &IndexVec<TypeId, Type>) -> bool {
        match (self, other) {
            (Type::PrimitiveType(a), Type::PrimitiveType(b)) => a.is_assignable_to(b, types),
            (Type::PrimitiveType(PrimitiveType::Any), _) => true,
            (Type::PrimitiveType(PrimitiveType::Error), _) => true,
            (_, Type::PrimitiveType(PrimitiveType::Any)) => true,
            (_, Type::PrimitiveType(PrimitiveType::Error)) => true,
            _ => false
        }   
    }
}




impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::PrimitiveType(prim) => prim.to_string(),
            _ => "TODO".to_owned()
        }
    }
}


