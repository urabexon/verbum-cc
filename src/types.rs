#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Type {
    Int,
    Char,
    Void,
    Ptr(Box<Type>),
}

#[allow(dead_code)]
impl Type {
    pub fn size(&self) -> i64 {
        match self {
            Type::Int => 8,
            Type::Char => 1,
            Type::Void => 0,
            Type::Ptr(_) => 8,
        }
    }

    pub fn is_compatible(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Int, Type::Int) => true,
            (Type::Char, Type::Char) => true,
            (Type::Void, Type::Void) => true,
            (Type::Ptr(a), Type::Ptr(b)) => a.is_compatible(b),
            (Type::Int, Type::Char) | (Type::Char, Type::Int) => true,
            (Type::Ptr(_), Type::Int) | (Type::Int, Type::Ptr(_)) => true,
            _ => false,
        }
    }

    pub fn base_type(&self) -> Option<&Type> {
        match self {
            Type::Ptr(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }
}
