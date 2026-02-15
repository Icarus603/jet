//! Type marshalling between Jet and C

pub use crate::types::{CType, CTypeKind};
pub use crate::values::CValue;

/// Errors that can occur during marshalling
#[derive(Debug, Clone)]
pub enum MarshalError {
    UnsupportedConversion { from: String, to: String },
    OutOfRange { value: String, target_type: String },
    NullPointer,
    InvalidString,
}

impl std::fmt::Display for MarshalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsupportedConversion { from, to } => {
                write!(f, "unsupported conversion from {} to {}", from, to)
            }
            Self::OutOfRange { value, target_type } => {
                write!(f, "value {} is out of range for {}", value, target_type)
            }
            Self::NullPointer => write!(f, "null pointer encountered"),
            Self::InvalidString => write!(f, "invalid UTF-8 string"),
        }
    }
}

impl std::error::Error for MarshalError {}

/// Result type for marshalling
pub type MarshalResult<T> = Result<T, MarshalError>;

/// Marshaller for type conversions
#[derive(Debug, Default)]
pub struct Marshaller;

impl Marshaller {
    pub fn new() -> Self {
        Self
    }
}

/// Trait for marshalling Jet types to C
pub trait JetToCMarshaller<T> {
    /// Convert a value to C
    fn marshal_to_c(&self, value: T, target_type: &CType) -> MarshalResult<CValue>;
}

/// Trait for marshalling C types to Jet
pub trait CToJetMarshaller<T> {
    /// Convert a value from C
    fn marshal_from_c(&self, value: &CValue) -> MarshalResult<T>;
}

impl JetToCMarshaller<i64> for Marshaller {
    fn marshal_to_c(&self, value: i64, target_type: &CType) -> MarshalResult<CValue> {
        match target_type.kind() {
            CTypeKind::Int8 => {
                if value >= i8::MIN as i64 && value <= i8::MAX as i64 {
                    Ok(CValue::int8(value as i8))
                } else {
                    Err(MarshalError::OutOfRange {
                        value: value.to_string(),
                        target_type: "int8".to_string(),
                    })
                }
            }
            CTypeKind::Int16 => {
                if value >= i16::MIN as i64 && value <= i16::MAX as i64 {
                    Ok(CValue::int16(value as i16))
                } else {
                    Err(MarshalError::OutOfRange {
                        value: value.to_string(),
                        target_type: "int16".to_string(),
                    })
                }
            }
            CTypeKind::Int32 => {
                if value >= i32::MIN as i64 && value <= i32::MAX as i64 {
                    Ok(CValue::int32(value as i32))
                } else {
                    Err(MarshalError::OutOfRange {
                        value: value.to_string(),
                        target_type: "int32".to_string(),
                    })
                }
            }
            CTypeKind::Int64 => Ok(CValue::int64(value)),
            _ => Err(MarshalError::UnsupportedConversion {
                from: "Jet int".to_string(),
                to: format!("{:?}", target_type.kind()),
            }),
        }
    }
}

impl CToJetMarshaller<i64> for Marshaller {
    fn marshal_from_c(&self, value: &CValue) -> MarshalResult<i64> {
        match value {
            CValue::Int8(v) => Ok(*v as i64),
            CValue::Int16(v) => Ok(*v as i64),
            CValue::Int32(v) => Ok(*v as i64),
            CValue::Int64(v) => Ok(*v),
            CValue::UInt8(v) => Ok(*v as i64),
            CValue::UInt16(v) => Ok(*v as i64),
            CValue::UInt32(v) => Ok(*v as i64),
            CValue::UInt64(v) => {
                if *v <= i64::MAX as u64 {
                    Ok(*v as i64)
                } else {
                    Err(MarshalError::OutOfRange {
                        value: v.to_string(),
                        target_type: "Jet int".to_string(),
                    })
                }
            }
            _ => Err(MarshalError::UnsupportedConversion {
                from: format!("{:?}", value),
                to: "Jet int".to_string(),
            }),
        }
    }
}

/// Unified type marshaller trait
pub trait TypeMarshaller<T> {
    fn marshal_to_c(&self, value: T, target_type: &CType) -> MarshalResult<CValue>;
    fn marshal_from_c(&self, value: &CValue) -> MarshalResult<T>;
}

impl TypeMarshaller<i64> for Marshaller {
    fn marshal_to_c(&self, value: i64, target_type: &CType) -> MarshalResult<CValue> {
        JetToCMarshaller::marshal_to_c(self, value, target_type)
    }

    fn marshal_from_c(&self, value: &CValue) -> MarshalResult<i64> {
        CToJetMarshaller::marshal_from_c(self, value)
    }
}
