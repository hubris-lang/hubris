use super::ty::Type;

use std::collections::HashMap;
use llvm_sys;
use llvm_sys::prelude::*;

pub struct Struct {
    field_map: HashMap<String, u32>,
    ty_ref: LLVMTypeRef,
}

impl Struct {
    fn new(attrs: Vec<(String, Type)>) -> Struct {
        // let mut field_map = HashMap::new();
        let mut types = panic!();
    }

    fn to_type() -> Type {
        panic!()
    }
}
