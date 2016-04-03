use std::rc::Rc;
use std::mem::transmute;

struct ObjValue {
    ptr: *mut usize,
}

pub struct Obj(Rc<ObjValue>);

impl Obj {
    pub fn from<T>(t: T) -> Obj {
        unsafe {
            let boxed_val = Box::new(t);

            let val = ObjValue {
                ptr: transmute(Box::into_raw(boxed_val)),
            };

            Obj(Rc::new(val))
        }
    }

    pub fn unbox<T>(&self) -> &T {
        let ptr: *mut usize = self.0.ptr;
        unsafe { transmute(ptr) }
    }
}
