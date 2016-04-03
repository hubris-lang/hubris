use std::mem::transmute;

struct Obj {
    data : *mut usize
}

impl Obj {
    fn as<T>(&self) -> &T {
        transmute(self.data)
    }
}

struct Prod {
    f1: Obj,
    f2: Obj,
    f2: Obj,
    f2: Obj
}

// fn Prod_main() {
//
// }

fn Prod_first(a1 : Obj, a2 : Obj, a3 : Obj) {
    let p = a3.as<Prod>();
    
}
