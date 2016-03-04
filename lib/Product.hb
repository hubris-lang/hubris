module Product

import Unit

inductive Prod (A : Type) (B : Type)
  | MkProd : A -> B -> Prod A B
end

def first {A B : Type} (p : Prod A B) : A :=
  Prod.rec (fun (_ : Prod A B) => A) (fun (a : A) (b : B) => a) p
 end
