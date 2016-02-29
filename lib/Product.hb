module Product

import Unit

inductive Prod (A : Type) (B : Type)
  | MkProd : A -> B -> Prod A B
end

def first {A B : Type} (p : Prod A B) : A :=
  Prod.rec (fun (_ : Prod A B) => A) (fun (a : A) (_ : B) => a) p
end

def second {A B : Type} (p : Prod A B) : B :=
  Prod.rec (fun (_ : Prod A B) => B) (fun (_ : A) (b : B) => b) p
end
