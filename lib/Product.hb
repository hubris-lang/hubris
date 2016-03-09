module Product

import Unit

inductive Prod (A : Type) (B : Type)
  | MkProd : A -> B -> Prod A B
end

def first {A B : Type} (p : Prod A B) : A :=
  match p with
  | Prod a _ => a
  end
end

def second {A B : Type} (p : Prod A B) : B :=
  match p with
  | Prod _ b => b
  end
end
