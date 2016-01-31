module Eq

inductive Eq : forall (A : Type), A -> A -> Type
  | Refl : forall (A : Type), forall (x : A), Eq A x x
end
