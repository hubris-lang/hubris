module Eq

inductive Eq (A : Type) : A -> A -> Type
  | Refl : forall (x : A), Eq A x x
end
