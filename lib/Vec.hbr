module Vec

import Nat

inductive Vec (A : Type) : Nat -> Type
  | Nil : Vec A Z
  | Cons : forall (n : Nat), A -> Vec A n -> Vec A (S n)
end
