module Nat

data Nat
  | Z : Nat
  | S : Nat -> Nat
end

fn add (n : Nat) (m : Nat) : Nat :=
  match n with
  | Z => m
  | S nn => add nn m
  end
end

fn nat_ind (n : Nat)
           (P : Nat -> Type)
           (proofZ : P Z)
           (proofS : forall (m : Nat), (P m -> P (S m))) : P n :=
  match n with
    | Z => proofZ
    | S np => proofZ
  end
end

data Eq : forall (A : Type), A -> A -> Type
  | Refl : forall (x : A), Eq x x
end
