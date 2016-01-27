module Nat

inductive Nat
  | Z : Nat
  | S : Nat -> Nat
end

inductive Eq : forall (A : Type), A -> A -> Type
  | Refl : forall (A : Type), forall (x : A), Eq A x x
end

def add (n : Nat) (m : Nat) : Nat :=
    Nat.rec
    (fun (c : Nat) : Type => Nat)
    m
    (fun (nprime : Nat) (pN : Nat) : Nat => S pN)
    n
end

def main : Nat :=
  add (S (S Z)) (S (S Z))
end
