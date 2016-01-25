module Nat

data Nat
  | Z : Nat
  | S : Nat -> Nat
end

fn add : Nat -> Nat -> Nat :=
  fun (n : Nat) (m : Nat) : Nat =>
    Nat_rec
    (fun (c : Nat) : Type => Nat)
    m
    (fun (nprime : Nat) (pN : Nat) : Nat => S pN)
    n
end

fn main : Nat :=
  add (S (S Z)) (S (S Z))
end
