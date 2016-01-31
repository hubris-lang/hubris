module Nat

import Eq

inductive Nat
  | Z : Nat
  | S : Nat -> Nat
end

def add (n : Nat) (m : Nat) : Nat :=
    Nat.rec
    (fun (c : Nat) : Type => Nat)
    m
    (fun (nprime : Nat) (pN : Nat) : Nat => S pN)
    n
end

def main : Nat :=
  Z
end
