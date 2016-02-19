module Nat

import Product

inductive Unit
  | Star : Unit
end

inductive Nat
  | Z : Nat
  | S : Nat -> Nat
end

def add (n : Nat) (m : Nat) : Nat :=
    Nat.rec _ m (fun (_ : Nat) (pN : Nat) => S pN) n
end

def below (C : (Nat -> Type)) : Nat -> Type :=
  fun (n : Nat) =>
    Nat.rec
    _
    Star
    (fun (m : Nat) (rest : Type) => MkProd (C m) rest)
    n
end

def main : Nat :=
  (fun (A : Type) => A) Z
end
