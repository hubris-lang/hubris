module Nat

inductive Nat
  | Z : Nat
  | S : Nat -> Nat
end

def add (n : Nat) (m : Nat) : Nat :=
    Nat.rec _ m (fun (_ : Nat) (pN : Nat) => S pN) n
end

def main : Nat :=
  Z
end
