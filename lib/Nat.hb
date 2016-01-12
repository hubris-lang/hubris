module Nat

data Nat
  | Z : Nat
  | S : Nat -> Nat
end

fn add (n : Nat) (m : Nat) : Nat :=
  Nat.rec (fun (x : Nat) => Nat)
    m (fun (e : Nat) (sum : Nat) => S sum) n
end
