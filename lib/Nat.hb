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
