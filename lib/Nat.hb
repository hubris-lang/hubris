module Nat

inductive Nat
  | Z : Nat
  | S : Nat -> Nat
end

def add (n : Nat) (m : Nat) : Nat :=
  match n with
  | Z => m
  | S np => add np m
  end
end

def main : Nat :=
  let x := Z in add x x
end
