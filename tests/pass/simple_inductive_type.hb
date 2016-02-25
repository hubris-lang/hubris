module SIT

inductive Nat
  | Z : Nat
  | S : Nat -> Nat
end

def main : Nat := Z end
