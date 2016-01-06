module Nat

data Nat
  | Z : Nat
  | S : Nat -> Nat
end

fn nat_ind (n : Nat)
           (P : Nat -> Type)
           (proofZ : P Z)
           (proofS : forall (m : Nat), (P m -> P (S m))) : P n :=
  match n with
    | Z => proofZ
    | S np => proofS np (nat_ind np P proofZ proofS)
  end
end
